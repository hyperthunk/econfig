%% -----------------------------------------------------------------------------
%%
%% econfig
%%
%% -----------------------------------------------------------------------------
%% Copyright (c) 2011 Tim Watson (watson.timothy@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -----------------------------------------------------------------------------
-module(econfig).
-export([write_globals/2]).
-export([eval/2]).
-export([read/2, read/3]).

read(termconf, Store) ->
    read(Store);
read(Backend, Store) ->
    {e_unsupported, {Backend, Store}}.

read(termconf, Store, Opts) ->
    read(Store, proplists:get_value(local_call_handler, Opts, none),
                proplists:get_value(remote_call_handler, Opts, none),
                proplists:get_value(element_handler, Opts, fun(X) -> X end));
read(Backend, Store, _Opts) ->
    read(Backend, Store).

%% @doc merges to proplists, apply the <i>global</i> configuration only if
%% it isn't already defined in the base proplists.
-spec write_globals/2 ::
        (BaseConfig::list(proplists:property()),
         Opts::list(proplists:property())) -> list(proplists:property()).
write_globals(BaseConfig, Opts) ->
    Keys = lists:umerge(proplists:get_keys(BaseConfig), proplists:get_keys(Opts)),
    lists:foldl(rewrite_opts(BaseConfig), Opts, Keys).

%%
%% Internal API
%%

eval([H|_]=S, Opts) when is_integer(H) ->
    case re:split(S, "(\\$)\\{([^\\}]*)\\}", [{return, list}, trim]) of
        [Data] ->
            Data;
        Parts when is_list(Parts) ->
            lists:flatten(lists:reverse(
                            lists:foldl(merge_opts(Opts), [], Parts)))
    end;
eval(S, _) ->
    S.

merge_opts(Opts) ->
    fun(E, [H|Acc]) when H == "$" ->
           [option(E, Opts)|Acc];
       (E, Acc) ->
           [E|Acc]
    end.

option(E, Opts) ->
    case kvc:path(E, Opts) of
        [] -> E;
        Other -> Other
    end.

resolve_variables(Data) when is_list(Data) ->
    Data2 = lists:reverse(Data),
    lists:foldl(fun({K, Val}, Acc) ->
                    lists:keyreplace(K, 1, Acc, {K, eval(Val, Acc)})
                end, Data2, Data2);
resolve_variables({K, [{_,_}|_]=V}) ->
    {K, resolve_variables(V)};
resolve_variables(Data) ->
    Data.

read(File) ->
    read(File, {eval, fun config_fn_handler/3},
               {value, fun ext_config_fn_handler/2},
               fun resolve_variables/1).

read(File, Lch, Rch, Transform) ->
    Bs = erl_eval:new_bindings(),
    {ok, Cwd} = file:get_cwd(),
    case file:path_open([Cwd], File, [read]) of
        {ok, Fd, _} ->
            try eval_stream(Fd, return, Bs, Lch, Rch) of
                {ok, R} ->
                    Transform(R);
                E1 ->
                    E1
            after
                file:close(Fd)
            end;
        E2 ->
            E2
    end.

config_fn_handler(resolve, [{atom, _, Key}], Bindings) ->
    {value, 
        kvc:path(list_to_atom("config." ++ atom_to_list(Key)), Bindings),
        Bindings}.

ext_config_fn_handler(Func, Args) when is_function(Func) ->
    Func(Args);
ext_config_fn_handler({Mod, Func}, Args) ->
    apply(Mod, Func, Args).

eval_stream(Fd, Handling, Bs, Lch, Rch) ->
    eval_stream(Fd, Handling, 1, [], [], Bs, Lch, Rch).

eval_stream(Fd, H, Line, Last, E, Bs, Lch, Rch) ->
    eval_stream2(io:parse_erl_exprs(Fd, '', Line),
                 Fd, H, Last, E, Bs, Lch, Rch).

eval_stream2({ok,Form,EndLine}, Fd, H, Last, E, Bs0, Lch, Rch) ->
    Bs1 = erl_eval:add_binding(config, Last, Bs0),
    try erl_eval:exprs(Form, Bs1, Lch, Rch) of
        {value, V, Bs2} ->
            eval_stream(Fd, H, EndLine, [V|Last], E, Bs2, Lch, Rch)
    catch Class:Reason ->
        Error = {EndLine,?MODULE,{Class,Reason,erlang:get_stacktrace()}},
        eval_stream(Fd, H, EndLine, Last, [Error|E], Bs0, Lch, Rch)
    end;
eval_stream2({error,What,EndLine}, Fd, H, Last, E, Bs, Lch, Rch) ->
    eval_stream(Fd, H, EndLine, Last, [What | E], Bs, Lch, Rch);
eval_stream2({eof, EndLine}, _Fd, H, Last, E, _Bs, _Lch, _Rch) ->
    case {H, Last, E} of
        {return, Val, []} ->
            {ok, Val};
        {return, undefined, E} ->
            {error, hd(lists:reverse(E, [{EndLine,?MODULE,undefined_script}]))};
        {ignore, _, []} ->
            ok;
        {_, _, [_|_] = E} ->
            {error, hd(lists:reverse(E))}
    end.

rewrite_opts(BaseConfig) ->
    fun(Key, Opts) ->
        Replacement = proplists:get_value(Key, BaseConfig, undefined),
        overwrite({Key, Replacement}, Opts)
    end.

overwrite({Key, [_|_]=Value}, Opts) ->
    case lists:keyfind(Key, 1, Opts) of
        false ->
            [{Key, Value}|Opts];
        {Key, Config} when is_list(Config) ->
            lists:keyreplace(Key, 1, Opts, {Key, Value ++ Config});
        _ ->
            lists:keyreplace(Key, 1, Opts, {Key, Value})
    end;
overwrite({_Key, []}, Opts) ->
    Opts;
overwrite({_Key, undefined}, Opts) ->
    Opts;
overwrite({Key, Value}, Opts) ->
    case lists:keyfind(Key, 1, Opts) of
        false ->
            [{Key, Value}|Opts];
        _ ->
            lists:keyreplace(Key, 1, Opts, {Key, Value})
    end.
