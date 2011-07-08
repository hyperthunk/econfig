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

-spec write_globals/2 :: 
        (BaseConfig::list(proplists:property()), 
         Opts::list(proplists:property())) -> list(proplists:property()).
write_globals(BaseConfig, Opts) ->
    Keys = lists:umerge(proplists:get_keys(BaseConfig), proplists:get_keys(Opts)),
    lists:foldl(rewrite_opts(BaseConfig), Opts, Keys).

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

