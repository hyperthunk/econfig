Erlang/OTP Common Configuration Handling

Why
===

Because every Erlang/OTP app I come across seems to have a module named
<app>_config which invariably pulls configuration data from disk (or elsewhere)
and then stores and exposes it in/from one of the following....

* a gen_server process
* application:get_env/set_env
* (d)ets/mnesia

How
===

If you're using Jacob Vorreuter's epm, then it's as simple as hitting the shell
with `$ epm install hyperthunk/econfig` and you're off. If you want to use econfig
in your application and you're building using rebar then a dependency in your
rebar.config will do the trick nicely:
  
  {deps, [{econfig, ".*",
    {git, "http://github.com/hyperthunk/econfig.git", "master"}}]}.

If you're still in Makefile land or simply want to install econfig for use across
a number of apps, you can build it from sources:

  $ git clone git://github.com/hyperthunk/econfig.git
  $ cd econfig
  $ rebar install target=$ERL_LIBS