Erlang/OTP Common Configuration Handling

What
====

* A consistent API to access application/system configuration data
* Looks like a simple key value store
* Modular config parsing support including code eval
* Modular support for disparate storage mechanisms 
* Easy to extend

Planned backend storage modules include

* gen_server process
* application env/props

Also aiming to _prove_ the extensibility by writing a backend for configuration
files written in embedded Lua (or javascript).

Why
===

1. Because I'd like to be able to embed erlang expressions in config files (of
various formats) and evaluate them on the fly.

2. Because every Erlang/OTP app I write or come across on github seems to have a
module named {appname}_config which invariably pulls configuration data from disk
(or elsewhere) and then stores and exposes it in/from one of the following....

* a gen_server process
* application:get_env/set_env
* (d)ets or a simple key value store

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
