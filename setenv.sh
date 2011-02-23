#!/usr/bin/env sh

# set up the ERL_LIBS environment variable so that this project can be built from the top level 
# as a cohesive whole

export PWD=`pwd`
export DEPS_DIR=`cat rebar.config | grep deps_dir | awk ' /.*/ { gsub("\\\[\"", ""); gsub("\"\\]\\}.", ""); print $2; }'`
export ERL_LIBS="$PWD:$PWD/$DEPS_DIR:$ERL_LIBS"
