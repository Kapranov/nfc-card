#!/bin/env bash

export ERL_CRASH_DUMP_SECONDS=1

erl \
  -name session1@axion.local \
  -pa ebin deps/*/ebin \
  -config config/sys.config \
  -eval 'lists:foreach(fun(App) -> ok = application:start(App) end, [])'
