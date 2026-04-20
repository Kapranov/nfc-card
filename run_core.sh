#!/bin/env sh

exec erl \
  -pa _build/default/lib/core/ebin \
  -pa _build/default/lib/*/ebin \
  -pa ebin -eval "application:ensure_all_started(core)." \
  -pa ebin -eval "sync:go()" \
  -config config/sys.config \
  -sname core_dev \
  -s core_app \
  -s reloader
