#!/bin/env sh

exec erl \
  -pa _build/default/lib/waialua/ebin \
  -pa _build/default/lib/brod/ebin \
  -pa _build/default/lib/kafka_protocol/ebin \
  -pa _build/default/lib/*/ebin \
  -pa ebin -eval "application:ensure_all_started(waialua)." \
  -pa ebin -eval "sync:go()" \
  -config config/sys.config \
  -sname waialua_dev \
  -s waialua \
  -s reloader
