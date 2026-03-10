#!/bin/env sh

exec erl \
  -pa _build/default/lib/server/ebin \
  -pa _build/default/lib/mochiweb/ebin \
  -pa _build/default/lib/webmachine/ebin \
  -pa _build/default/lib/*/ebin \
  -pa ebin -eval "application:ensure_all_started(server)." \
  -config config/sys.config \
  -sname server_dev \
  -s server \
  -s reloader
