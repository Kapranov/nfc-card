#!/bin/env sh

exec erl \
  -pa _build/default/lib/wakako/ebin \
  -pa _build/default/lib/mochiweb/ebin \
  -pa _build/default/lib/webmachine/ebin \
  -pa _build/default/lib/*/ebin \
  -pa ebin -eval "application:ensure_all_started(wakako)." \
  -pa ebin -eval "sync:go()" \
  -config config/sys.config \
  -sname wakako_dev \
  -s wakako \
  -s reloader
