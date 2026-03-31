#!/bin/env sh

exec erl \
  -pa _build/default/lib/wahiawa/ebin \
  -pa _build/default/lib/*/ebin \
  -pa ebin -eval "application:ensure_all_started(wahiawa)." \
  -sname wahiawa_dev \
  -s wahiawa \
  -s reloader
