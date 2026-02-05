#!/bin/env sh

exec erl \
    -pa _build/default/lib/server/ebin \
    -pa _build/default/lib/mochiweb/ebin \
    -pa ebin deps/*/ebin \
    -sname server_dev \
    -s server \
    -s reloader
