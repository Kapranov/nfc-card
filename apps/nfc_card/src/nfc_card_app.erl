%%%-------------------------------------------------------------------
%% @doc nfc_card public API
%% @end
%%%-------------------------------------------------------------------

-module(nfc_card_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    nfc_card_sup:start_link().

stop(_State) ->
    ok.
