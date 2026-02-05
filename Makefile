REBAR=rebar3

all:
		@$(REBAR) get-deps
		@rm -rf _build
		@echo "Copying nfc_card to deps/"
		@$(REBAR) compile

edoc:
	  @$(REBAR) edoc

clean:
		@$(REBAR) clean

remove:
		@rm -rf _build
		@rm rebar.lock

build:
	  @$(REBAR) get-deps
		@$(REBAR) compile

console:
		@erl -pa _build/default/lib/*/ebin -s maui_server test_run

dialyzer:
		@$(REBAR) dialyzer

eunit:
		@$(REBAR) eunit

deps:
		@$(REBAR) get-deps

test:
		@$(REBAR) eunit --sys_config ./config/test.config

run:
		@echo "nfc_card - compile and run in shell ..."
		@$(REBAR) compile
		@$(REBAR) shell

consumer:
		@erl -pa ./ebin ./deps/amqp_client/ebin ./deps/rabbit_common/ebin -run platform_message_consumer start -noshell

.PHONY: all build clean console consumer deps dialyzer edoc eunit remove run test
