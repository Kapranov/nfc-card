REBAR=rebar3

.PHONY: get-deps

all:
		@$(REBAR) get-deps
		@rm -rf _build
		@echo "Copying nfc_card to deps/"
		@$(REBAR) compile

clean:
		@$(REBAR) clean

clean-all:
		@rm -rf _build
		@rm rebar.lock

compile:
		@$(REBAR) compile

console:
		@erl -pa _build/default/lib/*/ebin -s maui_server test_run

dialyzer:
		@$(REBAR) dialyzer

eunit:
		@$(REBAR) eunit

get-deps:
		@$(REBAR) get-deps

test:
		@$(REBAR) eunit --sys_config ./config/test.config

run:
		@echo "nfc_card - compile and run in shell ..."
		@$(REBAR) compile
		@$(REBAR) shell

run-consumer:
		@erl -pa ./ebin ./deps/amqp_client/ebin ./deps/rabbit_common/ebin -run platform_message_consumer start -noshell
