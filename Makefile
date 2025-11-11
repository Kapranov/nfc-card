REBAR=rebar3

.PHONY: get-deps

all:
		@$(REBAR) get-deps
		@rm -rf _build
		@echo "Copying nfc_card to deps/"
		@$(REBAR) compile

clean:
		@$(REBAR) clean

get-deps:
		@$(REBAR) get-deps

test:
		@$(REBAR) skip_deps=true eunit

run:
		@echo "nfc_card - compile and run in shell ..."
		@$(REBAR) compile
		@$(REBAR) shell

run-consumer:
		@erl -pa ./ebin ./deps/amqp_client/ebin ./deps/rabbit_common/ebin -run platform_message_consumer start -noshell
