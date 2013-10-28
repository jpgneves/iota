DIALYZER_PLT := .dialyzer.plt
PRODUCTION_ERLS := $(wildcard src/*.erl)
PRODUCTION_BEAMS := $(addprefix ebin/, $(notdir $(PRODUCTION_ERLS:.erl=.beam)))

.PHONY: test eunit ct clean dialyze check

all: compile

compile: get-deps
	./rebar compile

check: dialyze

dialyze: compile $(DIALYZER_PLT)
	DIALYZER_PLT=$(DIALYZER_PLT) \
        PRODUCTION_BEAMS="$(PRODUCTION_BEAMS)" ./dialyze.sh

$(DIALYZER_PLT):
	DIALYZER_PLT=$(DIALYZER_PLT) ./make-plt.sh

escriptize: compile
	./rebar escriptize skip_deps=true

get-deps:
	./rebar get-deps

test: eunit ct

eunit: compile
	./rebar eunit skip_deps=true

ct: compile
	./rebar ct skip_deps=true

clean:
	./rebar clean
	rm -rf iota .eunit
