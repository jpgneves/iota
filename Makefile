.PHONY: test eunit ct clean

all: escriptize

compile:
	./rebar compile

escriptize: compile
	./rebar escriptize


test: eunit ct

eunit: compile
	./rebar eunit skip_deps=true

ct: compile
	./rebar ct skip_deps=true

clean:
	rm -rf iota ebin .eunit
