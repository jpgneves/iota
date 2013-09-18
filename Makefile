.PHONY: test eunit ct clean

all: compile

compile: get-deps
	./rebar compile

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
	rm -rf iota ebin .eunit deps
