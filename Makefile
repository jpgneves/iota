all: escriptize

compile:
	./rebar compile

escriptize: compile
	./rebar escriptize
