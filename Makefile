all: escriptize

compile:
	./rebar compile

escriptize: compile
	./rebar escriptize

clean:
	rm -rf iota ebin/
