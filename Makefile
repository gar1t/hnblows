rebar := ./rebar
config := dev
erl := erl

compile: deps
	$(rebar) compile

quick:
	$(rebar) compile skip_deps=true

deps:
	$(rebar) get-deps

refresh-deps: delete-deps deps
	$(rebar) get-deps

delete-deps:
	$(rebar) delete-deps

clean:
	./rebar clean

shell:
	$(erl) -pa ebin $(wildcard deps/*/ebin) \
               -config priv/$(config) \
               -s hnb \
	       +A 16 \
               +P 1048576
