#REBAR=`which rebar || ./rebar`
REBAR=./rebar
.PHONY: all compile clean eunit test eqc doc check dialyzer deps cleandeps tags

DIRS=src 

#all: compile eunit doc
all: compile 

check: compile dialyzer

deps:
	$(REBAR) get-deps

compile: deps
	$(REBAR) compile

clean:
	$(REBAR) clean
	-rm log/*

cleandeps:
	$(REBAR) delete-deps

eunit:
	$(REBAR) skip_deps=true eunit

test: eunit

doc:
	$(REBAR) doc

dialyzer: compile
	$(REBAR) skip_deps=true dialyze

tags:
	erl -s tags subdir "./" -s init stop -noshell

