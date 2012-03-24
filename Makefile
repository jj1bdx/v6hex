# GNU Make dependent

.PHONY: compile clean doc eunit

REBAR=$(shell sh -c "PATH='$(PATH)':support which rebar||support/getrebar||echo false")

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

doc:
	$(REBAR) doc

eunit:
	$(REBAR) eunit

