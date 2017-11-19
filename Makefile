.PHONY: test

shell:
	rebar3 shell

test:
	blockade -d .blockade/ daemon& rebar3 ct --cover && \
	killall blockade
