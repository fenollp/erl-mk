all: app  | erl.mk

erl.mk:
	curl -fsSLo $@ 'https://raw.github.com/fenollp/erl-mk/master/erl.mk' || rm $@

dep_sheriff = https://github.com/extend/sheriff 0.4.1

-include erl.mk
# Your targets after this line.

clean: clean-ebin
distclean: clean clean-deps

.PHONY: distclean clean debug test

debug: debug-app

test: eunit
