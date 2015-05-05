all: escript | erl.mk

erl.mk:
	curl -fsSLo $@ 'https://raw.github.com/fenollp/erl-mk/master/erl.mk' || rm $@

dep_erlydtl = https//github.com/erlydtl/erlydtl master

ERLCFLAGS += +debug_info

-include erl.mk

# Your targets after this line.

clean: clean-ebin

distclean: clean clean-escript clean-deps
	$(if $(wildcard docs/), rm -rf docs/ )
.PHONY: distclean clean
