all: erl.mk

erl.mk:
	curl -sSo $@ 'https://raw.github.com/fenollp/erl-mk/master/erl.mk' || rm $@

dep_erlydtl = git://github.com/erlydtl/erlydtl.git master erlydtl erlydtl

include erl.mk

# Your targets after this line.

distclean: clean clean-docs
	$(if $(wildcard deps/ ), rm -rf deps/)
	$(if $(wildcard erldocs), rm erldocs )
	$(if $(wildcard docs/), rm -rf docs/ )
.PHONY: distclean

all: escript

clean: clean-ebin
