#all: $(APP) escript # For non-parallel usage
all: erl.mk
	$(MAKE) $(APP)
	$(MAKE) escript

erl.mk:
	curl -fsSLo $@ 'https://raw.github.com/fenollp/erl-mk/master/erl.mk' || rm $@

dep_erlydtl = git://github.com/erlydtl/erlydtl.git master

ERLCFLAGS += +debug_info

include erl.mk

# Your targets after this line.

distclean: clean-ebin clean-docs clean-escript clean-deps
	$(if $(wildcard docs/), rm -rf docs/ )
.PHONY: distclean
