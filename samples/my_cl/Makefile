all: erl.mk

erl.mk:
	wget -nv -O $@ 'https://raw.github.com/fenollp/erl-mk/master/erl.mk' || rm $@

include erl.mk
# Your targets after this line.

distclean: clean clean-deps clean-docs
clean: clean-ebin
	$(MAKE) -C examples $@

examples:
	$(MAKE) -C examples all

.PHONY: clean distclean examples
