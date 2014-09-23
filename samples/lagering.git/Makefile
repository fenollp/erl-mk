all: erl.mk
	$(MAKE) $(APP)

erl.mk:
	curl -fsSLo $@ 'https://raw.github.com/fenollp/erl-mk/master/erl.mk' || rm $@

dep_lager = https://github.com/basho/lager.git 2.0.3

-include erl.mk
# Your targets after this line.

.PHONY: clean distclean debug

distclean: clean clean-deps
	$(if $(wildcard log/*), rm -r log/)
clean: clean-ebin

debug: debug-app
