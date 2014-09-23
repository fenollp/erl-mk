all: erl.mk

erl.mk:
	curl -fsSLo $@ 'https://raw.github.com/fenollp/erl-mk/master/erl.mk' || rm $@

dep_lager = https://github.com/basho/lager.git 2.0.3

include erl.mk

# Your targets after this line.

distclean: clean clean-docs
	$(if $(wildcard deps/ ), rm -rf deps/)
.PHONY: distclean

debug: ERLCFLAGS += +export_all +debug_info
debug: all
	erl -pa ebin/ -pa deps/*/ebin/ -eval 'c:l($(APP)).'
