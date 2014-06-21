all: erl.mk

erl.mk:
	curl -fsSLo $@ 'https://raw.github.com/fenollp/erl-mk/master/erl.mk' || rm $@

dep_sheriff = https://github.com/extend/sheriff.git 0.4.1

include erl.mk

# Your targets after this line.

distclean: clean clean-docs
	$(if $(wildcard deps/ ), rm -rf deps/)
	$(if $(wildcard logs/ ), rm -rf logs/)
	$(if $(wildcard erl.mk), rm erl.mk   )
.PHONY: distclean

debug: ERLCFLAGS += +export_all +debug_info
debug: all
	erl -pa ebin/ -pa deps/*/ebin/ -eval 'c:l($(APP)).'

test: eunit
