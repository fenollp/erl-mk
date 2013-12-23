all: erl.mk

erl.mk:
	wget -nv -O $@ 'https://raw.github.com/fenollp/erl-mk/master/erl.mk' || rm $@

DEPS = bullet
dep_bullet = https://github.com/extend/bullet.git 0.4.1

include erl.mk

# Your targets after this line.

distclean: clean clean-docs
	$(if $(wildcard deps/ ), rm -rf deps/)
	$(if $(wildcard logs/ ), rm -rf logs/)
	$(if $(wildcard erl.mk), rm erl.mk   )
.PHONY: distclean

debug: all
	erl -pa ebin/ -pa deps/*/ebin/ -eval 'c:l($(APP)).'

test: eunit
