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

## Redefining: special options for ErlyDTL templates compilation.
ebin/%_dtl.beam: templates/%.dtl                | ebin/
	$(if $(shell [[ ! -d deps/erlydtl ]] && echo y), \
	    $(error Error compiling $<: deps/erlydtl/ not found))
	@erl -noshell -pa ebin/ -pa deps/*/ebin/ \
	     -eval 'io:format("Compiling ErlyDTL template: $< -> $@\n").' \
	     -eval 'erlydtl:compile("$<", $*_dtl, [{out_dir,"ebin/"},{auto_escape,false}]).' \
	     -s init stop
