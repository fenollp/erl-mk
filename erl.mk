all: deps app
.PHONY: all

#### DEPS -- Fetches & compiles deps recursively then moves every dep to deps/

deps : $(patsubst %,deps/%/,$(DEPS))
	$(if $(wildcard deps/*/deps/), \
	    mv -v deps/*/deps/* deps/ && rmdir $(wildcard deps/*/deps/))
.PHONY: deps

deps/%/:
	$(if $(wildcard deps/),,mkdir deps/)
	git clone -n -- $(word 1,$(dep_$*)) $@
	cd $@ && git checkout -q $(word 2,$(dep_$*)) && cd ../..
	$(if $(wildcard $@/Makefile), \
	    make -C $@ all, \
	    cd $@ && rebar get-deps compile && cd ../..)

#### APP -- Compiles src/ into ebin/

app: $(patsubst src/%.app.src,ebin/%.app, $(wildcard src/*.app.src)) \
     $(patsubst src/%.erl,    ebin/%.beam,$(wildcard src/*.erl    )) \
     $(patsubst src/%.xrl,    ebin/%.beam,$(wildcard src/*.xrl    )) \
     $(patsubst src/%.yrl,    ebin/%.beam,$(wildcard src/*.yrl    )) \
     $(patsubst src/%.S,      ebin/%.beam,$(wildcard src/*.S      )) \
     $(patsubst src/%.core,   ebin/%.beam,$(wildcard src/*.core   ))
#	echo $?
.PHONY: app

ebin/%.beam: src/%.erl      | ebin/
	erlc -o ebin/ $(ERLCFLAGS) -v -I include/ -I deps/ $<

ebin/%.beam: src/%.xrl      | ebin/
	erlc -o ebin/ $<
	erlc -o ebin/ ebin/$*.erl

ebin/%.beam: src/%.yrl      | ebin/
	erlc -o ebin/ $<
	erlc -o ebin/ ebin/$*.erl

ebin/%.beam: src/%.S        | ebin/
	erlc -o ebin/ $(ERLCFLAGS) -v +from_asm -I include/ -I deps/ $<

ebin/%.beam: src/%.core     | ebin/
	erlc -o ebin/ $(ERLCFLAGS) -v +from_core -I include/ -I deps/ $<

ebin/%.app: src/%.app.src   | ebin/
	cp $< $@

ebin/:
	mkdir ebin/

#### EUNIT -- Compiles test/ into ebin/ and runs EUnit tests.

eunit: all
.PHONY: eunit

#### CLEAN -- Removes ebin/

clean:
	$(if $(wildcard ebin/),rm -r ebin/)
.PHONY: clean

#### DISTCLEAN -- Removes ebin/ & deps/

distclean: clean
	$(if $(wildcard deps/),rm -rf deps/)
.PHONY: distclean
