all: deps app
.PHONY: all

### DEPS -- Fetches & compiles deps recursively then moves every dep to deps/

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

### APP -- Compiles src/ into ebin/

app: $(patsubst src/%.app.src,ebin/%.app, $(wildcard src/*.app.src)) \
     $(patsubst src/%.erl,    ebin/%.beam,$(wildcard src/*.erl    )) \
     $(patsubst src/%.xrl,    ebin/%.beam,$(wildcard src/*.xrl    )) \
     $(patsubst src/%.yrl,    ebin/%.beam,$(wildcard src/*.yrl    )) \
     $(patsubst src/%.S,      ebin/%.beam,$(wildcard src/*.S      )) \
     $(patsubst src/%.core,   ebin/%.beam,$(wildcard src/*.core   ))
.PHONY: app

ebin/%.beam: src/%.erl      | ebin/
	erlc -o ebin/ $(ERLCFLAGS) -v -Iinclude/ -Ideps/ $<

ebin/%.beam: src/%.xrl      | ebin/
	erlc -o ebin/ $<
	erlc -o ebin/ ebin/$*.erl

ebin/%.beam: src/%.yrl      | ebin/
	erlc -o ebin/ $<
	erlc -o ebin/ ebin/$*.erl

ebin/%.beam: src/%.S        | ebin/
	erlc -o ebin/ $(ERLCFLAGS) -v +from_asm -Iinclude/ -Ideps/ $<

ebin/%.beam: src/%.core     | ebin/
	erlc -o ebin/ $(ERLCFLAGS) -v +from_core -Iinclude/ -Ideps/ $<

ebin/%.app: src/%.app.src   | ebin/
	cp $< $@

ebin/:
	mkdir ebin/

### EUNIT -- Compiles test/ into ebin/ and runs EUnit tests.

eunit: $(patsubst test/%.erl, ebin/%.beam, $(wildcard test/*_tests.erl))
	@$(foreach m, \
            $(patsubst test/%_tests.erl,%_tests,$(wildcard test/*_tests.erl)),\
	    erl -noshell \
	        -eval 'io:format("Module $(m):\n"), eunit:test($(m)).' \
	        -pa ebin/ -pa deps/*/ebin/ -s init stop;)
.PHONY: eunit

ebin/%_tests.beam: test/%_tests.erl     | all
	erlc -o ebin/ -DTEST=1 -DEUNIT $(ERLCFLAGS) -v -Iinclude/ -Ideps/ $<

### DOCS -- Compiles the app's documentation into doc/

docs: $(foreach ext,app.src erl xrl yrl S core, $(wildcard src/*.$(ext))) \
                                                $(wildcard doc/overview.edoc)
	$(eval $@_APP = $(patsubst src/%.app.src,%,$(wildcard src/*.app.src)))
	@erl -noshell \
             -eval 'io:format("Compiling documentation for $($@_APP).\n").' \
             -eval 'edoc:application($($@_APP), ".", [$(EDOC_OPTS)]).' \
             -s init stop
.PHONY: docs

### CLEAN-DOCS -- Removes generated doc/ stuff.

clean-docs:
	$(if $(wildcard doc/*.css),     rm doc/*.css)
	$(if $(wildcard doc/*.html),    rm doc/*.html)
	$(if $(wildcard doc/*.png),     rm doc/*.png)
	$(if $(wildcard doc/edoc-info), rm doc/edoc-info)
	$(if $(wildcard doc/*),,$(if $(wildcard doc/),rmdir doc/))
.PHONY: clean-docs

### CLEAN -- Removes ebin/

clean:
	$(if $(wildcard ebin/),rm -r ebin/)
.PHONY: clean

### DISTCLEAN -- Removes ebin/ & deps/

distclean: clean clean-docs
	$(if $(wildcard deps/),rm -rf deps/)
.PHONY: distclean
