all: deps app
.PHONY: all

#### DEPS

deps : $(patsubst %,deps/%/,$(DEPS))
	$(if $(wildcard deps/*/deps/*),mv -v deps/*/deps/* deps/)
.PHONY: deps

deps/%/: | deps-dir
	$(if $(wildcard $@/.git/),, \
	    git clone -n -- $(word 1,$(dep_$*)) $@ && \
	        cd $@ && git checkout -q $(word 2,$(dep_$*)) && cd ../../)
	$(if $(wildcard $@/Makefile), \
	    make -C $@ all, \
	    cd $@ && rebar get-deps compile && cd ../../)
#.PHONY: deps/%/

deps-dir: # Weird: Could not name target 'deps/' b/c of other target 'deps':
          #   ‘warning: overriding recipe for target `xxx'’
          #   ‘warning: ignoring old recipe for target `xxx'’
	$(if $(wildcard deps/),,mkdir deps/)

clean-deps:
	$(foreach dep,$(wildcard deps/*/), \
            $(if $(wildcard $(dep)/Makefile), \
                make -C $(dep) clean;, \
                cd $(dep) && rebar clean && cd ../../;))
.PHONY: clean-deps

update-deps: deps
	$(foreach dep,$(patsubst deps/%/,%,$(wildcard deps/*/)), \
                                              cd deps/$(dep)/ && \
                                             git fetch origin && \
                                      git fetch --tags origin && \
                      git checkout -q $(word 2,$(dep_$(dep))) && \
                                                      cd ../../; )
# TODO: recompile
.PHONY: update-deps

#### APP

ERLC_INCLUDES = -I include/ -I deps/ -I ../../

ebin/%.beam: src/%.erl      | ebin/
	erlc $(ERLCFLAGS) -v -o ebin/ $(ERLC_INCLUDES) $<

ebin/%.beam: src/%.xrl      | ebin/
	erlc -o ebin/ $<
	erlc -o ebin/ ebin/$*.erl

ebin/%.beam: src/%.yrl      | ebin/
	erlc -o ebin/ $<
	erlc -o ebin/ ebin/$*.erl

ebin/%.beam: src/%.S        | ebin/
	erlc $(ERLCFLAGS) +from_asm -v -o ebin/ $(ERLC_INCLUDES) $<

ebin/%.beam: src/%.core     | ebin/
	erlc $(ERLCFLAGS) +from_core -v -o ebin/ $(ERLC_INCLUDES) $<

ebin/%.app: src/%.app.src   | ebin/
	cp $< $@

app: $(patsubst src/%.app.src,ebin/%.app, $(wildcard src/*.app.src)) \
     $(patsubst src/%.erl,    ebin/%.beam,$(wildcard src/*.erl    )) \
     $(patsubst src/%.xrl,    ebin/%.beam,$(wildcard src/*.xrl    )) \
     $(patsubst src/%.yrl,    ebin/%.beam,$(wildcard src/*.yrl    )) \
     $(patsubst src/%.S,      ebin/%.beam,$(wildcard src/*.S      )) \
     $(patsubst src/%.core,   ebin/%.beam,$(wildcard src/*.core   ))
#	echo $?
.PHONY: app

ebin/:
	mkdir ebin/

clean: clean-deps
	$(if $(wildcard ebin/),rm -r ebin/)
.PHONY: clean

#### DISTCLEAN

distclean:
	$(if $(wildcard deps/),rm -r deps/)
.PHONY: distclean
