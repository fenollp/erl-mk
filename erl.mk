all: deps app
.PHONY: all

#### DEPS

get-deps : $(addprefix deps/,$(DEPS))
.PHONY: get-deps

deps/%/: | deps-dir
	git clone -n -- $(word 1,$(dep_$*)) $@
	cd $@ && git checkout -q $(word 2,$(dep_$*))
	$(if $(wildcard $@/Makefile), \
            make -C $@ get-deps, \
            cd $@ && rebar get-deps)

deps-dir: # Weird: Could not name target 'deps/' b/c of other target 'deps':
          #   ‘warning: overriding recipe for target `xxx'’
          #   ‘warning: ignoring old recipe for target `xxx'’
	$(if $(wildcard deps/),,mkdir deps/) # <> mkdir -p deps/

# Rewrite using 'deps/%/ebin/' when empty dir
clean-deps:
	$(foreach dep,$(wildcard deps/*/),make -C $(dep) clean;)
.PHONY: clean-deps

# Rewrite using dependency on 'deps/%/ebin/%.beam'
deps: get-deps
	$(foreach dep,$(wildcard deps/*/), \
            $(if $(wildcard $(dep)/Makefile), \
                make -C $(dep) all, \
                cd $(dep) && rebar compile); )
.PHONY: deps

update-deps: get-deps
	$(foreach dep,$(patsubst deps/%/,%,$(wildcard deps/*/)), \
                                              cd deps/$(dep)/ && \
                                             git fetch origin && \
                                      git fetch --tags origin && \
                         git checkout -q $(word 2,$(dep_$(dep)));)
.PHONY: update-deps

#### APP

ebin/%.beam: src/%.erl      | ebin/
	erlc $(ERLCFLAGS) -v -o ebin/ -pa ebin/ -pa deps/*/ebin/ -I include/ -I deps/ $<

ebin/%.beam: src/%.xrl      | ebin/
	erlc -o ebin/ $<
	erlc -o ebin/ ebin/$*.erl

ebin/%.beam: src/%.yrl      | ebin/
	erlc -o ebin/ $<
	erlc -o ebin/ ebin/$*.erl

ebin/%.beam: src/%.S        | ebin/
	erlc $(ERLCFLAGS) +from_asm -v -o ebin/ -pa ebin/ -pa deps/*/ebin/ -I include/ $<

ebin/%.beam: src/%.core     | ebin/
	erlc $(ERLCFLAGS) +from_core -v -o ebin/ -pa ebin/ -pa deps/*/ebin/ -I include/ $<

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
	rm -rf ebin/
.PHONY: clean

#### DISTCLEAN

distclean:
	rm -rf ebin/ deps/
.PHONY: distclean
