all: ebin project
.PHONY: all

####

ALL_DEPS_DIRS = $(addprefix deps/,$(DEPS))

get-deps : $(ALL_DEPS_DIRS)

deps/%/:
	git clone -n 
#	+$(MAKE) -C $@

#clean-deps:
#	$(foreach dep,$(DEPS),+$(MAKE) clean -C $(dep);)

####

ebin/%.beam: src/%.erl
	erlc $(ERL_OPTS) -v -o ebin/ -pa ebin/ -I include/ $<

ebin/%.beam: src/%.xrl
	erlc -o ebin/ $<
	erlc -o ebin/ ebin/$*.erl

ebin/%.beam: src/%.yrl
	erlc -o ebin/ $<
	erlc -o ebin/ ebin/$*.erl

ebin/%.beam: src/%.S
	erlc $(ERL_OPTS) -v +from_asm -o ebin/ -pa ebin/ -I include/ $<

ebin/%.beam: src/%.core
	erlc $(ERL_OPTS) -v +from_core -o ebin/ -pa ebin/ -I include/ $<

ebin/%.app: src/%.app.src
	cp $< $@

APPSRC = $(patsubst src/%.app.src,ebin/%.app,$(wildcard src/*.app.src))
ERLS = $(patsubst src/%.erl,ebin/%.beam,$(wildcard src/*.erl))
XRLS = $(patsubst src/%.xrl,ebin/%.beam,$(wildcard src/*.xrl))
YRLS = $(patsubst src/%.yrl,ebin/%.beam,$(wildcard src/*.yrl))
ASMS = $(patsubst src/%.S,ebin/%.beam,$(wildcard src/*.S))
CORES = $(patsubst src/%.core,ebin/%.beam,$(wildcard src/*.core))

project: $(APPSRC) $(ERLS) $(XRLS) $(YRLS) $(ASMS) $(CORES)
#	echo $?

ebin:
	mkdir ebin/

distclean:
	rm -rf ebin/ deps/
.PHONY: distclean
