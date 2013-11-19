all: ebin project
.PHONY: all

#ALL_DEPS_DIRS = $(addprefix deps/,$(DEPS))

####

#deps : $(ALL_DEPS_DIRS)

#clean-deps:
#	$(foreach dep,$(DEPS),+$(MAKE) clean -C $(dep);)

#deps/%/:
#	+$(MAKE) -C $@

####

ebin/%.beam: src/%.erl
	erlc $(ERL_OPTS) -v -o ebin/ -pa ebin/ -I include/ $<

ebin/%.beam: src/%.xrl
	erlc -o ebin/ $<
	erlc -o ebin/ ebin/$*.erl

ebin/%.beam: src/%.yrl
	erlc -o ebin/ $<
	erlc -o ebin/ ebin/$*.erl

ebin/%.app: src/%.app.src
	touch ebin/$*.app

APPSRC = $(patsubst src/%.app.src,ebin/%.app,$(wildcard src/*.app.src))
ERLS = $(patsubst src/%.erl,ebin/%.beam,$(wildcard src/*.erl))
XRLS = $(patsubst src/%.xrl,ebin/%.beam,$(wildcard src/*.xrl))
YRLS = $(patsubst src/%.yrl,ebin/%.beam,$(wildcard src/*.yrl))

project: $(APPSRC) $(ERLS) $(XRLS) $(YRLS)
	echo $?

ebin:
	mkdir ebin/

distclean:
	rm -rf ebin/ deps/
.PHONY: distclean
