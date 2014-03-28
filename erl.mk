.SECONDEXPANSION:

##------------------------------------------------------------------------------
## GENERAL
##------------------------------------------------------------------------------

SHELL = /bin/bash

APP = $(patsubst src/%.app.src,%,$(wildcard src/*.app.src))
APPS += $(notdir $(wildcard apps/*))
DEPS_DIR ?= $(addsuffix /deps, $(realpath .))
ERL_LIBS := $(DEPS_DIR):$(realpath apps):$(ERL_LIBS)

export DEPS_DIR
export ERL_LIBS

ifeq ($(APP), )

.DEFAULT_GOAL := apps

else

.DEFAULT_GOAL := app

endif

space :=
space +=
comma := ,

V ?= 0
verbose_0 = @echo -n;
verbose = $(verbose_$(V))

all: get-deps app

apps : $(APPS)
	@echo > /dev/null

$(APPS): erl.mk
	@if [ -f apps/$@/Makefile ] || [ -f apps/$@/makefile ] ; then \
		$(MAKE) -C apps/$@; \
	else \
		$(MAKE) -C apps/$@ -f ../../erl.mk; \
	fi

$(addsuffix .%, $(APPS)) : 
	$(MAKE) -C apps/$(word 1, $(subst ., , $@)) $*

##------------------------------------------------------------------------------
## COMPILE
##------------------------------------------------------------------------------
C_HEADERS = $(wildcard c_src/*.h)
C_OBJECTS = $(patsubst %.c, %.o, $(wildcard c_src/*.c))
UNAME := $(shell uname)

ifeq ($(UNAME), Darwin)
	ERL_DIR = $(shell erl -noshell -eval 'io:format("~s~n", [code:lib_dir(erl_interface)])' -eval 'init:stop()')
	CFLAGS += -I $(ERL_DIR)/include
	LDFLAGS += -L /usr/local/lib -L $(ERL_DIR)/lib -lei
	TARGET = priv/video_capture
	MAKE_TARGET = ar rcs $(TARGET) $(OBJECTS)
endif
ifeq ($(UNAME), Linux)
	ERL_DIR = $(shell erl -noshell -eval 'io:format("~s~n", [code:lib_dir(erl_interface)])' -eval 'init:stop()')
	CFLAGS += -I $(ERL_DIR)/include
	LDFLAGS += -L /usr/local/lib -L $(ERL_DIR)/lib -lei
	TARGET = priv/video_capture
	MAKE_TARGET = ar rcs $(TARGET) $(OBJECTS)
endif

MODULES = $(notdir $(patsubst src/%.erl, %, $(wildcard src/*.erl) $(wildcard src/*/*.erl)))
MODULES_LIST = $(subst $(space),$(comma),$(strip $(MODULES)))
ERL_BEAMS = $(addprefix ebin/, $(addsuffix .beam, $(MODULES)))

OTHER_BEAMS = $(foreach ext, xrl yrl S core, $(addprefix ebin/, $(notdir $(patsubst src/%.$(ext), %.beam, $(wildcard src/*.$(ext)) $(wildcard src/*/*.$(ext))))))

DTS = $(patsubst templates/%.dtl, ebin/%_dtl.beam, $(wildcard templates/*.dtl))

DEPENDENCIES = $(patsubst %.beam, %.d, $(ERL_BEAMS))

app: get-deps ebin/$(APP).app $(ERL_BEAMS) $(OTHER_BEAMS) $(C_TARGET_NAME) $(DTL) | ebin/
	@echo > /dev/null

$(C_TARGET_NAME) : $(C_OBJECTS)
	@if [ $(C_TARGET) == "static_library" ] ; then \
		echo Creating archive $(C_TARGET_NAME) ; \
		ar rcs $(C_TARGET_NAME) $(C_OBJECTS) ; \
	else \
		if [ $(C_TARGET) == "executable" ]; then \
			echo Creating executable $(C_TARGET_NAME) ; \
			$(CC) $(C_OBJECTS) $(LDFLAGS) -o $(C_TARGET_NAME) ; \
		fi \
	fi

c_src/%.o: c_src/%.c $(C_HEADERS)
	$(CC) $(CFLAGS) -c $< -o $@

ebin/%.app: src/%.app.src                       | ebin/
	@erl -noshell \
	     -eval 'case file:consult("$<") of {ok,_}->ok; {error,{_,_,M}}->io:format("$<: ~s~s\n",M),halt(1) end.' \
	     -s init stop
	@sed 's/{modules,[[:space:]]*\[\]}/{modules, \[$(MODULES_LIST)\]}/' < $< > $@

ebin/%.d: $$(wildcard src/%.erl) $$(wildcard src/*/%.erl)   | ebin/
	$(verbose) erlc -o ebin/ $(ERLCFLAGS) -Iinclude/ -I$(DEPS_DIR)/ -MP -MG -MF $@ $<
	@gawk '/^[ \t]*-(behaviou?r\(|compile\({parse_transform,)/ {match($$0, /-(behaviou?r\([ \t]*([^) \t]+)|compile\({parse_transform,[ \t]*([^} \t]+))/, a); m = (a[2] a[3]); if (m != "" && ((system("ls src/" m ".erl 1>/dev/null 2>/dev/null") == 0) || (system("ls src/*/" m ".erl 1>/dev/null 2>/dev/null") == 0))) print "\nebin/$*.beam: ebin/" m ".beam"}' < $< >> $@


ebin/%.beam: $$(wildcard src/%.erl) $$(wildcard src/*/%.erl)   | ebin/
	@echo $<
	$(verbose) erlc -pa ebin/ -o ebin/ $(ERLCFLAGS) -v -Iinclude/ -I$(DEPS_DIR)/ $<

ebin/%.beam: src/%.xrl $(wildcard include/*)    | ebin/
	erlc -o ebin/ $(ERLCFLAGS) $<
	erlc -o ebin/ ebin/$*.erl

ebin/%.beam: src/%.yrl $(wildcard include/*)    | ebin/
	erlc -o ebin/ $(ERLCFLAGS) $<
	erlc -o ebin/ ebin/$*.erl

ebin/%.beam: src/%.S $(wildcard include/*)      | ebin/
	erlc -o ebin/ $(ERLCFLAGS) -v +from_asm -Iinclude/ -I$(DEPS_DIR)/ $<

ebin/%.beam: src/%.core $(wildcard include/*)   | ebin/
	erlc -o ebin/ $(ERLCFLAGS) -v +from_core -Iinclude/ -I$(DEPS_DIR)/ $<

ebin/%_dtl.beam: templates/%.dtl                | ebin/
	$(if $(shell [[ ! -d $(DEPS_DIR)/erlydtl ]] && echo y), \
	    $(error Error compiling $<: $(DEPS_DIR)/erlydtl/ not found))
	@erl -noshell -pa ebin/ -pa $(DEPS_DIR)/*/ebin/ \
	     -eval 'io:format("Compiling ErlyDTL template $<\n").' \
	     -eval 'erlydtl:compile("$<", $*_dtl, [{out_dir,"ebin/"}]).' \
	     -s init stop

ebin/:
	mkdir ebin/

-include $(DEPENDENCIES)

.PHONY: app 

##------------------------------------------------------------------------------
## EUNIT 
##------------------------------------------------------------------------------
ifeq ($(APP), )

eunit: $(addsuffix .eunit, $(APPS))
	@echo > /dev/null
else

TESTS = $(foreach ext, erl, $(addprefix eunit., $(notdir $(patsubst src/%.$(ext), %, $(wildcard src/*.$(ext)) $(wildcard src/*/*.$(ext))))))

eunit_start_message:
	@echo ------------------------------------------------------------
	@echo eunit tests starting for $(APP)
	@echo ------------------------------------------------------------
	@echo

eunit: eunit_start_message $(TESTS)
	@echo ------------------------------------------------------------
	@echo eunit tests completed for $(APP)
	@echo ------------------------------------------------------------
endif

eunit.%: app ebin/%.beam
	@erl -noshell \
	     -pa ebin/ -pa $(DEPS_DIR)/*/ebin/ \
	     -eval 'eunit:test($*, [verbose]).' \
	     -s init stop

#test/%_tests.beam: test/%_tests.erl
#	@erlc -o test/ -DTEST=1 -DEUNIT $(ERLCFLAGS) -v -Iinclude/ -I$(DEPS_DIR)/ $<

.PHONY: eunit eunit.% eunit_start_message

##------------------------------------------------------------------------------
## CT -- Compiles (into ebin/) & run Common Test tests (test/*_SUITE.erl).
##------------------------------------------------------------------------------
ifeq ($(APP), )

ct: $(addsuffix .ct, $(APPS))
	@echo > /dev/null

else

ct_start_message:
	@echo ------------------------------------------------------------
	@echo ct tests starting for $(APP)
	@echo ------------------------------------------------------------
	@echo

ct: ct_start_message $(patsubst test/%_SUITE.erl, ct.%, $(wildcard test/*_SUITE.erl))
	@echo ------------------------------------------------------------
	@echo ct tests completed for $(APP)
	@echo ------------------------------------------------------------
endif 

ct.%: app test/%_SUITE.beam                 | logs/
	@ct_run -noshell -dir test/ -logdir logs/ \
		-no_auto_compile \
	        -pa ebin/ -pa $(wildcard $(shell pwd)/deps/*/ebin/) \
	        -suite $*_SUITE || true

test/%_SUITE.beam: test/%_SUITE.erl
	@erlc -o test/ $(ERLCFLAGS) -v -Iinclude/ -I$(DEPS_DIR)/ $<

.PRECIOUS: test/%_SUITE.beam

logs/:
	mkdir logs/

.PHONY: ct ct.%

##------------------------------------------------------------------------------
## ESCRIPT -- Create a stand-alone EScript executable.
##------------------------------------------------------------------------------

escript: | all
	@erl -noshell \
	     -eval 'io:format("Compiling escript \"./$(APP)\".\n").' \
	     -eval 'escript:create("$(APP)", [ {shebang,default}, {comment,""}, {emu_args,"-escript $(APP)"}, {archive, [{case F of "ebin/"++E -> E; "$(DEPS_DIR)/"++D -> D end, element(2,file:read_file(F))} || F <- filelib:wildcard("ebin/*") ++ filelib:wildcard("$(DEPS_DIR)/*/ebin/*")], []}]).' \
	     -eval '{ok, Mode8} = file:read_file_info("$(APP)"), ok = file:change_mode("$(APP)", element(8,Mode8) bor 8#00100).' \
	     -s init stop

##------------------------------------------------------------------------------
## DOCS -- Compiles the app's documentation into doc/
##------------------------------------------------------------------------------
docs: $(foreach ext,app.src erl xrl yrl S core, $(wildcard src/*.$(ext))) \
                                                $(wildcard doc/overview.edoc)
	@erl -noshell \
	     -eval 'io:format("Compiling documentation for $(APP).\n").' \
	     -eval 'edoc:application($(APP), ".", [$(EDOC_OPTS)]).' \
	     -s init stop
.PHONY: docs

##------------------------------------------------------------------------------
## DEPENDENCIES
##------------------------------------------------------------------------------

DEPS_DIR ?= $(CURDIR)/deps
export DEPS_DIR

FULL_DEPS = $(addsuffix /, $(addprefix $(DEPS_DIR)/, $(DEPS)))

REBAR_DEPS_DIR = $(DEPS_DIR)
export REBAR_DEPS_DIR
GLOBAL_REBAR = $(shell which rebar || echo "./rebar")

define get_dep
	@if [[ ! -d "$(DEPS_DIR)/$(1)" ]] ; then \
		echo Cloning $(1) / $(3) from $(2) ; \
		git clone -n -- $(2) $(DEPS_DIR)/$(1) ; \
		cd $(DEPS_DIR)/$(1); git checkout -q $(3) ; \
	else \
		echo Already cloned $(1) / $(3) from $(2) ; \
	fi
endef

define build_dep
	@if [[ -f $(DEPS_DIR)/$(1)/rebar.config ]] ; then \
		if [[ -f $(DEPS_DIR)/$(1)/rebar ]]; then \
			export THIS_REBAR="./rebar"; \
		else \
			if [[ -f $(GLOBAL_REBAR) ]]; then \
				export THIS_REBAR=$(GLOBAL_REBAR) ; \
			else \
				echo $(1) has rebar.config but no rebar - pulling form github...; \
				wget -nv -O $(DEPS_DIR)/$(1)/rebar 'https://raw.github.com/id3as/erl-mk/master/rebar'; \
				chmod 744 $(DEPS_DIR)/$(1)/rebar ; \
				export THIS_REBAR="./rebar" ; \
			fi \
		fi \
	fi ; \
	echo BUILD $(1) - $(ERL_LIBS), $(DEPS_DIR) ; \
	if [[ -f $(DEPS_DIR)/$(1)/makefile ]] || [[ -f $(DEPS_DIR)/$(1)/Makefile ]] ; then \
		echo 'make -C $(DEPS_DIR)/$(1)' ; \
	       	make -C $(DEPS_DIR)/$(1)  ; \
	else \
		if [[ -f $(DEPS_DIR)/$(1)/rebar.config ]]; then \
			echo 'cd $(DEPS_DIR)/$(1) && $THIS_REBAR get-deps compile && cd ../..' ; \
			cd $(DEPS_DIR)/$(1) && $$THIS_REBAR get-deps compile && cd ../.. ; \
		else \
			make -C $(DEPS_DIR)/$(1) -f ../../erl.mk ; \
		fi \
	fi
endef

define update_dep
	@echo Updating $(1) / $(3) from $(2)
	@cd $(DEPS_DIR)/$(1); \
	git fetch origin; \
	git checkout $(3); \
	git pull --ff-only --no-rebase origin $(3)
endef

define clean_dep
	@if [[ -f $(DEPS_DIR)/$(1)/makefile ]] || [[ -f $(DEPS_DIR)/$(1)/Makefile ]] ; then \
		echo 'make -C $(DEPS_DIR)/$(1) clean' ; \
	       	make -C $(DEPS_DIR)/$(1) clean  ; \
	else \
		echo 'cd $(DEPS_DIR)/$(1) && rebar clean && cd ../..' ; \
	        cd $(DEPS_DIR)/$(1) && rebar clean && cd ../..  ; \
	fi
endef

get-deps: $(patsubst %,deps/%/,$(DEPS))    | deps-dir

build-deps: get-deps $(patsubst %,build-deps/%/,$(DEPS))

update-deps: get-deps $(patsubst %,update-deps/%/,$(DEPS))

clean-deps: get-deps $(patsubst %,clean-deps/%/,$(DEPS))

deps-dir:
	$(if $(wildcard $(DEPS_DIR)),,mkdir -p $(DEPS_DIR))

$(FULL_DEPS):
	$(call get_dep,$(@F),$(word 1,$(dep_$(@F))),$(word 2,$(dep_$(@F))))
	$(call build_dep,$(@F))

deps/%/ : | $(DEPS_DIR)/%/
	@#

build-deps/%/:
	$(call build_dep,$*)

update-deps/%/:
	$(call update_dep,$*,$(word 1,$(dep_$*)),$(word 2,$(dep_$*)))

clean-deps/%/:
	$(call clean_dep,$*)

.PHONY: get-deps deps-dir build-deps update-deps clean-deps build-deps/%/ update-deps/%/ clean-deps/%/

##------------------------------------------------------------------------------
## DIALYZER
##------------------------------------------------------------------------------
PLT_APPS ?=
DIALYZER_OPTS ?= -Werror_handling -Wrace_conditions -Wunmatched_returns # -Wunderspecs
APP_DIRS = $(wildcard ebin) $(wildcards apps/*/ebin)

build-base-plt: | ~/plts
	dialyzer --build_plt --apps erts kernel stdlib mnesia compiler crypto hipe edoc gs syntax_tools inets xmerl ssl runtime_tools public_key asn1 os_mon sasl eunit --output_plt ~/plts/base.plt

~/plts:
	mkdir ~/plts

build-plt: app
	@dialyzer -r $(DEPS_DIR) --add_to_plt --plt ~/plts/base.plt --output_plt dialyzer.plt

dialyzer: all
	dialyzer -r $(APP_DIRS) --plt dialyzer.plt -Wunmatched_returns -Werror_handling -Wrace_conditions --verbose

.PHONY: build-base-plt build-plt dialyzer

##------------------------------------------------------------------------------
## RELEASE
##------------------------------------------------------------------------------

RELX_CONFIG ?= $(CURDIR)/relx.config

ifeq ($(wildcard $(RELX_CONFIG)),)

rel:
	@echo ERROR: No relx.config found at $(RELX_CONFIG) - if it\'s in a non-standard place, then set the RELX_CONFIG variable.
	@exit 1

clean-rel:

else

RELX ?= $(CURDIR)/relx
export RELX

RELX_URL ?= https://github.com/erlware/relx/releases/download/v0.5.2/relx
RELX_OPTS ?=

define get_relx
	wget -O $(RELX) $(RELX_URL) || rm $(RELX)
	chmod +x $(RELX)
endef

rel: clean-rel all $(RELX)
	@$(RELX) -c $(RELX_CONFIG) $(RELX_OPTS)

$(RELX):
	@$(call get_relx)

clean-rel:
	@rm -rf _rel

endif

.PHONY: rel clean-rel

##------------------------------------------------------------------------------
## CLEAN
##------------------------------------------------------------------------------
clean:
	@rm -f $(DEPENDENCIES)
	@rm -rf ebin/*
	@rm -f test/*.beam

clean-docs:
	$(if $(wildcard doc/*.css),     rm doc/*.css)
	$(if $(wildcard doc/*.html),    rm doc/*.html)
	$(if $(wildcard doc/*.png),     rm doc/*.png)
	$(if $(wildcard doc/edoc-info), rm doc/edoc-info)
	@[[ -d doc/ ]] && [[ 'doc/*' = "`echo doc/*`" ]] && rmdir doc/ || true

clean-all: clean clean-docs clean-deps clean-rel

.PHONY: clean clean-docs clean-all
