all: deps app
.PHONY: all

APP = $(patsubst src/%.app.src,%,$(wildcard src/*.app.src))

### DEPS -- Fetches & compiles deps recursively then moves every dep to deps/

deps : $(patsubst dep_%,deps/%/,$(filter dep_%,$(.VARIABLES)))    | deps-dir
	$(if $(shell [[ 'deps/*/deps/*' != "`echo deps/*/deps/*`" ]] && echo y), \
	    mv -v deps/*/deps/* deps/ && rmdir $(wildcard deps/*/deps/))
.PHONY: deps

deps-dir:
	$(if $(wildcard deps/),,mkdir deps/)

lel = https://github.com/fenollp/erl-mk.git fenollp erl-mk master
lel = https://github.com/fenollp/erl-mk.git master
# git@github.com/fenollp/erl-mk.git https://github.com/fenollp/erl-mk
# https://github.com/fenollp/erl-mk/archive/master.zip
deps/%/:
	if [[ $(words $(dep_$*)) = 4 ]]; then \
	    case $(word 1,$(dep_$*)) in \
	    *github.com*) \
	        curl -sS 'https://codeload.github.com/$(word 2,$(dep_$*))/$(word 3,$(dep_$*))/zip/$(word 4,$(dep_$*))' -o deps/$*.zip ; \
	        unzip -q deps/$*.zip -d deps/ && rm deps/$*.zip && mv 'deps/$(word 3,$(dep_$*))-$(word 4,$(dep_$*))' 'deps/$(word 3,$(dep_$*))' ; \
	    ;; *) \
	        git archive --format tar --remote $(word 1,$(dep_$*)) $(word 2,$(dep_$*)) deps/$*.tar && \
	        tar xvf deps/$*.tar && rm deps/$*.tar \
	        || git clone -n -- $(word 1,$(dep_$*)) $@ && \
	        cd $@ && git checkout -q $(word 2,$(dep_$*)) && cd ../.. \
	    esac \
	else \
	    git clone -n -- $(word 1,$(dep_$*)) $@ && \
	    cd $@ && git checkout -q $(word 2,$(dep_$*)) && cd ../.. ; \
	fi
	@exit 1
	if [[ -f $@/Makefile ]]; \
	 then echo 'make -C $@ all' ; \
	            make -C $@ all  ; \
	 else echo 'cd $@ && rebar get-deps compile && cd ../..' ; \
	            cd $@ && rebar get-deps compile && cd ../..  ; fi

### APP -- Compiles src/ into ebin/

app: ebin/$(APP).app \
     $(foreach ext, erl xrl yrl S core, \
         $(patsubst src/%.$(ext), ebin/%.beam, $(wildcard src/*.$(ext)))) \
     $(patsubst templates/%.dtl,  ebin/%_dtl.beam,$(wildcard templates/*.dtl))
.PHONY: app

ebin/%.app: src/%.app.src                       | ebin/
	@erl -noshell \
	     -eval 'case file:consult("$<") of {ok,_}->ok; {error,{_,_,M}}->io:format("$<: ~s~s\n",M),halt(1) end.' \
	     -s init stop
	cp $< $@

ebin/%.beam: src/%.erl $(wildcard include/*)    | ebin/
	erlc -o ebin/ $(ERLCFLAGS) -v -Iinclude/ -Ideps/ $<

ebin/%.beam: src/%.xrl $(wildcard include/*)    | ebin/
	erlc -o ebin/ $(ERLCFLAGS) $<
	erlc -o ebin/ ebin/$*.erl

ebin/%.beam: src/%.yrl $(wildcard include/*)    | ebin/
	erlc -o ebin/ $(ERLCFLAGS) $<
	erlc -o ebin/ ebin/$*.erl

ebin/%.beam: src/%.S $(wildcard include/*)      | ebin/
	erlc -o ebin/ $(ERLCFLAGS) -v +from_asm -Iinclude/ -Ideps/ $<

ebin/%.beam: src/%.core $(wildcard include/*)   | ebin/
	erlc -o ebin/ $(ERLCFLAGS) -v +from_core -Iinclude/ -Ideps/ $<

ebin/%_dtl.beam: templates/%.dtl                | ebin/
	$(if $(shell [[ ! -d deps/erlydtl ]] && echo y), \
	    $(error Error compiling $<: deps/erlydtl/ not found))
	@erl -noshell -pa ebin/ -pa deps/*/ebin/ \
	     -eval 'io:format("Compiling ErlyDTL template: $< -> $@\n").' \
	     -eval 'erlydtl:compile("$<", $*_dtl, [{out_dir,"ebin/"},{auto_escape,false}]).' \
	     -s init stop

ebin/:
	mkdir ebin/

### EUNIT -- Compiles (into ebin/) & run EUnit tests (test/*_test.erl files).

eunit: $(patsubst test/%_tests.erl, eunit.%, $(wildcard test/*_tests.erl))
.PHONY: eunit

#eunit.%: ebin/%_tests.beam
eunit.%:                                | all
	erlc -o ebin/ -DTEST=1 -DEUNIT $(ERLCFLAGS) -v -Iinclude/ -Ideps/ test/$*_tests.erl
	@erl -noshell -pa ebin/ -pa deps/*/ebin/ \
	     -eval 'io:format("Module $*_tests:\n"), eunit:test($*_tests).' \
	     -s init stop
.PHONY: eunit.%

#ebin/%_tests.beam: test/%_tests.erl     | all #TODO fix something so that this can be used instead of the erlc line above
#	erlc -o ebin/ -DTEST=1 -DEUNIT $(ERLCFLAGS) -v -Iinclude/ -Ideps/ $<
#.PRECIOUS: ebin/%_tests.beam

### CT -- Compiles (into ebin/) & run Common Test tests (test/*_SUITE.erl).

ct: $(patsubst test/%_SUITE.erl, ct.%, $(wildcard test/*_SUITE.erl))
.PHONY: ct

ct.%: ebin/%_SUITE.beam                 | logs/
#	Todo: use -no_auto_compile and ebin/%_SUITE.beam properly.
	@ct_run -noshell -dir test/ -logdir logs/ \
	        -pa ebin/ -pa $(wildcard $(shell pwd)/deps/*/ebin/) \
	        -suite $*_SUITE || true
.PHONY: ct.%

#todo: make ct depend on target all, and in a parallel-safe way.
#ebin/%_SUITE.beam: test/%_SUITE.erl     | ebin/ all <-- this blocks if no ebin/
ebin/%_SUITE.beam: test/%_SUITE.erl     | ebin/
	erlc -o ebin/ $(ERLCFLAGS) -v -Iinclude/ -Ideps/ $<
.PRECIOUS: ebin/%_SUITE.beam

logs/:
	mkdir logs/

### ESCRIPT -- Create a stand-alone EScript executable.

escript: | all
	@erl -noshell \
	     -eval 'io:format("Compiling escript \"./$(APP)\".\n").' \
	     -eval 'escript:create("$(APP)", [ {shebang,default}, {comment,""}, {emu_args,"-escript $(APP)"}, {archive, [{case F of "ebin/"++E -> E; "deps/"++D -> D end, element(2,file:read_file(F))} || F <- filelib:wildcard("ebin/*") ++ filelib:wildcard("deps/*/ebin/*")], []}]).' \
	     -eval '{ok, Mode8} = file:read_file_info("$(APP)"), ok = file:change_mode("$(APP)", element(8,Mode8) bor 8#00100).' \
	     -s init stop

### DOCS -- Compiles the app's documentation into doc/

docs: $(foreach ext,app.src erl xrl yrl S core, $(wildcard src/*.$(ext))) \
                                                $(wildcard doc/overview.edoc)
	@erl -noshell \
	     -eval 'io:format("Compiling documentation for $(APP).\n").' \
	     -eval 'edoc:application($(APP), ".", [$(EDOC_OPTS)]).' \
	     -s init stop
.PHONY: docs

### CLEAN-DOCS -- Removes generated doc/ stuff.

clean-docs:
	$(if $(wildcard doc/*.css),     rm doc/*.css)
	$(if $(wildcard doc/*.html),    rm doc/*.html)
	$(if $(wildcard doc/*.png),     rm doc/*.png)
	$(if $(wildcard doc/edoc-info), rm doc/edoc-info)
	@[[ -d doc/ ]] && [[ 'doc/*' = "`echo doc/*`" ]] && rmdir doc/ || true
.PHONY: clean-docs

### CLEAN -- Removes ebin/

clean:
	$(if $(wildcard ebin/),rm -r ebin/)
.PHONY: clean
