all: deps
	@$(MAKE) app
.PHONY: all

APP = $(patsubst src/%.app.src,%,$(wildcard src/*.app.src))

### DEPS -- Fetches & compiles deps recursively then moves every dep to deps/

deps: $(patsubst dep_%,deps/%/,$(filter dep_%,$(.VARIABLES)))
	$(if $(wildcard deps/*/deps/), \
	    mv -v deps/*/deps/* deps/ 2>/dev/null ; rmdir deps/*/deps/)
.PHONY: deps

deps/%/:
	git clone -n -- $(word 1,$(dep_$*)) $@
	cd $@ && git checkout -q $(word 2,$(dep_$*)) && cd ../..
	@bash -c "if [[ -f $@/Makefile ]]; \
	then echo '$(MAKE) -C $@ all' ; \
	           $(MAKE) -C $@ all  ; \
	else echo 'cd $@ && rebar get-deps compile && cd ../..' ; \
	           cd $@ && rebar get-deps compile && cd ../..  ; fi"

### APP -- Compiles src/ into ebin/

app: ebin/$(APP).app \
     $(foreach ext, erl xrl yrl S core, \
         $(patsubst src/%.$(ext), ebin/%.beam, $(wildcard src/*.$(ext)))) \
     $(patsubst templates/%.dtl,  ebin/%_dtl.beam,$(wildcard templates/*.dtl))
.PHONY: app

ebin/%.app: src/%.app.src               | ebin/
	@erl -noshell \
	     -eval 'case file:consult("$<") of {ok,_} -> ok ; {error,{_,_,M}} -> io:format("$<: ~s~s\n",M), halt(1) end.' \
	     -s init stop
	cp $< $@

ebin/%.beam: first_flags = -o ebin/ $(patsubst %,-pz %,$(wildcard deps/*/ebin/))
ebin/%.beam: include_files = $(wildcard include/*.hrl)
ebin/%.beam: include_dirs = -I include/ -I deps/

ebin/%.beam: $(include_files) src/%.erl | ebin/
	erlc -v $(first_flags) $(ERLCFLAGS) $(include_dirs) $<

ebin/%.beam: $(include_files) src/%.xrl | ebin/
	erlc    $(first_flags) $(ERLCFLAGS) $<
	erlc    $(first_flags) $(ERLCFLAGS) $(include_dirs) ebin/$*.erl

ebin/%.beam: $(include_files) src/%.yrl | ebin/
	erlc    $(first_flags) $(ERLCFLAGS) $<
	erlc    $(first_flags) $(ERLCFLAGS) $(include_dirs) ebin/$*.erl

ebin/%.beam: $(include_files) src/%.S   | ebin/
	erlc -v $(first_flags) $(ERLCFLAGS) $(include_dirs) +from_asm $<

ebin/%.beam: $(include_files) src/%.core| ebin/
	erlc -v $(first_flags) $(ERLCFLAGS) $(include_dirs) +from_core $<

ebin/%_dtl.beam: templates/%.dtl        | ebin/
	$(if $(wildcard deps/erlydtl/),, \
	    $(error Error compiling $<: deps/erlydtl/ not found))
	@erl -noshell -pz ebin/ $(patsubst %,-pz %,$(wildcard deps/*/ebin/)) \
	     -eval 'io:format("Compiling ErlyDTL template: $< -> $@\n").' \
	     -eval 'erlydtl:compile("$<", $*_dtl, [{out_dir,"ebin/"},{auto_escape,false}]).' \
	     -s init stop

ebin/:
	mkdir $@

### EUNIT -- Compiles (into ebin/) & run EUnit tests (test/*_test.erl files)

eunit: $(patsubst test/%_tests.erl, eunit.%, $(wildcard test/*_tests.erl))
.PHONY: eunit

eunit.%: first_flags = -o ebin/ $(patsubst %,-pz %,$(wildcard deps/*/ebin/))
eunit.%: include_files = $(wildcard include/*.hrl)
eunit.%: include_dirs = -I include/ -I deps/

#eunit.%: ebin/%_tests.beam #FIXME
eunit.%: $(include_files)               | all
	erlc -v $(first_flags) -DTEST=1 -DEUNIT $(ERLCFLAGS) $(include_dirs) test/$*_tests.erl
	@erl -noshell -pz ebin/ $(patsubst %,-pz %,$(wildcard deps/*/ebin/)) \
	     -eval 'io:format("Module $*_tests:\n"), eunit:test($*_tests).' \
	     -s init stop
.PHONY: eunit.%

#ebin/%_tests.beam: test/%_tests.erl     | all #FIXME so that this can be used instead of the erlc line above
#	erlc -v $(first_flags) -DTEST=1 -DEUNIT $(ERLCFLAGS) $(include_dirs) $<
#.PRECIOUS: ebin/%_tests.beam

### CT -- Compiles (into ebin/) & run Common Test tests (test/*_SUITE.erl)

ct: $(patsubst test/%_SUITE.erl, ct.%, $(wildcard test/*_SUITE.erl))
.PHONY: ct

ct.%: first_flags = -o ebin/ $(patsubst %,-pz %,$(wildcard deps/*/ebin/))
ct.%: include_files = $(wildcard include/*.hrl)
ct.%: include_dirs = -I include/ -I deps/

ct.%: ebin/%_SUITE.beam                 | logs/
#	FIXME use -no_auto_compile and ebin/%_SUITE.beam properly.
	@ct_run -noshell -dir test/ -logdir logs/ \
	        -pz ebin/ $(patsubst %,-pz %,$(realpath $(wildcard deps/*/ebin/))) \
	        -suite $*_SUITE || true
.PHONY: ct.%

#FIXME make ct depend on target all, and in a parallel-safe way.
#ebin/%_SUITE.beam: test/%_SUITE.erl     | ebin/ all <-- this blocks if no ebin/ (try swapping them?)
ebin/%_SUITE.beam: $(include_files) test/%_SUITE.erl    | ebin/
	erlc -v $(first_flags) $(ERLCFLAGS) $(include_dirs) $<
.PRECIOUS: ebin/%_SUITE.beam

logs/:
	mkdir $@

### ESCRIPT -- Create a stand-alone EScript executable

escript:                                | all
	@erl -noshell \
	     -eval 'io:format("Compiling escript \"./$(APP)\".\n").' \
	     -eval 'G = fun (_, [], Acc) -> Acc; (F, [Path|Rest], Acc) -> case filelib:is_dir(Path) of false -> F(F, Rest, [Path|Acc]); true -> {ok, Listing} = file:list_dir(Path), Paths = [filename:join(Path, Name) || Name <- Listing, Name /= ".git"], F(F, Paths ++ Rest, Acc) end end, escript:create("$(APP)", [ {shebang,default}, {comment,""}, {emu_args,"-escript main $(APP)"}, {archive, [{case File of "./deps/"++File1 -> File1; _ -> "$(APP)/" ++ File -- "./" end,element(2,file:read_file(File))} || File <- G(G, ["."], [])], []}]).' \
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

### CLEAN-DOCS -- Removes generated stuff from doc/

clean-docs:
	$(if $(wildcard doc/*.css),     rm doc/*.css)
	$(if $(wildcard doc/*.html),    rm doc/*.html)
	$(if $(wildcard doc/*.png),     rm doc/*.png)
	$(if $(wildcard doc/edoc-info), rm doc/edoc-info)
	@bash -c '[[ -d doc/ ]] && [[ ''doc/*'' = "`echo doc/*`" ]] && rmdir doc/ || true'
.PHONY: clean-docs

### CLEAN -- Removes ebin/ if it exists

clean:
	$(if $(wildcard ebin/),rm -r ebin/)
.PHONY: clean
