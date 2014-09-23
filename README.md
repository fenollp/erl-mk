#erl-mk • [GitHub](//github.com/fenollp/erl-mk)

Include this in your Makefile:
```make
all: erl.mk

erl.mk:
	curl -fsSLo $@ 'https://raw.github.com/fenollp/erl-mk/master/erl.mk' || rm $@

include erl.mk
# Your targets after this line.

## Redefine an erl.mk target here to implement special behavior!
```

Now, `make -j`. This is exquivalent to `rebar -j get-deps compile`.

## Why?
* `rebar` is too slow for a tight test-fail-fix loop.
* **erlang.mk** had too many [quirks](//github.com/extend/erlang.mk/issues/21) and uses too many shell code & loops (ie. is slow again)
* **erl.mk** is meant to replace **erlang.mk** and `rebar`'s compile & dependencies-handling commands.
* To generate releases, dialyse code and otherwise one would need a different tool.


## Notes
* **erl.mk** requires `make` (not particularly GNU's), `erlang`, `bash` and tools like `cp` & `mv`
* If your dependencies (and theirs & so on) don't have a Makefile, **erl.mk** requires `rebar`
* Same dependency management as `rebar`'s:
    * `mv deps/*/deps/* deps/ ; rmdir deps/*/deps/`
* **erl.mk** automatically sets & exports **$(APP)**: a variable that contains the current project's name

## Usage
**erl.mk** implements most of [erlang.mk](//github.com/extend/erlang.mk)'s capabilities
except the packaging stuff, and also some of `rebar`'s commands.
Thus the compiling and dependency handling commands are available, but not ‹paste ToDo›.  

You should be all set with `make`, `make clean`, `make eunit` and `make ct`.

Dependency specification is the same as **erlang.mk**'s
```make
dep_cowboy = https://github.com/extend/cowboy.git master
dep_bullet = https://github.com/extend/bullet.git 0.4.1
```
…except that writing `DEPS = cowboy bullet` is not needed.

### Compilation support
| Glob               | File type            |
| ------------------ | -------------------- |
| `src/*.app.src`    | Application resource |
| `src/*.erl`        | Erlang code          |
| `src/*.xrl`        | Leex code            |
| `src/*.yrl`        | Yecc code            |
| `src/*.S`          | Erlang ASM code      |
| `src/*.core`       | Erlang Core code     |
| `templates/*.dtl`  | ErlyDTL templates    |
| `test/*_tests.erl` | EUnit tests          |
| `test/*_SUITE.erl` | Common Test tests    |

### API
| `make` target        | Action                                                      |
| -----------------    | ----------------------------------------------------------- |
| `make $(APP)`        | ⇔ `make deps` then `make app`                               |
| `make deps`          | Fetch & compile dependencies and deps of deps, into `deps/` |
| `make app`           | Compile files from `src/` | `templates/`                    |
| `make eunit`         | Compile & EUnit-test files in `test/*_tests.erl`            |
| `make eunit.Mod`     | Compile & EUnit-test code in `test/Mod_tests.erl`           |
| `make ct`            | Compile & CommonTest-test files in `test/*_SUITE.erl`       |
| `make ct.Mod`        | Compile & CommonTest-test code in `test/Mod_SUITE.erl`      |
| `make escript`       | Generate a stand-alone `escript` executable                 |
| `make clean-escript` | Remove `./$(APP)` executable                                |
| `make docs`          | Generate the app's documentation into `doc/`                |
| `make clean-docs`    | Remove `doc/{edoc-info,*.{css,html,png}}`                   |
| `make clean`         | Remove `ebin/`                                              |

## Differences with erlang.mk
* Compatible with dependencies that use a Makefile or `rebar`
* Automatic discovery of files given that project respects OTP directory structure
* Makes use of `make`'s fast dependency graph and parallelisation
* Much simpler design (as far as Makefiles go)
* No PROJECT variable needed, does not depend on `wget` nor `curl`
* Uses relative paths, thus no trouble with superior folders' name containing whitespaces

## To Do
* Support the whole OTP arch (eg. `lib/`, `c_src/`, subdirectories, …)
* Proper compilation of `ct` target's files (ie. `-no_auto_compile` issue)
* Optional `dep_*` notation retrieving snapshots instead of whole git repos
    * See `git-archive --format tar --remote ‹URI› ‹commit› > some.tar` and GitHub's support
* Refactor using `aTarget: localVariable = val`
    * *localVariable is then expanded when used, not when declared*
    * Also look at `aTarget:: …`. Especially for `clean` targets.
* Include test suite in codebase
* Refactor `escript` using `filelib:fold_files/5`
* Add `make help`
* Require `sh` instead of `bash`
* Option to compile `deps/` using `make -f erl.mk …` instead of default `rebar`
    * How about `depsWith=erl.mk make deps`?


## ¬ To Do
* `relx`|`rebar` download tool | wrapper
* Dialyzer wrapper

# Contact
Please [report issues](//github.com/fenollp/erl-mk/issues) and [submit Pull Requests](//github.com/fenollp/erl-mk/pulls).
