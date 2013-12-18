#erl-mk • [GitHub](//github.com/fenollp/erl-mk)

Include this in your Makefile:
```make
all: erl.mk

erl.mk:
	wget -nv -O erl.mk 'https://raw.github.com/fenollp/erl-mk/master/erl.mk' || rm erl.mk

DEPS =

include erl.mk

# Your targets after this line.

distclean: clean clean-docs
	$(if $(wildcard deps/ ), rm -rf deps/)
	$(if $(wildcard logs/ ), rm -rf logs/)
	$(if $(wildcard erl.mk), rm erl.mk   )
.PHONY: distclean
```

Now, `make -j`. This is the parallel exquivalent of `rebar -j get-deps compile`.

## Why?
* `rebar` is too slow for a tight dev-fail loop.
* **erlang.mk** had too many [quirks](https://github.com/extend/erlang.mk/issues/21) and uses too many shell code & loops (ie slow again)

## Usage
**erl.mk** implements most of [erlang.mk](https://github.com/extend/erlang.mk)'s capabilities
except the packaging stuff, and also some of `rebar`'s commands.
Thus the compiling and dependency handling commands are available, but not ‹paste TODO›.  

You should be all set with `make`, `make clean` and `make eunit`.

Dependency specification is the same as **erlang.mk**'s.
```make
DEPS = cowboy bullet
dep_cowboy = https://github.com/extend/cowboy.git 0.8.4
dep_bullet = https://github.com/extend/bullet.git 0.4.1
```
…and that's all you'll have to put in your Makefile. Ever.

### Compilation support
| Pattern            | File type            |
| ------------------ | -------------------- |
| `src/*.app.src`    | Application resource |
| `src/*.erl`        | Erlang code          |
| `src/*.xrl`        | LEEX code            |
| `src/*.yrl`        | YECC code            |
| `src/*.S`          | Erlang ASM code      |
| `src/*.core`       | Erlang Core code     |
| `templates/*.dtl`  | ErlyDTL templates    |
| `test/*_tests.erl` | EUnit tests          |
| `test/*_SUITE.erl` | Common Test tests    |

### API
| `make` target     | Action                                                      |
| ----------------- | ----------------------------------------------------------- |
| `make all`        | ⇔ `make deps app`                                           |
| `make deps`       | Fetch & compile dependencies and deps of deps, into `deps/` |
| `make app`        | Compile files from `src/` | `templates/`                    |
| `make eunit`      | Compile & EUnit-test files in `test/*_tests.erl`            |
| `make eunit.Mod`  | Compile & EUnit-test code in `test/Mod_tests.erl`           |
| `make ct`         | Compile & EUnit-test files in `test/*_SUITE.erl`            |
| `make ct.Mod`     | Compile & CommonTest-test code in `test/Mod_SUITE.erl`      |
| `make docs`       | Generate the app's documentation into `doc/`                |
| `make clean-docs` | Remove `doc/{edoc-info,*.{css,html,png}}`                   |
| `make clean`      | Remove `ebin/`                                              |

## Differences with erlang.mk
* Compatible with deps that use a Makefile or `rebar`
* Automatic discovery of files given that project respects OTP directory structure
* Makes use of `make`'s fast dependency graph and parallelisation
* Much simpler design (as far as Makefiles go)
* No PROJECT variable needed, does not depend on wget
* Uses relative paths, thus no trouble with folders' name containing whitespaces

## TODO
**erl.mk** is meant to replace **erlang.mk** and `rebar`'s compile commands. However, one should still use `rebar` and `relx` for something other than compilation.
* Support the generic arch (eg. apps/, …)
* Proper compilation of `ct` target's files (ie `-no_auto_compile` issue)
* Do | Document the ≠ ENV variables available.
* Watch HRLs like in https://github.com/extend/erlang.mk/pull/27

## ¬TODO
* `relx`|`rebar` download tool | wrapper
* Dialyzer wrapper

# Contact
Please [report issues](https://github.com/fenollp/erl-mk/issues) and do a lot of [Pull Requests](https://github.com/fenollp/erl-mk/pulls).
