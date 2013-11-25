#erl-mk • [GitHub](//github.com/fenollp/erl-mk)

Include this in your Makefile:
```make
all: erl.mk

erl.mk:
	wget 'https://raw.github.com/fenollp/erl-mk/master/erl.mk'

include erl.mk

# Your targets after this line.
```

Now, `make -j 5`. This is the parallel exquivalent of `rebar get-deps compile`.

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

### Compilation support
| Pattern         | File type            |
| --------------- | -------------------- |
| `src/*.app.src` | Application resource |
| `src/*.erl`     | Erlang code          |
| `src/*.xrl`     | LEEX code            |
| `src/*.yrl`     | YECC code            |
| `src/*.core`    | Erlang Core code     |
| `src/*.S`       | Erlang ASM code      |

### API
| `make` target     | Action                                                      |
| ----------------- | ----------------------------------------------------------- |
| `make all`        | ⇔ `make deps app`                                           |
| `make deps`       | Fetch & compile dependencies and deps of deps, into `deps/` |
| `make app`        | Compile files from `src/`                                   |
| `make eunit`      | Compile & EUnit-test files in `test/*_tests.erl`            |
| `make docs`       | Generate the app's documentation into `doc/`                |
| `make clean-docs` | Remove `doc/{edoc-info,*.{css,html,png}}`                   |
| `make clean`      | Remove `ebin/`                                              |
| `make distclean`  | Remove `ebin/` and `deps/`                                  |

## Differences with erlang.mk
* Makes use of `make`'s fast dependency graph and parallelisation
* Much simpler design (as far as Makefiles go)
* No PROJECT variable needed, does not depend on wget
* Uses relative paths, thus no trouble with folders' name containing whitespaces

## TODO
**erl.mk** is meant to replace **erlang.mk** and `rebar`'s compile commands. However, one should still use `rebar` and `relx` for something other than compilation.
* Implement `doc` target
* Support the generic arch (eg. apps/, …)
* Implement **erlang.mk**'s behaviour on
	* `ct` target
	* compiling dtl files
	* support verbose toggle

## ¬TODO
* `relx`|`rebar` download tool | wrapper
* Dialyzer wrapper
* not achieve widespread usage

# Contact
Please [report issues](https://github.com/fenollp/erl-mk/issues) and do a lot of [Pull Requests](https://github.com/fenollp/erl-mk/pulls).
