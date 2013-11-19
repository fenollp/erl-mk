#erl-mk • [GitHub](//github.com/fenollp/erl-mk)

Include this in your Makefile:
```make
all: erl.mk

erl.mk:
	wget https://raw.github.com/fenollp/erl-mk/master/erl.mk

include erl.mk

# Your targets after this line.
```

## Why?
* `rebar` is too slow for a tight dev-fail loop.
* **erlang.mk** had too many [quirks](https://github.com/extend/erlang.mk/issues/21) and uses too many shell code & loops (ie slow again)

## Usage
**erl.mk** implements most of [erlang.mk](https://github.com/extend/erlang.mk)'s capabilities
except the packaging stuff, and also some of `rebar`'s commands.
Thus the compiling and dependency handling commands are available, but not ‹paste TODO›.

Dependency specification is the same as **erlang.mk**'s. But it builds a `rebar`-like deps/ architecture.
```make
DEPS = cowboy bullet
dep_cowboy = https://github.com/extend/cowboy.git 0.8.4
dep_bullet = https://github.com/extend/bullet.git 0.4.1
```

### Differences with erlang.mk
* Makes use of `make`'s fast dependency graph, `make -j` works.
* Much simpler design (as far as Makefiles go)
* No PROJECT variable needed, does not depend on wget

## TODO
**erl.mk** is meant to replace **erlang.mk** and `rebar`'s compile commands. However, one should still use `rebar` and `relx`.
* Implement `doc` target
* Add `rebar`-style `eunit` target
* Support the generic arch (eg. apps/, …)
* Implement **erlang.mk**'s behaviour on
	* common test
	* test deps
	* compiling dtl files
	* support options and ENV variables like ERLCFLAGS

## ¬TODO
* `relx`|`rebar` download tool | wrapper
* Dialyzer wrapper
* not achieve widespread usage

# Contact
Please [report issues](https://github.com/fenollp/erl-mk/issues) and do a lot of [Pull Requests](https://github.com/fenollp/erl-mk/pulls).
