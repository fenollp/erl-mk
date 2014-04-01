#erl-mk • [GitHub](//github.com/id3as/erl-mk)

Makefile rules for building and testing Erlang applications, supports multiple applications inside the apps folder.

Forked from [fenollp/erl-mk](https://github.com/fenollp/erl-mk), with inspiration from [erlang.mk](https://github.com/extend/erlang.mk) and [stdapp.mk](https://github.com/richcarl/stdapp.mk)


Include this in your Makefile:
```make
export ERLCFLAGS = +debug_info +warn_export_vars +warn_shadow_vars +warn_obsolete_guard +'{lager_truncation_size, 10240}' 

DEPS = gproc cowboy jsx
dep_gproc = git://github.com/esl/gproc.git master
dep_cowboy = git://github.com/extend/cowboy.git master
dep_jsx = git://github.com/talentdeficit/jsx.git master

erl.mk:
	@wget -nv -O $@ 'https://raw.github.com/id3as/erl-mk/master/erl.mk' || rm -f $@

-include erl.mk

# Your targets after this line.
```

Now, `make` or `make -j`.

## Usage
You should be all set with `make`, `make clean`, `make eunit` and `make ct`.

Dependency specification is the same as **erlang.mk**'s.
```make
DEPS = cowboy bullet
dep_cowboy = https://github.com/extend/cowboy.git 0.8.4
dep_bullet = https://github.com/extend/bullet.git 0.4.1
```
…but without the packaging stuff.  Maybe that'll get added at some point...

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
| `test/*_SUITE.erl` | Common Test tests    |

### API
| `make` target         | Action                                                          |
| --------------------- | --------------------------------------------------------------- |
| `make all`            | `make get-deps app`                                             |
| `make get-deps`       | Fetch dependencies and deps of deps, into `deps/`               |
| `make build-deps`     | Fetch & compile dependencies and deps of deps, into `deps/`     |
| `make update-deps`    | Update dependencies and deps of deps, into `deps/`              |
| `make app`            | Compile files from `src/` | `templates/`                        |
| `make eunit`          | Compile & EUnit-test files in `src/*.erl`                       |
| `make eunit.Mod`      | Compile & EUnit-test code in `src/Mod.erl`                      |
| `make ct`             | Compile & EUnit-test files in `test/*_SUITE.erl`                |
| `make ct.Mod`         | Compile & CommonTest-test code in `test/Mod_SUITE.erl`          |
| `make escript`        | Generate a stand-alone `escript` executable                     |
| `make docs`           | Generate the app's documentation into `doc/`                    |
| `make clean-docs`     | Remove `doc/{edoc-info,*.{css,html,png}}`                       |
| `make clean`          | Remove `ebin/`                                                  |
| `make build-base-plt` | Makes a plt for the standard erl libraries into ~/plts/base.plt |
| `make build-plt`      | Makes a plt for everything in `deps/`                           |
| `make dialyzer`       | Runs dialyzer on the current project                            |
| `make rel`            | Uses `relx` to build a release package for each app in the apps subdirectory.  Each app needs its own `relx.config` file in the root of that application's directory (e.g. `apps/appname/relx.config`).  The releases are placed in `apps/appname/_rel`

### App directory structure support
A lot of projects use a structure of

```
project
	apps
		app1
			src
			include
			ebin
		app2
			src
			include
			ebin
	deps
		dep1
		dep2
```

`erl.mk` supports these - just put the root makefile (the example above would work) at the root of the tree.  Then the commands listed above should be applied to each application in the `apps` directory.  You can also do things like:

```
make app1.ct		# just run the common test suites for app1
make app1.eunit.foo	# just run the eunit tests in the foo module in app1
```

Basically any of the above commands can have ```appname.``` prepended, and they will apply to just that application.

## TODO
* Lots of stuff!  Pull requests most welcome
