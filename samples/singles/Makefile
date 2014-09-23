all: erl.mk

erl.mk:
	curl -fsSLo $@ 'https://raw.github.com/fenollp/erl-mk/master/erl.mk' || rm $@

dep_ring  = git@bitbucket.org:fenollp/ring.git master
dep_ringo = git@bitbucket.org:fenollp/ring.git master
dep_starr = git@bitbucket.org:fenollp/ring.git master

include erl.mk

# Your targets after this line.

distclean: clean clean-docs clean-deps
	$(if $(wildcard logs/), rm -rf logs/)
.PHONY: distclean

clean: clean-ebin

local_vars_indeed_inaccessible: $(APP)
	echo .$(include_dirs).

debug: debug-app
