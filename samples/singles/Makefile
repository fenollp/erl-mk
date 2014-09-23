all: erl.mk

dep_ring  = git@bitbucket.org:fenollp/ring.git master
dep_ringo = git@bitbucket.org:fenollp/ring.git master
dep_starr = git@bitbucket.org:fenollp/ring.git master

distclean: clean clean-docs
	$(if $(wildcard deps/ ), rm -rf deps/)
	$(if $(wildcard logs/ ), rm -rf logs/)
.PHONY: distclean

include erl.mk

# Your targets after this line.

clean: clean-ebin

more: $(APP)
	echo .$(include_dirs).

debug: debug-app
