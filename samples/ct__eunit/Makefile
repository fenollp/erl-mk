all: erl.mk
	$(MAKE) $(APP)

include erl.mk
# Your targets after this line.

clean: clean-ebin
distclean: clean clean-deps
	$(if $(wildcard logs/), rm -r logs/)

.PHONY: distclean clean test

test: ct eunit
