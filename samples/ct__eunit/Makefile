all: erl.mk

distclean: clean clean-docs
	$(if $(wildcard deps/ ), rm -rf deps/)
	$(if $(wildcard logs/ ), rm -rf logs/)
#	$(if $(wildcard erl.mk), rm erl.mk   )
.PHONY: distclean

include erl.mk

# Your targets after this line.

