all:

dep_strings_attached = https://github.com/fenollp/strings_attached.git

include erl.mk

# Your targets after this line.

distclean: clean clean-docs
	$(if $(wildcard deps/ ), rm -rf deps/)
	$(if $(wildcard strings_attached_demo), rm strings_attached_demo)
.PHONY: distclean

debug: all
	erl -pa ebin/ -pa deps/*/ebin/ -eval 'c:l($(APP)).'

all: escript
