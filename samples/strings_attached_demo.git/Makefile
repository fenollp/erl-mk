all: escript


dep_strings_attached = https://github.com/fenollp/strings_attached master

-include erl.mk

# Your targets after this line.

clean: clean-ebin
distclean: clean clean-docs clean-deps clean-escript

debug: debug-app
