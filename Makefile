default: all

.PHONY: all
all:
	dune build @install @all
