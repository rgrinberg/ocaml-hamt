default: all

.PHONY: all
all:
	dune build

.PHONY: test
test:
	dune runtest
