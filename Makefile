default: all

configure:
	ocaml setup.ml -configure

configure-test:
	ocaml setup.ml -configure --enable-tests

build:
	ocaml setup.ml -build

all:
	ocaml setup.ml -all

test:
	ocaml setup.ml -test

doc:
	ocaml setup.ml -doc

clean:
	ocaml setup.ml -clean

scrub: clean
	ocaml setup.ml -distclean

install:
	ocaml setup.ml -install

uninstall:
	ocaml setup.ml -uninstall

reinstall:
	ocaml setup.ml -reinstall

.PHONY: build all build default install uninstall configure-test configure
