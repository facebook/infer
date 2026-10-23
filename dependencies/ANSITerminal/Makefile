PKGVERSION = $(shell git describe --always --dirty)

build:
	dune build @install

all: build
	dune build @runtest --force

test: all
	_build/default/tests/test.exe


clean:
	dune clean

.PHONY: all build test clean
