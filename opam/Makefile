include ../Makefile.autoconf

all: infer.opam.locked infer-tests.opam.locked

infer.opam.locked: infer.opam
	$(OPAM) lock --switch $(OPAMSWITCH) infer.opam

infer-tests.opam.locked: infer.opam
	sed 's/{with-test}//' infer.opam > infer-tests.opam
	$(OPAM) lock --switch $(OPAMSWITCH) infer-tests.opam

PHONY: all
