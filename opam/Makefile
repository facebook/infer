include ../Makefile.autoconf

all: infer.opam.locked infer-tests.opam.locked

# depends on opam internal state
.PHONY: infer.opam.locked
infer.opam.locked:
	$(OPAM) lock --switch $(OPAMSWITCH) ./infer.opam

# depends on opam internal state
.PHONY: infer-tests.opam.locked
infer-tests.opam.locked:
	sed 's/{with-test}//' infer.opam > infer-tests.opam
	$(OPAM) lock --switch $(OPAMSWITCH) ./infer-tests.opam

PHONY: all
