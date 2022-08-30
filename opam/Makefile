# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

ROOT_DIR = ..
include $(ROOT_DIR)/Makefile.config

.PHONY: all
all: infer.opam.locked infer-tests.opam.locked ocamlformat.opam.locked

# depends on opam internal state
.PHONY: infer.opam.locked
infer.opam.locked:
	$(OPAM) lock --switch $(OPAMSWITCH) ./infer.opam

# depends on opam internal state
.PHONY: infer-tests.opam.locked
infer-tests.opam.locked:
	sed -e 's/{with-test}//' -e 's/ & with-test}/}/' infer.opam > infer-tests.opam
	$(OPAM) lock --switch $(OPAMSWITCH) ./infer-tests.opam

# depends on opam internal state
.PHONY: ocamlformat.opam.locked
ocamlformat.opam.locked:
	$(OPAM) lock --switch $(OPAMSWITCH) ocamlformat
