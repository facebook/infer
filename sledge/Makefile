# Copyright (c) 2018-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Expected workflow is to launch `make watch` in a terminal to
# continuously rebuild when files change, and to use `make check` in
# the editor to quickly type-check and populate files used by merlin
# for cross-module information.

.PHONY: default
default: exes

DUNEINS=$(shell find src -name dune.in)
DUNES=$(patsubst %.in,%,$(DUNEINS))

.PHONY: dunes
dunes: $(DUNES)

%/dune: dune-common.in %/dune.in
	@cat $+ > $@

.PHONY: setup
setup: dunes

.PHONY: check
check: setup
	dune build @check

.PHONY: exes
exes: setup
	dune build _build/dev/src/sledge.exe _build/release/src/sledge.exe _build/dev/sledge.install _build/release/sledge.install

.PHONY: watch
watch: setup
	dune build --watch _build/dev/src/sledge.exe _build/release/src/sledge.exe

.PHONY: test
test:
	dune build @_build/dev/runtest --auto-promote

BISECT_DIR=$(CURDIR)/_coverage/out

.PHONY: coverage
coverage: setup
	@mkdir -p $(BISECT_DIR)
	@BISECT_FILE=$(BISECT_DIR)/bisect dune build --force @_build/coverage/runtest
	@find $(BISECT_DIR) -type f | xargs bisect-ppx-report -I _build/coverage/ -text _coverage/summary.txt -html _coverage/
	@echo "open _coverage/index.html"

.PHONY: clean
clean:
	dune clean
	rm -rf _coverage $(DUNES)

.PHONY: fmt
fmt:
	dune build @_build/dev/src/fmt --auto-promote
	ocamlformat -i src/version.ml.in $(DUNEINS)
	clang-format -i model/llair_intrinsics.h model/cxxabi.cpp

# print any variable for Makefile debugging
print-%:
	@printf '$*='; printf '$($*)'; printf '\n'
