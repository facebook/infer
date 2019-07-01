# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

.PHONY: default
default: exes

EXES = src/sledge
INSTALLS = sledge
FMTS = @_build/dbg/src/fmt

DBG_TARGETS = $(patsubst %,_build/dbg/%.exe,$(EXES)) $(patsubst %,_build/dbg/%.install,$(INSTALLS)) _build/dbg/sledge-help.txt
OPT_TARGETS = $(patsubst %,_build/opt/%.exe,$(EXES)) $(patsubst %,_build/opt/%.install,$(INSTALLS)) _build/opt/sledge-help.txt
DBG_OPT_TARGETS = $(patsubst %,_build/dbg-opt/%.exe,$(EXES)) $(patsubst %,_build/dbg-opt/%.install,$(INSTALLS)) _build/dbg-opt/sledge-help.txt

DUNEINS = $(shell find src model -name dune.in)
DUNES = $(patsubst %.in,%,$(DUNEINS))

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
	dune build $(DBG_TARGETS) $(DBG_OPT_TARGETS) $(OPT_TARGETS)

.PHONY: dbg
dbg: setup
	dune build $(DBG_TARGETS)

.PHONY: do
do: setup
	dune build $(DBG_OPT_TARGETS)

.PHONY: opt
opt: setup
	dune build $(OPT_TARGETS)

.PHONY: watch
watch: setup
	dune build --watch $(DBG_TARGETS) $(DBG_OPT_TARGETS) $(OPT_TARGETS)

.PHONY: test
test: setup
	dune build @_build/dbg/runtest --auto-promote

.PHONY: ci-test
ci-test: setup
	dune build @_build/dbg/runtest

BISECT_DIR = $(CURDIR)/_coverage/out

.PHONY: coverage
coverage: setup
	@rm -rf _coverage
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
	dune build $(FMTS) --auto-promote
	ocamlformat -i src/version.ml.in $(DUNEINS)
	clang-format -i model/llair_intrinsics.h model/cxxabi.cpp
	${MAKE} -C test fmt

# print any variable for Makefile debugging
print-%:
	@printf '$*='; printf '$($*)'; printf '\n'
