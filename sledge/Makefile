# Copyright (c) 2018-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

.PHONY: default
default: exes

EXES = src/sledge
INSTALLS = sledge
FMTS = @_build/dev/src/fmt

facebook/Makefile:
	-@${MAKE} -s -C ../facebook setup
	-@${MAKE} -s -C facebook

-include facebook/Makefile

DBG_TARGETS = $(patsubst %,_build/dev/%.exe,$(EXES)) $(patsubst %,_build/dev/%.install,$(INSTALLS))
OPT_TARGETS = $(patsubst %,_build/release/%.exe,$(EXES)) $(patsubst %,_build/release/%.install,$(INSTALLS))

DUNEINS = $(shell find src facebook -name dune.in)
DUNES = $(patsubst %.in,%,$(DUNEINS))

.PHONY: dunes
dunes: $(DUNES)

%/dune: dune-common.in %/dune.in
	@cat $+ > $@

.PHONY: setup
setup: facebook/Makefile dunes

.PHONY: check
check: setup
	dune build @check

.PHONY: exes
exes: setup
	dune build $(DBG_TARGETS) $(OPT_TARGETS)

.PHONY: dbg
dbg: setup
	dune build $(DBG_TARGETS)

.PHONY: opt
opt: setup
	dune build $(OPT_TARGETS)

.PHONY: watch
watch: setup
	dune build --watch $(DBG_TARGETS) $(OPT_TARGETS)

.PHONY: test
test: setup
	dune build @_build/dev/runtest --auto-promote

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

# print any variable for Makefile debugging
print-%:
	@printf '$*='; printf '$($*)'; printf '\n'
