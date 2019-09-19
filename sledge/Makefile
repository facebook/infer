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

TARGETS = $(DBG_TARGETS) $(subst dbg,dbg-opt,$(DBG_TARGETS)) $(subst dbg,opt,$(DBG_TARGETS))

dune_build_dbg = dune build $(DBG_TARGETS)
dune_install_dbg = dune install --context=dbg --prefix=_build/_install/dbg sledge 2>/dev/null

dune_build_dbg_opt = $(subst dbg,dbg-opt,$(dune_build_dbg))
dune_install_dbg_opt = $(subst dbg,dbg-opt,$(dune_install_dbg))

dune_build_opt = $(subst dbg,opt,$(dune_build_dbg))
dune_install_opt = $(subst dbg,opt,$(dune_install_dbg))

DUNEINS = $(shell find src model -name dune.in)
DUNES = $(patsubst %.in,%,$(DUNEINS))

.PHONY: dunes
dunes: $(DUNES)

%/dune: dune-common.in %/dune.in
	cat $+ > $@

.PHONY: setup
setup: dunes

.PHONY: check
check: setup
	dune build @check

.PHONY: exes
exes: setup
	dune build $(TARGETS)
	$(dune_install_dbg)
	$(dune_install_dbg_opt)
	$(dune_install_opt)

.PHONY: dbg
dbg: setup
	$(dune_build_dbg)
	$(dune_install_dbg)

.PHONY: do
do: setup
	$(dune_build_dbg_opt)
	$(dune_install_dbg_opt)

.PHONY: opt
opt: setup
	$(dune_build_opt)
	$(dune_install_opt)

.PHONY: watch
watch: setup
	dune build --watch $(TARGETS)

.PHONY: test
test: setup
	dune build @_build/dbg/runtest --auto-promote

.PHONY: ci-test
ci-test: setup
	dune build @_build/dbg/runtest

BISECT_DIR = $(CURDIR)/_coverage/out

.PHONY: coverage
coverage: setup
	rm -rf _coverage
	mkdir -p $(BISECT_DIR)
	BISECT_FILE=$(BISECT_DIR)/bisect dune build --force @_build/coverage/runtest
	find $(BISECT_DIR) -type f | xargs bisect-ppx-report -I _build/coverage/ -text _coverage/summary.txt -html _coverage/
	@echo "open _coverage/index.html"

.PHONY: clean
clean:
	dune clean
	rm -rf _coverage $(DUNES)

.PHONY: fmt
fmt:
	dune build $(FMTS) --auto-promote
	ocamlformat -i $(DUNEINS)
	clang-format -i model/llair_intrinsics.h model/cxxabi.cpp
	${MAKE} -C test fmt

# print any variable for Makefile debugging
print-%:
	@printf '$*='; printf '$($*)'; printf '\n'
