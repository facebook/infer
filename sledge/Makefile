# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

.PHONY: default
default: exes

EXES = bin/sledge
INSTALLS = sledge

DBG_TARGETS = $(patsubst %,_build/dbg/%.exe,$(EXES)) $(patsubst %,_build/dbg/%.install,$(INSTALLS))

TARGETS = $(DBG_TARGETS) $(subst dbg,dbg-opt,$(DBG_TARGETS)) $(subst dbg,opt,$(DBG_TARGETS)) _build/opt/sledge-help.txt

dune_build_dbg = dune build $(DBG_TARGETS)
dune_install_dbg = dune install --context=dbg --prefix=_build/_install/dbg sledge 2>/dev/null

dune_build_dbg_opt = $(subst dbg,dbg-opt,$(dune_build_dbg))
dune_install_dbg_opt = $(subst dbg,dbg-opt,$(dune_install_dbg))

dune_build_opt = $(subst dbg,opt,$(dune_build_dbg))
dune_install_opt = $(subst dbg,opt,$(dune_install_dbg))

.PHONY: check
check:
	dune build @check

.PHONY: exes
exes:
	dune build $(TARGETS)
	$(dune_install_dbg)
	$(dune_install_dbg_opt)
	$(dune_install_opt)

.PHONY: dbg
dbg:
	$(dune_build_dbg)
	$(dune_install_dbg)

.PHONY: dbg-opt
dbg-opt:
	$(dune_build_dbg_opt)
	$(dune_install_dbg_opt)

.PHONY: opt
opt:
	$(dune_build_opt)
	$(dune_install_opt)

.PHONY: watch
watch:
	dune build --watch --terminal-persistence=clear-on-rebuild $(TARGETS)

.PHONY: test
test:
	-dune build @_build/dbg/runtest --auto-promote
	dune build @fmt --auto-promote

.PHONY: ci-test
ci-test:
	dune build @_build/dbg/runtest

.PHONY: clean
clean:
	dune clean

.PHONY: fmt
fmt:
	dune build @fmt --auto-promote
	clang-format -i model/llair_intrinsics.h model/cxxabi.cpp
	${MAKE} -C test fmt

# print any variable for Makefile debugging
print-%:
	@printf '$*='; printf '$($*)'; printf '\n'
