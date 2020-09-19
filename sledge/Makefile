# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

.PHONY: default
default: exes

EXES = cli/sledge_cli report/sledge_report
INSTALLS = sledge

DEBUG_TARGETS = $(patsubst %,_build/debug/%.exe,$(EXES)) $(patsubst %,_build/debug/%.install,$(INSTALLS))

TARGETS = $(DEBUG_TARGETS) $(subst debug,trace,$(DEBUG_TARGETS)) $(subst debug,release,$(DEBUG_TARGETS)) _build/release/sledge-help.txt

dune_build_debug = dune build $(DEBUG_TARGETS)
dune_install_debug = dune install --context=debug --prefix=_build/_install/debug sledge 2>/dev/null

dune_build_debug_release = $(subst debug,trace,$(dune_build_debug))
dune_install_debug_release = $(subst debug,trace,$(dune_install_debug))

dune_build_release = $(subst debug,release,$(dune_build_debug))
dune_install_release = $(subst debug,release,$(dune_install_debug))

.PHONY: check
check:
	dune build @check

.PHONY: exes
exes:
	dune build $(TARGETS)
	$(dune_install_debug)
	$(dune_install_debug_release)
	$(dune_install_release)

.PHONY: debug
debug:
	$(dune_build_debug)
	$(dune_install_debug)

.PHONY: trace
trace:
	$(dune_build_debug_release)
	$(dune_install_debug_release)

.PHONY: release
release:
	$(dune_build_release)
	$(dune_install_release)

.PHONY: watch
watch:
	dune build --watch --terminal-persistence=clear-on-rebuild $(TARGETS)

.PHONY: test
test:
	-dune build @_build/debug/runtest --auto-promote
	dune build @_build/debug/fmt --auto-promote 2>/dev/null

.PHONY: ci-test
ci-test:
	dune build @_build/debug/runtest

.PHONY: clean
clean:
	dune clean

.PHONY: fmt
fmt:
	-dune build @_build/debug/fmt --auto-promote 2>/dev/null
	tmp=$(mktemp -t 'dune-format'); dune format-dune-file dune-project > tmp; mv -f tmp dune-project
	tmp=$(mktemp -t 'dune-format'); dune format-dune-file dune-workspace > tmp; mv -f tmp dune-workspace
	clang-format -i model/llair_intrinsics.h model/cxxabi.cpp
	${MAKE} -C test fmt

OCAMLFORMAT_EXE = ocamlformat
OCAMLFORMAT_ARGS =

.PHONY: fmt_all
fmt_all:
	parallel $(OCAMLFORMAT_EXE) $(OCAMLFORMAT_ARGS) -i ::: $(shell find * \( -name _build -or -name llvm \) -not -prune -or \( -name '*'.ml -or -name '*'.mli \) -print 2>/dev/null)

# print any variable for Makefile debugging
print-%:
	@printf '$*='; printf '$($*)'; printf '\n'
