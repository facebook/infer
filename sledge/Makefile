# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

.PHONY: default
default: exes

EXES = bin/sledge_cli
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
	clang-format -i model/llair_intrinsics.h model/cxxabi.cpp
	${MAKE} -C test fmt

# print any variable for Makefile debugging
print-%:
	@printf '$*='; printf '$($*)'; printf '\n'
