# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

.PHONY: default
default: exes

RELEASE_TARGETS:=_build/release/cli/sledge_cli.exe _build/release/sledge.install _build/release/report/sledge_report.exe _build/release/sledge-help.txt
DEBUG_TARGETS:=$(subst release,debug,$(RELEASE_TARGETS))
TRACE_TARGETS:=$(subst release,trace,$(RELEASE_TARGETS))

TARGETS:=$(DEBUG_TARGETS) $(TRACE_TARGETS) $(RELEASE_TARGETS)

dune_install_release:=dune install --context=release --prefix=_build/_install/release sledge 2>/dev/null
dune_install_trace:=$(subst release,trace,$(dune_install_release))
dune_install_debug:=$(subst release,debug,$(dune_install_release))

export PATH:=../facebook/dependencies/bin:$(PATH)

LLVM_OCAML_SRC:=$(shell git ls-files -- vendor/llvm-dune)

# file to use as a sentinel indicating llvm ocaml bindings are up-to-date
LLVM_OCAML_SENTINEL=vendor/llvm-dune/src/llvm/common/dune

# Note that llvm-config is broken and does not correctly detect llvm
# shared libraries. A filthy workaround seems to be to create the
# libLLVM-12.dylib file llvm-config looks for as a symbolic link to
# libLLVM.dylib.
link_llvm_dylib:
	@set -e ;\
	LLVMLIB=$$($$(opam var conf-llvm:config) --libdir) ;\
	ln -s $$LLVMLIB/libLLVM.dylib $$LLVMLIB/libLLVM-12.dylib

$(LLVM_OCAML_SENTINEL): $(LLVM_OCAML_SRC)
	cd vendor/llvm-dune; ./setup.sh $$(opam var conf-llvm:config)

clean_llvm:
	rm -rf vendor/llvm-dune/{src,llvm_*.opam}

.PHONY: setup
setup: $(LLVM_OCAML_SENTINEL)

.PHONY: check
check: setup
	OCAML_ERROR_STYLE=short dune build @check --display=quiet

.PHONY: exes
exes: setup
	dune build $(TARGETS)
	$(dune_install_debug)
	$(dune_install_trace)
	$(dune_install_release)

.PHONY: debug
debug: setup
	dune build $(DEBUG_TARGETS)
	$(dune_install_debug)

.PHONY: trace
trace: setup
	dune build $(TRACE_TARGETS)
	$(dune_install_trace)

.PHONY: release
release: setup
	dune build $(RELEASE_TARGETS)
	$(dune_install_release)

.PHONY: watch
watch: setup
	dune build --watch --terminal-persistence=clear-on-rebuild $(TARGETS)

.PHONY: watch-test
watch-test: setup
	dune build --watch @_build/debug/runtest @_build/debug/fmt --auto-promote $(DEBUG_TARGETS)

.PHONY: test
test:
	-dune build @_build/debug/runtest --auto-promote
	dune build @_build/debug/fmt --auto-promote 2>/dev/null

.PHONY: ci-test
ci-test: exes
	dune build @_build/debug/fmt @_build/debug/runtest
	$(MAKE) --no-print-directory -C test ci-test

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

OCAMLFORMAT_EXE:=ocamlformat
OCAMLFORMAT_ARGS=

.PHONY: fmt_all
fmt_all:
	parallel $(OCAMLFORMAT_EXE) $(OCAMLFORMAT_ARGS) -i ::: $(shell find * \( -name _build -or -name vendor \) -prune -or \( -name '*'.ml -or -name '*'.mli \) -print 2>/dev/null)

# print any variable for Makefile debugging
print-%:
	@printf '$*='; printf '$($*)'; printf '\n'
