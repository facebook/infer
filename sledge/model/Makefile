# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# additional arguments to pass to clang
OPT_ARGS?=-Os
CLANG_ARGS?=-g $(OPT_ARGS)

ROOT?=..
LIBCXXABI:=$(ROOT)/vendor/llvm-dune/llvm-project/libcxxabi

llvm-config:=$(shell opam var conf-llvm:config)
LLVM_BIN:=$(shell ${llvm-config} --bindir)
LLVM_INCLUDE:=$(shell ${llvm-config} --includedir)
LLVM_CFLAGS:=$(shell ${llvm-config} --cflags)
LLVM_CXXFLAGS:=$(shell ${llvm-config} --cxxflags)

default: cxxabi.bc lib_fuzzer_main.bc

cxxabi.bc : cxxabi.cpp
	$(LLVM_BIN)/clang $(LLVM_CXXFLAGS) $(CLANG_ARGS) -D_LIBCXXABI_HAS_NO_THREADS -I$(LIBCXXABI)/include -I$(LIBCXXABI)/src -c -emit-llvm cxxabi.cpp

lib_fuzzer_main.bc : lib_fuzzer_main.c
	$(LLVM_BIN)/clang $(LLVM_CFLAGS) $(CLANG_ARGS) -c -emit-llvm -o $@ $<

clean:
	rm -f cxxabi.bc lib_fuzzer_main.bc

fmt:
	clang-format -i *.h *.c *.cpp

# print any variable for Makefile debugging
print-%:
	@printf '$*='; printf '$($*)'; printf '\n'
