# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# additional arguments to pass to clang
OPT_ARGS?=-Os
CLANG_ARGS?=-g $(OPT_ARGS)

# select llvm and clang
SWITCH?=$(shell opam switch show)
ROOT?=..
LLVM=$(ROOT)/.llvm_install/$(SWITCH)

LIBCXXABI=$(ROOT)/llvm/projects/libcxxabi

PLATFORM=$(shell uname)
ifeq ($(PLATFORM),Linux)
CLANG_ARGS += --sysroot=/usr
endif

cxxabi.bc : cxxabi.cpp
	$(LLVM)/bin/clang $(CLANG_ARGS) -D_LIBCXXABI_HAS_NO_THREADS -I$(LLVM)/include/c++/v1 -I$(LIBCXXABI)/include -I$(LIBCXXABI)/src -c -emit-llvm cxxabi.cpp

lib_fuzzer_main.bc : lib_fuzzer_main.c
	$(LLVM)/bin/clang $(CLANG_ARGS) -c -emit-llvm -o $@ $<

clean:
	rm -f cxxabi.bc lib_fuzzer_main.bc

fmt:
	clang-format -i *.h *.c *.cpp

# print any variable for Makefile debugging
print-%:
	@printf '$*='; printf '$($*)'; printf '\n'
