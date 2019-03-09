# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# additional arguments to pass to clang
OPT_ARGS?=-Os
CLANG_ARGS?=-g $(OPT_ARGS)

# select llvm and clang
SWITCH?=$(shell opam switch show)
LLVM?=../llvm/_install/$(SWITCH)

LIBCXXABI=../llvm/projects/libcxxabi

cxxabi.bc : cxxabi.cpp
	$(LLVM)/bin/clang $(CLANG_ARGS) -I$(LLVM)/include/c++/v1 -I$(LIBCXXABI)/include -I$(LIBCXXABI)/src -c -emit-llvm cxxabi.cpp

clean:
	rm cxxabi.bc

fmt:
	clang-format -i *.h *.c *.cpp

# print any variable for Makefile debugging
print-%:
	@printf '$*='; printf '$($*)'; printf '\n'
