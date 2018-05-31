# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

ANALYZER = checkers

CLANG_OPTIONS = -x c -c
INFER_OPTIONS = --project-root ../codetoanalyze
INFERPRINT_OPTIONS = --issues-tests

SOURCES = ../codetoanalyze/hello.unknown_ext

include $(TESTS_DIR)/clang.make

../codetoanalyze/hello.o: ../codetoanalyze/hello.unknown_ext
	clang $(CLANG_OPTIONS) $< > $@
