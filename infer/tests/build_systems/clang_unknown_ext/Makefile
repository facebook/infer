# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../..

ANALYZER = infer

CLANG_OPTIONS = -x c -c
INFER_OPTIONS = --project-root ../codetoanalyze --no-failures-allowed
INFERPRINT_OPTIONS = --issues-tests

SOURCES = ../codetoanalyze/hello.unknown_ext

include $(TESTS_DIR)/clang.make

../codetoanalyze/hello.o: ../codetoanalyze/hello.unknown_ext
	clang $(CLANG_OPTIONS) $< > $@
