# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

CLANG_OPTIONS = -c
INFER_OPTIONS = --project-root ../codetoanalyze
INFERPRINT_OPTIONS = --issues-tests

SOURCES = ../codetoanalyze/hello.c ../codetoanalyze/hello_dup.c

include $(TESTS_DIR)/clang.make

issues.exp.test:
	$(QUIET)cat infer-out$(TEST_SUFFIX)/duplicates.txt > $@
#	delete duplicates file to avoid throwing an error because duplicate symbols were found
	$(QUIET)$(REMOVE) infer-out$(TEST_SUFFIX)/duplicates.txt
