# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

# The "-index-store-path <folder>" argument does not exist in Clang and it gets
# discarded without any error message. The problem is that its argument,
# a folder, is not discarded, and Clang considers it as a source file. This
# leads to an error. This weird case can be observed when -index-store-path is
# passed in a sequence like the one in CLANG_OPTIONS.
CLANG_OPTIONS = -x c -index-store-path . -c -fsanitize=builtin -fsanitize=address,builtin -bogus=arg
INFER_OPTIONS = --report-custom-error --developer-mode --project-root ../codetoanalyze --clang-block-listed-flags-with-arg -bogus
INFERPRINT_OPTIONS = --issues-tests

SOURCES = ../codetoanalyze/hello.c

include $(TESTS_DIR)/clang.make
