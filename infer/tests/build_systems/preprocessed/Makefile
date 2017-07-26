# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../..

ANALYZER = infer
CLANG_OPTIONS = -c
INFER_OPTIONS = --report-custom-error --developer-mode --headers --project-root ../codetoanalyze --no-keep-going
INFERPRINT_OPTIONS = --issues-tests

SOURCES = \
  ../codetoanalyze/preprocessed.c

include $(TESTS_DIR)/clang.make
