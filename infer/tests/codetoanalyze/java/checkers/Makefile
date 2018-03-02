# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../../..

ANALYZER = checkers
INFER_OPTIONS = \
  --debug-exceptions --no-default-checkers \
  --annotation-reachability --fragment-retains-view --immutable-cast --printf-args --quandary \
  --suggest-nullable --check-nullable --racerd \

INFERPRINT_OPTIONS = --issues-tests
SOURCES = $(wildcard *.java) $(wildcard $(TESTS_DIR)/external/library/*.java)

include $(TESTS_DIR)/javac.make
