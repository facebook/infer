# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../..

ANALYZER = infer

CODETOANALYZE_DIR = ../codetoanalyze

CLANG_OPTIONS = -c
INFER_OPTIONS = --project-root $(CODETOANALYZE_DIR) --pmd-xml
INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(CODETOANALYZE_DIR)/hello.c

include $(TESTS_DIR)/clang.make

issues.exp.test: infer-out/report.json
#	grab only a few interesting fields from the xml to prevent flakiness
	$(QUIET)grep --only-matching -e ' \(name\|method\|rule\)="[^"]*"' infer-out/report.xml > $@
