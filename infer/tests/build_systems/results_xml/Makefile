# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

CODETOANALYZE_DIR = ../codetoanalyze

CLANG_OPTIONS = -c
INFER_OPTIONS = --project-root $(CODETOANALYZE_DIR) --pmd-xml
INFERPRINT_OPTIONS = --issues-tests
SOURCES = $(CODETOANALYZE_DIR)/hello.c

include $(TESTS_DIR)/clang.make

issues.exp.test: infer-out/report.json
#      massage the first line of the XML to delete non-reproducible information (the current date)
	$(QUIET)sed -e 's/date="[^"]*"/date=""/' infer-out/report.xml > $@

