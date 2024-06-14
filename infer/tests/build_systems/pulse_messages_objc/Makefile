# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

CLANG_OPTIONS = -c $(OBJCPP_CLANG_OPTIONS) -fobjc-arc
INFER_OPTIONS = \
  --pulse-only --debug-exceptions --no-pulse-force-continue \
  --pulse-taint-config .infertaintconfig --pulse-taint-skip-sources \
  --project-root $(TESTS_DIR) \
  --report-suppress-errors MUTUAL_RECURSION_CYCLE \

# test that the error messages are good; sort of mimicks what gets displayed on the console
INFERPRINT_OPTIONS = --issues-tests-fields file,procedure,line,column,severity,bug_type,qualifier,suggestion --issues-tests
CODETOANALYZE_DIR = ../../codetoanalyze/objc/pulse/retain_cycles/

SOURCES = $(wildcard *.mm) $(wildcard *.m) $(CODETOANALYZE_DIR)/RetainCycleBlockAsParameter.m \
$(CODETOANALYZE_DIR)/IvarInheritanceRetainCycle.m $(CODETOANALYZE_DIR)/RetainCycleDeduplication.m

include $(TESTS_DIR)/clang.make
include $(TESTS_DIR)/objc.make

infer-out/report.json: $(MAKEFILE_LIST)
