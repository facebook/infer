# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

include $(TESTS_DIR)/base.make

CODETOANALYZE_DIR = ../codetoanalyze

CLANG_OPTIONS = -c
INFER_OPTIONS = --project-root $(CODETOANALYZE_DIR) --fail-on-issue
INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(CODETOANALYZE_DIR)/hello.c
CLEAN_EXTRA = $(SOURCES:$(CODETOANALYZE_DIR)/%.c=%.o)

default: test

issues.exp.test: $(CLANG_DEPS) $(SOURCES)
	$(QUIET)$(call silent_on_success,Testing Infer fails on issue,\
	  exit_code=0; $(INFER_BIN) --fail-on-issue -- clang $(CLANG_OPTIONS) $(SOURCES) || exit_code=$$?; \
	  echo "infer exit code: $$exit_code" > $@)

.PHONY: print
print: issues.exp.test

.PHONY: test
test: issues.exp.test
	$(QUIET)cd $(TESTS_DIR) && \
	$(call check_no_diff,$(TEST_REL_DIR)/issues.exp,$(TEST_REL_DIR)/issues.exp.test)

.PHONY: replace
replace: issues.exp.test
	cp $< issues.exp

.PHONY: clean
clean:
	rm -rf issues.exp.test infer-out $(CLEAN_EXTRA)
