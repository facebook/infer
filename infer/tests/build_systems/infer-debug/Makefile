# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

include $(TESTS_DIR)/base.make

CODETOANALYZE_DIR = ../codetoanalyze

CLANG_OPTIONS = -c
INFER_OPTIONS = --project-root $(CODETOANALYZE_DIR)
INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(CODETOANALYZE_DIR)/hello.c
CLEAN_EXTRA = $(SOURCES:$(CODETOANALYZE_DIR)/%.c=%.o)

default: test

output.exp.test: $(CLANG_DEPS) $(SOURCES) $(MAKEFILE_LIST)
	$(INFER_BIN) $(INFER_OPTIONS) -- clang $(CLANG_OPTIONS) $(SOURCES)
	echo "# infer debug --procedures" > $@
	$(INFER_BIN) debug --procedures >> $@
#	matches "test"
	echo "# infer debug --procedures --procedures-filter 'te.t'" >> $@
	$(INFER_BIN) debug --procedures --procedures-filter 'te.t' >> $@
	echo "# infer debug --procedures --procedures-filter 'hello.c:test'" >> $@
	$(INFER_BIN) debug --procedures --procedures-filter 'te.t' >> $@
#	matches nothing
	echo "# infer debug --procedures --procedures-filter 'nothing matches this'" >> $@
	$(INFER_BIN) debug --procedures --procedures-filter 'nothing matches this' >> $@
	echo "# infer debug --source-files" >> $@
	$(INFER_BIN) debug --source-files >> $@
	echo "# infer debug --source-files --source-files-filter 'he.lo.*'" >> $@
	$(INFER_BIN) debug --source-files  --source-files-filter 'he.lo.*' >> $@
	echo "# infer debug --source-files --source-files-filter 'nothing matches this'" >> $@
	$(INFER_BIN) debug --source-files  --source-files-filter 'nothing matches this' >> $@

.PHONY: print
print: output.exp.test

.PHONY: test
test: output.exp.test
	$(QUIET)cd $(TESTS_DIR) && \
	$(call check_no_diff,$(TEST_REL_DIR)/output.exp,$(TEST_REL_DIR)/output.exp.test)

.PHONY: replace
replace: output.exp.test
	cp $< output.exp

.PHONY: clean
clean:
	rm -rf output.exp.test infer-out $(CLEAN_EXTRA)
