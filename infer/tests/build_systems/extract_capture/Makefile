# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

SOURCES = $(wildcard src/*.c)
CLEAN_EXTRA += *.o infer-out extracted-out source-files.test procedures.test

include $(TESTS_DIR)/base.make

source-files.test: $(INFER_BIN) $(MAKEFILE_LIST) $(SOURCES)
	$(QUIET)$(INFER_BIN) capture --buck -- clang -c $(SOURCES)
	$(QUIET)$(INFER_BIN) debug -o extracted-out --source-files --extract-capture-from infer-out --changed-files-index files.index
	$(QUIET)$(INFER_BIN) debug -o extracted-out --source-files > source-files.test
	$(QUIET)$(INFER_BIN) debug -o extracted-out --procedures > procedures.test

.PHONY: test
test: source-files.test
	$(QUIET)$(call check_no_diff, source-files.test, source-files.exp)
	$(QUIET)$(call check_no_diff, procedures.test, procedures.exp)

.PHONY: replace
replace: source-files.test
	$(COPY) source-files.test source-files.exp
	$(COPY) procedures.test procedures.exp

.PHONY: clean
clean:
	$(REMOVE_DIR) $(CLEAN_EXTRA)
