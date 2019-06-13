# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

ROOT_DIR = ../../../..

include $(ROOT_DIR)/Makefile.config

LEVEL1_DIR = level1
LEVEL2_DIR = $(LEVEL1_DIR)/level2
LEVEL3_DIR = $(LEVEL2_DIR)/level3

LEVELS = LEVEL1 LEVEL2 LEVEL3
TESTS = $(LEVELS:%=%_TEST)
PRINTS = $(LEVELS:%=%_PRINT)
CLEANS = $(LEVELS:%=%_CLEAN)
REPLACES = $(LEVELS:%=%_REPLACE)

.PHONY: default
default: print

.PHONY: $(TESTS)
$(TESTS): %_TEST:
	$(QUIET)$(MAKE) -C $($*_DIR) test

.PHONY: test
test: $(TESTS)

.PHONY: $(PRINTS)
$(PRINTS): %_PRINT:
	$(QUIET)$(MAKE) -C $($*_DIR) print

.PHONY: print
print: $(PRINTS)

.PHONY: $(CLEANS)
$(CLEANS): %_CLEAN:
	$(QUIET)$(MAKE) -C $($*_DIR) clean
	$(QUIET)$(REMOVE) $($*_DIR)/*.o

.PHONY: clean
clean: $(CLEANS)

.PHONY: $(REPLACES)
$(REPLACES): %_REPLACE:
	$(QUIET)$(MAKE) -C $($*_DIR) replace

.PHONY: replace
replace: $(REPLACES)
