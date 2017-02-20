# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

ROOT_DIR = $(TESTS_DIR)/../..

CLEAN_EXTRA += duplicates.txt

OBJECTS = $(foreach source,$(SOURCES),$(basename $(source)).o)

include $(TESTS_DIR)/base.make
include $(TESTS_DIR)/clang-base.make

infer-out$(TEST_SUFFIX)/report.json: $(CLANG_DEPS) $(SOURCES) $(HEADERS) $(TESTS_DIR)/.inferconfig
	$(call silent_on_success,\
	  $(INFER_BIN) --results-dir $(@D) --check-duplicate-symbols \
	    $(INFER_OPTIONS) -a $(ANALYZER) -- \
	    clang $(CLANG_OPTIONS) $(SOURCES) 2> \
	    >(tee duplicates.txt))
	grep "DUPLICATE_SYMBOLS" duplicates.txt; test $$? -ne 0
