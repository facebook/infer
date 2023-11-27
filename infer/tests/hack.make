# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

ROOT_DIR = $(TESTS_DIR)/../..

CLEAN_EXTRA += duplicates.txt

include $(TESTS_DIR)/infer.make

infer-out$(TEST_SUFFIX)/report.json: $(SOURCES) $(INFER_BIN) $(HACKC) $(MAKEFILE_LIST)
	$(QUIET)$(call silent_on_success,Testing infer/hack in $(TEST_REL_DIR),\
	  $(INFER_BIN) --results-dir $(@D) --dump-duplicate-symbols --hackc-binary $(HACKC) \
	    $(INFER_OPTIONS) -- hackc compile-infer $(SOURCES))
