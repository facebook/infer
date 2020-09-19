# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

ROOT_DIR = $(TESTS_DIR)/../..

CLEAN_EXTRA += duplicates.txt

OBJECTS = $(foreach source,$(filter %.c %.cpp %.m %.mm,$(SOURCES)),$(basename $(source)).o)

include $(TESTS_DIR)/infer.make
include $(TESTS_DIR)/clang-base.make

infer-out$(TEST_SUFFIX)/report.json: $(CLANG_DEPS) $(SOURCES) $(HEADERS) $(TESTS_DIR)/.inferconfig $(MAKEFILE_LIST)
	$(QUIET)$(call silent_on_success,Testing infer/clang in $(TEST_REL_DIR),\
	  $(INFER_BIN) capture --results-dir $(@D) --dump-duplicate-symbols $(INFER_OPTIONS) \
	    -- clang -fno-objc-arc $(CLANG_OPTIONS) $(SOURCES_NO_ARC) && \
	  $(INFER_BIN) capture --continue --results-dir $(@D) --dump-duplicate-symbols $(INFER_OPTIONS) \
	    -- clang -fobjc-arc $(CLANG_OPTIONS) $(SOURCES_ARC) && \
	  $(INFER_BIN) analyze --results-dir $(@D) --dump-duplicate-symbols $(INFER_OPTIONS))
