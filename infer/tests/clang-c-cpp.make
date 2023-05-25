# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

SOURCES = $(SOURCES_C) $(SOURCES_CPP)

ROOT_DIR = $(TESTS_DIR)/../..

CLEAN_EXTRA += duplicates.txt
CLEAN_EXTRA += $(foreach source,$(filter %.c %.cpp %.m %.mm,$(SOURCES)),$(basename $(source)).o)

include $(TESTS_DIR)/infer.make
include $(TESTS_DIR)/clang-base.make

infer-out$(TEST_SUFFIX)/report.json: $(CLANG_DEPS) $(SOURCES) $(HEADERS) $(TESTS_DIR)/.inferconfig $(MAKEFILE_LIST)
	$(QUIET)$(call silent_on_success,Testing infer/clang in $(TEST_REL_DIR),\
	  echo $(SOURCES) && \
	  $(INFER_BIN) capture --results-dir $(@D) --dump-duplicate-symbols \
	    $(INFER_OPTIONS) -- clang $(CLANG_OPTIONS_C) $(SOURCES_C) && \
	  $(INFER_BIN) capture --continue --results-dir $(@D) --dump-duplicate-symbols \
	    $(INFER_OPTIONS) -- clang $(CLANG_OPTIONS_CPP) $(SOURCES_CPP) && \
	  $(INFER_BIN) analyze --results-dir $(@D) --dump-duplicate-symbols $(INFER_OPTIONS))
