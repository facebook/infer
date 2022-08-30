# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
include $(TESTS_DIR)/infer.make

CLEAN_EXTRA += _build

infer-out$(TEST_SUFFIX)/report.json: $(SOURCES) $(TESTS_DIR)/.inferconfig $(INFER_BIN) $(MAKEFILE_LIST)
	$(QUIET) mkdir _build && $(call silent_on_success,Testing infer/erlang in $(TEST_REL_DIR),\
	  $(INFER_BIN) --results-dir $(@D) --dump-duplicate-symbols \
	    $(INFER_OPTIONS) -- erlc -o _build $(SOURCES))
