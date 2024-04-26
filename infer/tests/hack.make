# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

ROOT_DIR = $(TESTS_DIR)/../..

CLEAN_EXTRA += duplicates.txt

include $(TESTS_DIR)/infer.make

HH_AUTO_NAMESPACE_MAP = '{ \
  "C": "HH\\Lib\\C", \
  "Dict": "HH\\Lib\\Dict", \
  "Vec": "HH\\Lib\\Vec" \
}'
HH_OPTIONS = --auto-namespace-map=$(HH_AUTO_NAMESPACE_MAP)

infer-out$(TEST_SUFFIX)/report.json: $(SOURCES) $(HH_SOURCES) $(INFER_BIN) $(HACKC) $(MAKEFILE_LIST)
ifneq ($(HH_SINGLE_TYPE_CHECK),no)
	$(QUIET)$(call silent_on_success,Type checking infer/hack in $(TEST_REL_DIR),\
	  $(HH_SINGLE_TYPE_CHECK) $(HH_OPTIONS) $(HH_SOURCES) $(SOURCES))
endif
	$(QUIET)$(call silent_on_success,Testing infer/hack in $(TEST_REL_DIR),\
	  $(INFER_BIN) --results-dir $(@D) --dump-duplicate-symbols --hackc-binary $(HACKC) \
	    $(INFER_OPTIONS) -- $(HACKC) compile-infer $(SOURCES))
