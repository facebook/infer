# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR=../../..

SOURCES = $(sort $(wildcard *.hack))
# Hack textual uses Hack-specific builtins that --capture-textual can't parse
PARSE_EXPORTED = no
# Filter out model files which have absolute paths
MANIFEST_FILTER = python3 -c "import json,sys; m=json.load(sys.stdin); json.dump([e for e in m if not e['source'].startswith('/')],sys.stdout,indent=2); print()"

.PHONY: capture
capture: $(SOURCES) $(INFER_BIN) $(HACKC)
	$(QUIET)$(call silent_on_success,Capturing with --store-textual in $(TEST_REL_DIR),\
	  $(REMOVE_DIR) infer-out $(EXPORT_DIR) && \
	  $(INFER_BIN) capture --store-textual --project-root $(TESTS_DIR) \
	    --hackc-binary $(HACKC) -- $(HACKC) compile-infer $(SOURCES))

include $(TESTS_DIR)/export-textual.make
