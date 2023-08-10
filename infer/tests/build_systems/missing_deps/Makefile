# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..
INFERPRINT_OPTIONS= --issues-tests
CLEAN_EXTRA = *.o *-out *.index

include $(TESTS_DIR)/infer.make

export INFER_BIN

infer-out$(TEST_SUFFIX)/report.json: $(SOURCES) $(MAKEFILE_LIST)
	$(MAKE) clean
	$(QUIET)$(call silent_on_success,Testing distributed analysis in $(TEST_REL_DIR), ./dist.py)
