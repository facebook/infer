# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..
INFER_OUT ?= infer-out$(TEST_SUFFIX)
INFERPRINT_OPTIONS = --issues-tests

include $(TESTS_DIR)/infer.make

SOURCES = $(sort $(wildcard *.py))

default: test

$(INFER_OUT)/report.json: $(SOURCES) $(INFER_BIN)
	$(INFER_BIN) --quiet --no-progress-bar --debug-exceptions \
		--dump-duplicate-symbols \
		-o $(INFER_OUT) \
                -- python3 $(SOURCES)
