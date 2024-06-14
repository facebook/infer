# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..
INFER_OUT ?= infer-out$(TEST_SUFFIX)
INFERPRINT_OPTIONS = --issues-tests

include $(TESTS_DIR)/infer.make

SOURCES = $(sort $(wildcard *.sil))
CAPTURE_CMD = $(patsubst %.sil, --capture-textual %.sil,$(SOURCES))

default: test

$(INFER_OUT)/report.json: $(SOURCES) $(INFER_BIN)
	$(INFER_BIN) --quiet --no-progress-bar --pulse-only --no-pulse-force-continue \
          --debug-exceptions --dump-duplicate-symbols -o $(INFER_OUT) $(CAPTURE_CMD) \
          --capture-textual $(HACK_LIB_DIR)/models.sil
