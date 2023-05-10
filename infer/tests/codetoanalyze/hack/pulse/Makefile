# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..
INFER_OUT ?= infer-out$(TEST_SUFFIX)
INFERPRINT_OPTIONS = --issues-tests

include $(TESTS_DIR)/infer.make

SOURCES = $(sort $(wildcard *.hack))

default: test

$(INFER_OUT)/report.json: $(SOURCES) $(INFER_BIN) $(HACKC)
	$(INFER_BIN) --quiet --no-progress-bar --pulse-only --debug-exceptions\
		--dump-duplicate-symbols -o $(INFER_OUT) \
                --hack-models ../../sil/pulse/models/models.sil \
                --hackc-binary $(HACKC) -- hackc compile-infer $(SOURCES)
