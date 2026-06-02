# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

SOURCES = $(sort $(wildcard *.toy))

INFER_OPTIONS = \
  --pulse-only --debug-exceptions --project-root $(TESTS_DIR) \
  --pulse-report-issues-for-tests --no-pulse-force-continue \
  --dump-duplicate-symbols --jobs 1

INFERPRINT_OPTIONS = --issues-tests

include $(TESTS_DIR)/infer.make

infer-out/report.json: $(SOURCES) $(INFER_BIN)
	$(QUIET)$(call silent_on_success,Capturing and analyzing ToyLang in $(TEST_REL_DIR),\
	  $(REMOVE_DIR) infer-out && \
	  $(INFER_BIN) \
	    $(foreach src,$(SOURCES),--capture-toylang $(CURDIR)/$(src)) \
	    $(INFER_OPTIONS))
