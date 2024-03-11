# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../../..

INFER_OPTIONS = --topl-only --topl-properties property.topl --enable-issue-type TOPL_ERROR_LATENT --project-root $(TESTS_DIR)
INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(wildcard *.sil)
CAPTURE_CMD = $(patsubst %.sil, --capture-textual %.sil,$(SOURCES))

default: test

include $(TESTS_DIR)/infer.make

infer-out/report.json: $(SOURCES) $(INFER_BIN)
	$(INFER_BIN) --quiet --no-progress-bar --debug-exceptions --dump-duplicate-symbols\
		$(INFER_OPTIONS) $(CAPTURE_CMD) -o $(INFER_OUT)
