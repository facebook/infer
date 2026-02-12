# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..
INFER_OUT ?= infer-out$(TEST_SUFFIX)
INFERPRINT_OPTIONS = --issues-tests

include $(TESTS_DIR)/infer.make

SOURCES = $(sort $(wildcard *.ullbc))
RUSTSOURCES = $(realpath $(wildcard *.rs))
CAPTURE_CMD = $(patsubst %.rs.ullbc, --capture-rust-ullbc %.rs.ullbc,$(SOURCES))

default: test

$(INFER_OUT)/report.json: $(SOURCES) $(INFER_BIN)
	$(INFER_BIN) --quiet --no-progress-bar --debug-exceptions \
		--dump-duplicate-symbols \
		-o $(INFER_OUT) $(CAPTURE_CMD)

# (Re)generate the ullbc and print_ullbc files for all rust programs in the folder
.PHONY: charon
charon:
	$(QUIET)if [ "$(BUILD_RUST_ANALYZERS)" = "yes" ]; then \
	error=0; for file in $(RUSTSOURCES) ; do \
		charon rustc --print-ullbc --ullbc --dest-file $$file.ullbc -- $$file --allow unused > $$file.print_ullbc || error=1 ; \
	done; fi
