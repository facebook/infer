# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR=../../..

SOURCES = $(sort $(wildcard *.c))

ifdef LLVM_CONFIG
	CLANG_BIN=$(shell dirname $(LLVM_CONFIG))/clang
else
	CLANG_BIN=$(CLANG_PREFIX)/bin/clang
endif

# LLVM textual uses builtins that --capture-textual can't parse
PARSE_EXPORTED = no

.PHONY: capture
capture: $(SOURCES) $(INFER_BIN) $(MAKEFILE_LIST)
	$(QUIET)$(call silent_on_success,Capturing with --store-textual in $(TEST_REL_DIR),\
	  $(REMOVE_DIR) infer-out $(EXPORT_DIR) && \
	  $(foreach src,$(SOURCES),\
	    $(INFER_BIN) capture --bitcode-capture --store-textual --reactive \
	      --project-root $(TESTS_DIR) -- $(CLANG_BIN) -g -c $(src) -o $(src:.c=.o) &&) \
	  true)

include $(TESTS_DIR)/export-textual.make
