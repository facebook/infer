# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../..
ROOT_DIR = $(TESTS_DIR)/../..

ANALYZER = infer
BUCK_TARGET = //infer/tests/codetoanalyze/java/infer:compile
SOURCES = $(addprefix $(ROOT_DIR), $(shell buck audit input $(BUCK_TARGET)))
TARGET_OUTPUT = $(word 2, $(shell buck targets --show-output $(BUCK_TARGET)))
OBJECTS = $(ROOT_DIR)/$(TARGET_OUTPUT)

INFERPRINT_OPTIONS = --project-root $(ROOT_DIR) --issues-tests
CLEAN_EXTRA = $(ROOT_DIR)/buck-out

include $(TESTS_DIR)/java.make
include $(TESTS_DIR)/base.make

$(OBJECTS): $(JAVA_SOURCE_FILES)
	cd $(ROOT_DIR) && \
	$(call silent_on_success,\
	INFER_BIN=$(INFER_BIN) NO_BUCKD=1 \
	buck build --no-cache $(BUCK_TARGET))

infer-out/report.json: $(JAVA_DEPS) $(JAVA_SOURCE_FILES)
	cd $(ROOT_DIR) && \
	$(call silent_on_success,\
	INFER_BIN=$(INFER_BIN) NO_BUCKD=1 \
	$(INFER_BIN) -a $(ANALYZER) --results-dir $(CURDIR)/infer-out -- \
	  buck build --no-cache $(BUCK_TARGET))
