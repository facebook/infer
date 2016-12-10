# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

CURDIR = $(shell pwd)

TESTS_DIR = ../..
ROOT_DIR = $(TESTS_DIR)/../..

ANALYZER = infer
SOURCES = $(wildcard $(TESTS_DIR)/codetoanalyze/java/infer/*.java)
OBJECTS = $(ROOT_DIR)/buck-out/gen/infer/tests/codetoanalyze/java/infer/lib__compile__output/compile.jar
INFERPRINT_OPTIONS = --project-root $(ROOT_DIR) --issues-tests
CLEAN_EXTRA = $(ROOT_DIR)/buck-out/ $(CURDIR)/buck-out/

include $(TESTS_DIR)/java.make
include $(TESTS_DIR)/base.make

$(JAR_OUTPUT): $(JAVA_SOURCE_FILES)
	cd $(ROOT_DIR) && $(call silent_on_success, INFER_BIN=$(INFER_BIN) NO_BUCKD=1 buck build --no-cache //infer/tests/codetoanalyze/java/infer:compile)

.PHONY: genrule
genrule: $(JAR_OUTPUT)
	cd $(ROOT_DIR) && $(call silent_on_success, INFER_BIN=$(INFER_BIN) NO_BUCKD=1 GENERATE_INFER_GENRULES=1 buck build --no-cache //infer/tests/build_systems/genrule/module2:module2_infer)

infer-out/report.json: genrule $(INFER_BIN) $(JAVA_SOURCE_FILES)
	cd $(ROOT_DIR) && $(call silent_on_success, INFER_BIN=$(INFER_BIN) NO_BUCKD=1 $(INFER_BIN) -a $(ANALYZER) --results-dir $(CURDIR)/infer-out  -- buck build --no-cache //infer/tests/codetoanalyze/java/infer:compile)
