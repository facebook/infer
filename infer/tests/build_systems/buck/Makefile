# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../..
ROOT_DIR = $(TESTS_DIR)/../..

ANALYZER = infer
BUCK_TARGET = //infer/tests/build_systems/genrule/module2:module2
SOURCES = $(wildcard $(TESTS_DIR)/codetoanalyze/java/infer/*.java)
OBJECTS = $(ROOT_DIR)/buck-out/gen/infer/tests/build_systems/genrule/module2/lib__module2__output/module2.jar
INFERPRINT_OPTIONS = --project-root $(ROOT_DIR) --issues-tests
CLEAN_EXTRA = $(ROOT_DIR)/buck-out

include $(TESTS_DIR)/java.make
include $(TESTS_DIR)/infer.make

$(OBJECTS): $(JAVA_SOURCE_FILES)
	$(QUIET)cd $(ROOT_DIR) && \
	$(call silent_on_success,Compiling Buck Java tests,\
	INFER_BIN=$(INFER_BIN) NO_BUCKD=1 \
	$(BUCK) build --no-cache $(BUCK_TARGET))

infer-out/report.json: $(JAVA_DEPS) $(JAVA_SOURCE_FILES)
	$(QUIET)cd $(ROOT_DIR) && \
	$(REMOVE_DIR) buck-out && \
	$(call silent_on_success,Testing Buck Java integration,\
	INFER_BIN=$(INFER_BIN) NO_BUCKD=1 \
	$(INFER_BIN) -a $(ANALYZER) --results-dir $(CURDIR)/infer-out -- \
	  $(BUCK) build --no-cache $(BUCK_TARGET))
