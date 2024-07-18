# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..
ROOT_DIR = $(TESTS_DIR)/../..

SOURCES_DIR = $(ROOT_DIR)/examples/java_hello
SOURCES = $(wildcard $(SOURCES_DIR)/*.java)
INFERPRINT_OPTIONS = --issues-tests
TEST_CLASSPATH = $(JAVA_BUILTINS_DIR)

include $(TESTS_DIR)/java.make
include $(TESTS_DIR)/infer.make

infer-out/report.json: $(JAVA_DEPS) $(SOURCES) $(MAKEFILE_LIST)
#	mock version of gradle
	$(QUIET)PATH=$(CURDIR)/../mock:"$$PATH"; \
	cd $(SOURCES_DIR) && \
	$(call silent_on_success,Testing gradle Java integration,\
	  $(INFER_BIN) --pulse-only --results-dir $(CURDIR)/infer-out -- gradle build)
