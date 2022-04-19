# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

INFER_OPTIONS = --biabduction-only --debug-exceptions
INFERPRINT_OPTIONS = --issues-tests
TEST_CLASSPATH = $(JAVA_BUILTINS_DIR)

SOURCES = A.java B.java
CLEAN_EXTRA = infer-out-A infer-out-B

OBJECTS = $(patsubst %.java,%.class,$(SOURCES))

include $(TESTS_DIR)/java.make
include $(TESTS_DIR)/infer.make

PROJECT_ROOT ?= $(TESTS_DIR)

JAVAC_FLAGS = -g -source 8 -target 8

$(OBJECTS): $(SOURCES)
	$(QUIET)$(JAVAC) $(JAVAC_FLAGS) -cp $(CLASSPATH) $(SOURCES)

infer-out$(TEST_SUFFIX)/report.json: $(JAVA_DEPS) $(SOURCES) $(MAKEFILE_LIST)
	$(QUIET)$(call silent_on_success,Capturing A.java,\
	  $(INFER_BIN) capture --project-root $(PROJECT_ROOT) -o infer-out-A -- $(JAVAC) $(JAVAC_FLAGS) A.java)
	$(QUIET)$(call silent_on_success,Capturing B.java,\
	  $(INFER_BIN) capture --project-root $(PROJECT_ROOT) -o infer-out-B -- $(JAVAC) $(JAVAC_FLAGS) B.java)
	$(QUIET)$(call silent_on_success,Merging infer-outs,\
	  $(INFER_BIN) capture --project-root $(PROJECT_ROOT) --merge-infer-out $(shell realpath infer-out-A) --merge-infer-out $(shell realpath infer-out-B) -o infer-out)
	$(QUIET)$(call silent_on_success,Testing infer/java in $(TEST_REL_DIR),\
	  $(INFER_BIN) analyze $(INFER_OPTIONS))
