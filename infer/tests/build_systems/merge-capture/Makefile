# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

INFER_OPTIONS = --pulse-only --debug-exceptions
INFERPRINT_OPTIONS = --issues-tests
TEST_CLASSPATH = $(JAVA_BUILTINS_DIR)

SOURCES = A.java B.java
ARGSFILE = argsfile
CLEAN_EXTRA = infer-out-A infer-out-B $(ARGSFILE)

include $(TESTS_DIR)/java.make
include $(TESTS_DIR)/infer.make

PROJECT_ROOT ?= $(TESTS_DIR)

JAVAC_FLAGS = -g -source 8 -target 8

infer-out$(TEST_SUFFIX)/report.json: $(JAVA_DEPS) $(SOURCES) $(MAKEFILE_LIST)
	$(QUIET)$(call silent_on_success,Capturing A.java,\
	  $(INFER_BIN) capture --buck --project-root $(PROJECT_ROOT) -o infer-out-A -- $(JAVAC) $(JAVAC_FLAGS) A.java)
	$(QUIET)$(call silent_on_success,Capturing B.java,\
	  $(INFER_BIN) capture --buck --project-root $(PROJECT_ROOT) -o infer-out-B -- $(JAVAC) $(JAVAC_FLAGS) B.java)
	$(QUIET)echo $$(realpath infer-out-B) > $(ARGSFILE)
	$(QUIET)$(call silent_on_success,Merging infer-outs,\
	  $(INFER_BIN) capture --project-root $(PROJECT_ROOT) --merge-capture $$(realpath infer-out-A) --merge-capture @$(ARGSFILE) -o infer-out)
	$(QUIET)$(call silent_on_success,Testing infer/java in $(TEST_REL_DIR),\
	  $(INFER_BIN) analyze $(INFER_OPTIONS))
	$(QUIET)rm $(ARGSFILE)
