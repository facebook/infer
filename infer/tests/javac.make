# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Makefiles that include this one must define TESTS_DIR and then include
# $(TESTS_DIR)/javac.make.
#
# Makefiles that include this one must define the SOURCES variable, and may optionally define
# INFER_OPTIONS, INFERPRINT_OPTIONS, CLEAN_EXTRA.

include $(TESTS_DIR)/java.make
include $(TESTS_DIR)/infer.make

CLEAN_EXTRA = *.class META-INF

PROJECT_ROOT ?= $(TESTS_DIR)

JAVAC_FLAGS = -g -source 8 -target 8

infer-out$(TEST_SUFFIX)/report.json: $(JAVA_DEPS) $(SOURCES) $(MAKEFILE_LIST)
	$(QUIET)$(call silent_on_success,Testing infer/java in $(TEST_REL_DIR),\
	  $(INFER_BIN) --project-root $(PROJECT_ROOT) --dump-duplicate-symbols \
	    -o $(@D) $(INFER_OPTIONS) -- \
	    $(JAVAC) $(JAVAC_FLAGS) -cp $(CLASSPATH) $(SOURCES))
