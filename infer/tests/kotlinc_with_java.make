# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Makefiles that include this one must define TESTS_DIR and then include
# $(TESTS_DIR)/kotlinc_with_java.make.
#
# Makefiles that include this one must define JAVA_SOURCES and KOTLIN_SOURCES variables, and may
# optionally define INFER_OPTIONS, INFERPRINT_OPTIONS, CLEAN_EXTRA.

include $(TESTS_DIR)/infer.make
include $(TESTS_DIR)/java.make

CLEAN_EXTRA = *.class META-INF

PROJECT_ROOT ?= $(TESTS_DIR)

JAVAC_FLAGS = -g -source 8 -target 8

KOTLINC_FLAGS = -nowarn

infer-out$(TEST_SUFFIX)/report.json: $(JAVA_DEPS) $(JAVA_SOURCES) $(KOTLIN_SOURCES) $(MAKEFILE_LIST)
	$(QUIET)$(call silent_on_success,Testing infer/java/kotlin in $(TEST_REL_DIR),\
	  $(INFER_BIN) capture --project-root $(PROJECT_ROOT) --kotlin-capture \
	    -o $(@D) $(INFER_OPTIONS) -- $(KOTLINC) $(KOTLINC_FLAGS) -cp $(CLASSPATH) $(KOTLIN_SOURCES) $(JAVA_SOURCES) && \
	  $(INFER_BIN) capture --continue --project-root $(PROJECT_ROOT) --dump-duplicate-symbols \
	    -o $(@D) $(INFER_OPTIONS) -- $(JAVAC) $(JAVAC_FLAGS) -cp $(CLASSPATH) $(JAVA_SOURCES) \
	    && \
	  $(INFER_BIN) analyze --project-root $(PROJECT_ROOT) -o $(@D) $(INFER_OPTIONS))
