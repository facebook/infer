# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Makefiles that include this one must define TESTS_DIR and then include
# $(TESTS_DIR)/kotlinc_with_java.make.
#
# Makefiles that include this one must define JAVA_SOURCES and KOTLIN_SOURCES variables, and may
# optionally define INFER_OPTIONS, INFERPRINT_OPTIONS, CLEAN_EXTRA.

JAVA_OBJECTS = $(patsubst %.java,%.class,$(JAVA_SOURCES))
OBJECTS = $(JAVA_OBJECTS)
SOURCES_ARGS = $(patsubst %,--sources %,$(KOTLIN_SOURCES))

include $(TESTS_DIR)/infer.make
include $(TESTS_DIR)/java.make

PROJECT_ROOT ?= $(TESTS_DIR)

JAVAC_FLAGS = -g -source 8 -target 8

.PHONY: kotlin_objects
kotlin_objects: $(JAVA_SOURCES) $(KOTLIN_SOURCES)
	$(QUIET)$(call silent_on_success,Compile Kotlin code, \
	  $(KOTLINC) -cp $(CLASSPATH) $(JAVA_SOURCES) $(KOTLIN_SOURCES))

$(OBJECTS): $(JAVA_SOURCES) kotlin_objects
	$(QUIET)$(call silent_on_success,Compile Java code, \
	  $(JAVAC) $(JAVAC_FLAGS) -cp $(CLASSPATH) $(JAVA_SOURCES))

infer-out$(TEST_SUFFIX)/report.json: $(JAVA_DEPS) $(JAVA_SOURCES) kotlin_objects $(MAKEFILE_LIST)
	$(QUIET)$(call silent_on_success,Testing infer/java/kotlin in $(TEST_REL_DIR),\
	  $(INFER_BIN) capture --project-root $(PROJECT_ROOT) --kotlin-capture \
	    --generated-classes . $(SOURCES_ARGS) -o $(@D) $(INFER_OPTIONS) && \
	  $(INFER_BIN) capture --continue --project-root $(PROJECT_ROOT) --dump-duplicate-symbols \
	    -o $(@D) $(INFER_OPTIONS) -- $(JAVAC) $(JAVAC_FLAGS) -cp $(CLASSPATH) $(JAVA_SOURCES) \
	    && \
	  $(INFER_BIN) analyze -o $(@D) $(INFER_OPTIONS))
