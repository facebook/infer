# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Makefiles that include this one must define TESTS_DIR and then include
# $(TESTS_DIR)/kotlinc.make.
#
# Makefiles that include this one must define the SOURCES variable, and may optionally define
# INFER_OPTIONS, INFERPRINT_OPTIONS, CLEAN_EXTRA.

OBJECTS = $(patsubst %.kt,%.class,$(SOURCES))
SOURCES_ARGS = $(patsubst %,--sources %,$(SOURCES))

$(OBJECTS): $(SOURCES)
	$(QUIET)$(call silent_on_success,Compile Kotlin code,$(KOTLINC) -cp $(CLASSPATH) $(SOURCES))

include $(TESTS_DIR)/infer.make
include $(TESTS_DIR)/java.make

infer-out$(TEST_SUFFIX)/report.json: $(OBJECTS) $(MAKEFILE_LIST)
	$(QUIET)$(call silent_on_success,Testing infer/kotlin in $(TEST_REL_DIR),\
	  $(INFER_BIN) --kotlin-capture --generated-classes . $(SOURCES_ARGS) -o $(@D) $(INFER_OPTIONS))
