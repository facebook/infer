# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Makefiles that include this one must define TESTS_DIR and then include
# $(TESTS_DIR)/javac.make.
#
# Makefiles that include this one must define the SOURCES variable, and may optionally define
# INFER_OPTIONS, INFERPRINT_OPTIONS, CLEAN_EXTRA.

OBJECTS = $(patsubst %.java,%.class,$(SOURCES))

include $(TESTS_DIR)/java.make
include $(TESTS_DIR)/infer.make

PROJECT_ROOT ?= $(TESTS_DIR)

$(OBJECTS): $(SOURCES)
	$(QUIET)$(JAVAC) -cp $(CLASSPATH) $(SOURCES)

infer-out/report.json: $(JAVA_DEPS) $(SOURCES) $(MAKEFILE_LIST)
	$(QUIET)$(call silent_on_success,Testing infer/java in $(TEST_REL_DIR),\
	  $(INFER_BIN) --project-root $(PROJECT_ROOT) \
	    $(INFER_OPTIONS) -- \
	    $(JAVAC) -cp $(CLASSPATH) $(SOURCES))
