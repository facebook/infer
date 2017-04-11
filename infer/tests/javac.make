# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

# Makefiles that include this one must define TESTS_DIR and then include
# $(TESTS_DIR)/javac.make.
#
# Makefiles that include this one must define the ANALYZER and SOURCES
# variables, and may optionally define INFER_OPTIONS, INFERPRINT_OPTIONS,
# CLEAN_EXTRA.

OBJECTS = $(patsubst %.java,%.class,$(SOURCES))

include $(TESTS_DIR)/java.make
include $(TESTS_DIR)/infer.make

PROJECT_ROOT ?= $(TESTS_DIR)

$(OBJECTS): $(SOURCES)
	$(JAVAC) -cp $(CLASSPATH) $(SOURCES)

infer-out/report.json: $(JAVA_DEPS) $(SOURCES)
	$(QUIET)$(call silent_on_success,Testing infer/java$(ANALYZER_STRING) in $(TEST_REL_DIR),\
	  $(INFER_BIN) -a $(ANALYZER) --project-root $(PROJECT_ROOT) $(INFER_OPTIONS) -- \
	    $(JAVAC) -cp $(CLASSPATH) $(SOURCES))
