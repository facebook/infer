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
include $(TESTS_DIR)/base.make

$(OBJECTS): $(SOURCES)
	javac -cp $(CLASSPATH) $(SOURCES)

infer-out/report.json: $(INFER_BIN) $(SOURCES)
	$(call silent_on_success,\
	  $(INFER_BIN) --project-root $(TESTS_DIR) --inferconfig-home . $(INFER_OPTIONS) -a $(ANALYZER) -- javac -cp $(CLASSPATH) $(SOURCES))
