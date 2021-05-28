# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# E2E test for differential of costs

TESTS_DIR = ../..
SOURCES = src/DiffExample.current.java src/DiffExample.previous.java \
  src/DiffAnonymousClass.current.java src/DiffAnonymousClass.previous.java
COPIED = src/DiffExample.java src/DiffAnonymousClass.java
CLEAN_EXTRA = $(COPIED) *.class
DIFFERENTIAL_ARGS = --enable-issue-type CONFIG_IMPACT
include $(TESTS_DIR)/differential-config-impact.make
include $(TESTS_DIR)/java.make

$(CURRENT_REPORT) $(PREVIOUS_REPORT): $(JAVA_DEPS)

$(CURRENT_REPORT):
	$(QUIET)$(COPY) src/DiffExample.current.java src/DiffExample.java
	$(QUIET)$(COPY) src/DiffAnonymousClass.current.java src/DiffAnonymousClass.java
	$(QUIET)$(call silent_on_success,Testing Config Impact Differential: current,\
	$(INFER_BIN) --no-filtering --config-impact-analysis-only --config-impact-data-file config_data.json -o $(CURRENT_DIR) \
	  -- $(JAVAC) -cp $(CLASSPATH)  $(COPIED))

$(PREVIOUS_REPORT):
	$(QUIET)$(COPY) src/DiffExample.previous.java src/DiffExample.java
	$(QUIET)$(COPY) src/DiffAnonymousClass.previous.java src/DiffAnonymousClass.java
	$(QUIET)$(call silent_on_success,Testing Config Impact Differential: previous,\
	$(INFER_BIN) --no-filtering --config-impact-analysis-only --config-impact-data-file config_data.json -o $(PREVIOUS_DIR) \
	  -- $(JAVAC) -cp $(CLASSPATH) $(COPIED))
