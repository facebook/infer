# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# E2E test for differential of costs

TESTS_DIR = ../..
SOURCES = src/DiffExample.current.java src/DiffExample.previous.java src/DiffExampleUIThread.current.java src/DiffExampleUIThread.previous.java
COPIED = src/DiffExample.java src/DiffExampleUIThread.java
CLEAN_EXTRA = $(COPIED) *.class
include $(TESTS_DIR)/differential.make
include $(TESTS_DIR)/java.make
INFERPRINT_ISSUES_FIELDS = \
	"bug_type,bucket,file,procedure,line_offset,bug_trace"

$(CURRENT_REPORT) $(PREVIOUS_REPORT): $(JAVA_DEPS)

$(CURRENT_REPORT):
	$(QUIET)$(COPY) src/DiffExample.current.java src/DiffExample.java
	$(QUIET)$(COPY) src/DiffExampleUIThread.current.java src/DiffExampleUIThread.java
	$(QUIET)$(call silent_on_success,Testing Cost Differential: current,\
	$(INFER_BIN) --no-filtering --cost-only --no-cost-suppress-func-ptr -o $(CURRENT_DIR) \
	  -- $(JAVAC) -cp $(CLASSPATH)  $(COPIED))

$(PREVIOUS_REPORT):
	$(QUIET)$(COPY) src/DiffExample.previous.java src/DiffExample.java
	$(QUIET)$(COPY) src/DiffExampleUIThread.previous.java src/DiffExampleUIThread.java
	$(QUIET)$(call silent_on_success,Testing Cost Differential: previous,\
	$(INFER_BIN) --debug --no-filtering --cost-only --no-cost-suppress-func-ptr -o $(PREVIOUS_DIR) \
	  -- $(JAVAC) -cp $(CLASSPATH) $(COPIED))
