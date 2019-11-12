# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# E2E test for differential of costs

TESTS_DIR = ../..
SOURCES = src/DiffExample.java.current src/DiffExample.java.previous src/DiffExampleColdStart.java.current src/DiffExampleColdStart.java.previous  src/DiffExampleUIThread.java.current src/DiffExampleUIThread.java.previous
CLEAN_EXTRA = src/Diff*.java *.class
include $(TESTS_DIR)/differential.make
include $(TESTS_DIR)/java.make
INFERPRINT_ISSUES_FIELDS = \
	"bug_type,bucket,file,procedure,line_offset,bug_trace"

$(CURRENT_REPORT) $(PREVIOUS_REPORT): $(JAVA_DEPS)

$(CURRENT_REPORT):
	$(QUIET)$(COPY) src/DiffExample.java.current src/DiffExample.java
	$(QUIET)$(COPY) src/DiffExampleColdStart.java.current src/DiffExampleColdStart.java
	$(QUIET)$(COPY) src/DiffExampleUIThread.java.current src/DiffExampleUIThread.java
	$(QUIET)$(call silent_on_success,Testing Cost Differential: current,\
	$(INFER_BIN) --enable-issue-type INFINITE_EXECUTION_TIME --cost-only -o $(CURRENT_DIR) -- $(JAVAC) -cp $(CLASSPATH)  src/*.java)

$(PREVIOUS_REPORT):
	$(QUIET)$(COPY) src/DiffExample.java.previous src/DiffExample.java
	$(QUIET)$(COPY) src/DiffExampleColdStart.java.previous src/DiffExampleColdStart.java
	$(QUIET)$(COPY) src/DiffExampleUIThread.java.previous src/DiffExampleUIThread.java
	$(QUIET)$(call silent_on_success,Testing Cost Differential: previous,\
	$(INFER_BIN) --debug --enable-issue-type INFINITE_EXECUTION_TIME --cost-only -o $(PREVIOUS_DIR) -- $(JAVAC)  -cp $(CLASSPATH) src/*.java)
