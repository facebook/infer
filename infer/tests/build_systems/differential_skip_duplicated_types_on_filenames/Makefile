# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# E2E test involving the skip_duplicated_types_on_filenames filter

TESTS_DIR = ../..
SOURCES = $(wildcard src/*.java.current src/*.java.previous)
CLEAN_EXTRA = src/Diff*.java src/Diff*.java *.class

include $(TESTS_DIR)/differential.make

$(CURRENT_REPORT) $(PREVIOUS_REPORT): $(JAVA_DEPS)

$(CURRENT_REPORT):
	$(QUIET)$(COPY) src/DiffExample.java.current src/DiffExample.java
	$(QUIET)$(COPY) src/DiffExampleTwo.java.current src/DiffExampleTwo.java
	$(QUIET)$(call silent_on_success,Testing\
	Differential skips duplicated types on filenames: current,\
	$(INFER_BIN) --pulse-only -o $(CURRENT_DIR) -- $(JAVAC) src/DiffExample.java src/DiffExampleTwo.java)

$(PREVIOUS_REPORT):
	$(QUIET)$(COPY) src/DiffExample.java.previous src/DiffExample.java
	$(QUIET)$(call silent_on_success,Testing\
	Differential skips duplicated types on filenames: previous,\
	$(INFER_BIN) --pulse-only -o $(PREVIOUS_DIR) -- $(JAVAC) src/DiffExample.java)
