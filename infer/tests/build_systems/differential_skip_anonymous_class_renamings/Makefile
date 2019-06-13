# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# E2E test for anonymous class renaming

TESTS_DIR = ../..
SOURCES = src/DiffExample.java.current src/DiffExample.java.previous
CLEAN_EXTRA = src/Diff*.java *.class

include $(TESTS_DIR)/differential.make

$(CURRENT_REPORT) $(PREVIOUS_REPORT): $(JAVA_DEPS)

$(CURRENT_REPORT):
	$(QUIET)$(COPY) src/DiffExample.java.current src/DiffExample.java
	$(QUIET)$(call silent_on_success,Testing Differential skips anon class renamings: current,\
	$(INFER_BIN) -o $(CURRENT_DIR) -- $(JAVAC) src/*.java)

$(PREVIOUS_REPORT):
	$(QUIET)$(COPY) src/DiffExample.java.previous src/DiffExample.java
	$(QUIET)$(call silent_on_success,Testing Differential skips anon class renamings: previous,\
	$(INFER_BIN) -o $(PREVIOUS_DIR) -- $(JAVAC) src/*.java)
