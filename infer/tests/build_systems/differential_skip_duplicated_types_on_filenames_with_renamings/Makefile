# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# E2E test involving the skip_duplicated_types_on_filenames filter

TESTS_DIR = ../..
SOURCES = $(wildcard src/*.java.current src/*.java.previous)
DIFFERENTIAL_ARGS = --file-renamings file_renamings.json
CLEAN_EXTRA = src/Diff*.java *.class

include $(TESTS_DIR)/differential.make

$(CURRENT_REPORT) $(PREVIOUS_REPORT): $(JAVA_DEPS)

# TODO: the renaming heuristic doesn't work for Pulse because the error messages include class
# names. Find a more robust way to deal with this issue.

$(CURRENT_REPORT):
	$(QUIET)$(COPY) src/DiffExampleRenamed.java.current src/DiffExampleRenamed.java
	$(QUIET)$(call silent_on_success,Testing\
	Differential skips duplicated types on renamed files: current,\
	$(INFER_BIN) --biabduction-only -o $(CURRENT_DIR) -- $(JAVAC) src/DiffExampleRenamed.java)

$(PREVIOUS_REPORT):
	$(QUIET)$(COPY) src/DiffExample.java.previous src/DiffExample.java
	$(QUIET)$(call silent_on_success,Testing\
	Differential skips duplicated types on renamed files: previous,\
	$(INFER_BIN) --biabduction-only -o $(PREVIOUS_DIR) -- $(JAVAC) src/DiffExample.java)
