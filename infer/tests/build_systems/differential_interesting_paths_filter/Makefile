# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# E2E test involving the interesting_paths_filter function

TESTS_DIR = ../..

MODIFIED_FILES_FILE = filter_files.test.txt
DIFFERENTIAL_ARGS = --differential-filter-files $(MODIFIED_FILES_FILE) \
	--differential-filter-set "fixed,preexisting"
SOURCES = $(wildcard src/com/example/*.java.current src/com/example/*.java.previous \
	 src/com/example/*.java.unchanged)
SRC_OBJECT_FILES = src/com/example/*.java src/com/example/*.class com/
CLEAN_EXTRA = $(SRC_OBJECT_FILES) $(MODIFIED_FILES_FILE)

include $(TESTS_DIR)/differential.make

$(CURRENT_REPORT) $(PREVIOUS_REPORT): $(JAVA_DEPS)

.PHONY: compare_reports replace_reports
compare_reports: current.exp.test previous.exp.test
	$(QUIET)$(call check_no_diff,current.exp,current.exp.test)
	$(QUIET)$(call check_no_diff,previous.exp,previous.exp.test)

replace_reports: current.exp.test previous.exp.test
	$(COPY) current.exp.test current.exp
	$(COPY) previous.exp.test previous.exp

$(MODIFIED_FILES_FILE):
	$(QUIET)$(call silent_on_success,Creating interesting paths input file, \
		sed 's#__ABSOLUTE_PATH__#$(CURDIR)#g' '$(MODIFIED_FILES_FILE).template' > $@)

current.exp.test: $(CURRENT_REPORT)
	$(QUIET)$(INFER_BIN) report -o $(<D) \
		--issues-tests-fields $(INFERPRINT_ISSUES_FIELDS) \
		--issues-tests current.exp.test

previous.exp.test: $(PREVIOUS_REPORT)
	$(QUIET)$(INFER_BIN) report -o $(<D) \
		--issues-tests-fields $(INFERPRINT_ISSUES_FIELDS) \
		--issues-tests previous.exp.test

test: compare_reports
print: current.exp.test previous.exp.test
replace: replace_reports

$(CURRENT_REPORT):
	$(QUIET)$(COPY) src/com/example/DiffClass1.java.current src/com/example/DiffClass1.java
	$(QUIET)$(COPY) src/com/example/DiffClass2.java.current src/com/example/DiffClass2.java
	$(QUIET)$(COPY) src/com/example/DiffClass3.java.current src/com/example/DiffClass3.java
	$(QUIET)$(COPY) src/com/example/DiffClassUnchanged.java.unchanged src/com/example/DiffClassUnchanged.java
	$(QUIET)$(call silent_on_success,Testing Differential with interesting paths: current,\
	  $(INFER_BIN) --pulse-only -o $(CURRENT_DIR) -- $(JAVAC) src/com/example/*.java)
	$(QUIET)$(REMOVE_DIR) $(SRC_OBJECT_FILES)

$(PREVIOUS_REPORT):
	$(QUIET)$(COPY) src/com/example/DiffClass1.java.previous src/com/example/DiffClass1.java
	$(QUIET)$(COPY) src/com/example/DiffClass2.java.previous src/com/example/DiffClass2.java
	$(QUIET)$(COPY) src/com/example/DiffClassThree.java.previous src/com/example/DiffClassThree.java
	$(QUIET)$(COPY) src/com/example/DiffClassUnchanged.java.unchanged src/com/example/DiffClassUnchanged.java
	$(QUIET)$(call silent_on_success,Testing Differential with interesting paths: previous,\
	  $(INFER_BIN) --pulse-only -o $(PREVIOUS_DIR) -- $(JAVAC) src/com/example/*.java)
	$(QUIET)$(REMOVE_DIR) $(SRC_OBJECT_FILES)
