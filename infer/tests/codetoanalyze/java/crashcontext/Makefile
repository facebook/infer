# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

JAVA_TEST_DIR = ..
include $(JAVA_TEST_DIR)/config.make

ANALYZER = crashcontext

JAVA_SOURCE_FILES = $(wildcard *.java)
JAVA_CLASS_FILES = $(patsubst %.java,%.class,$(JAVA_SOURCE_FILES))
EXP_TESTs = $(sort $(patsubst %.java,%.exp.test,$(JAVA_SOURCE_FILES)))
INFER_OUTs = $(patsubst %.java,infer-out-%,$(JAVA_SOURCE_FILES))

$(JAVA_CLASS_FILES): $(JAVA_SOURCE_FILES)
	javac -cp $(CLASSPATH) $(JAVA_SOURCE_FILES)

# analyze a single source file and generate the test results for it
%.exp.test: $(INFER_BIN) %.stacktrace.json %.java
	$(call silent_on_success,\
	  $(INFER_BIN) -a $(ANALYZER) -o infer-out-$* --stacktrace $*.stacktrace.json \
	  -- javac -cp $(CLASSPATH) $*.java)
#	add a newline at the end of the json when creating the exp.test
	bash -c 'cat infer-out-$*/crashcontext/crashcontext.json <(echo) > $*.exp.test'

# combine the test results for all the source files
issues.exp.test: $(EXP_TESTs)
	cat $(EXP_TESTs) > issues.exp.test


default: compile

.PHONY: compile
compile: $(JAVA_CLASS_FILES)

.PHONY: analyze
analyze: $(EXP_TESTs)

.PHONY: print
print: issues.exp.test

.PHONY: test
test: issues.exp.test
	diff -u issues.exp issues.exp.test

.PHONY: clean
clean:
	$(REMOVE_DIR) $(INFER_OUTs) $(JAVA_CLASS_FILES) $(EXP_TESTs)
