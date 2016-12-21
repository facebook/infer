# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../../..
include $(TESTS_DIR)/java.make

# see base.make
TEST_REL_DIR = $(patsubst $(abspath $(TESTS_DIR))/%,%,$(abspath $(CURDIR)))

ANALYZER = crashcontext

SOURCES = $(wildcard *.java)
OBJECTS = $(patsubst %.java,%.class,$(SOURCES))
EXP_TESTs = $(sort $(patsubst %.java,%.exp.test.noreplace,$(SOURCES)))
INFER_OUTs = $(patsubst %.java,infer-out-%,$(SOURCES))

$(OBJECTS): $(SOURCES)
	javac -cp $(CLASSPATH) $(SOURCES)

# analyze a single source file and generate the test results for it
%.exp.test.noreplace: $(JAVA_DEPS) %.stacktrace.json %.java
	$(call silent_on_success,\
	  $(INFER_BIN) -a $(ANALYZER) -o infer-out-$* --stacktrace $*.stacktrace.json \
	  -- javac -cp $(CLASSPATH) $*.java)
#	add a newline at the end of the json when creating the exp.test
	$(COPY) infer-out-$*/crashcontext/crashcontext.json $@ && echo >> $@

# combine the test results for all the source files
issues.exp.test: $(EXP_TESTs)
	cat $(EXP_TESTs) > issues.exp.test

default: compile

.PHONY: compile
compile: $(OBJECTS)

.PHONY: analyze
analyze: $(EXP_TESTs)

.PHONY: print
print: issues.exp.test

.PHONY: test
test: issues.exp.test
	@cd $(TESTS_DIR) && \
	diff -u $(TEST_REL_DIR)/issues.exp $(TEST_REL_DIR)/issues.exp.test

.PHONY: clean
clean:
	$(REMOVE_DIR) $(INFER_OUTs) $(OBJECTS) $(EXP_TESTs)
