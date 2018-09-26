# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..
include $(TESTS_DIR)/java.make
include $(TESTS_DIR)/base.make

SOURCES = $(wildcard *.java)
OBJECTS = $(patsubst %.java,%.class,$(SOURCES))
EXP_TESTs = $(sort $(patsubst %.java,issues-%.exp.test,$(SOURCES)))
INFER_OUTs = $(patsubst %.java,infer-out-%,$(SOURCES))

$(OBJECTS): $(SOURCES)
	$(JAVAC) -cp $(CLASSPATH) $(SOURCES)

# analyze a single source file and generate the test results for it
issues-%.exp.test: $(JAVA_DEPS) %.stacktrace.json %.java
	$(QUIET)$(call silent_on_success,Testing crashcontext: $*,\
	  $(INFER_BIN) --crashcontext-only -o infer-out-$* --stacktrace $*.stacktrace.json \
	  -- $(JAVAC) -cp $(CLASSPATH) $*.java)
#	add a newline at the end of the json when creating the exp.test
	$(QUIET)$(COPY) infer-out-$*/crashcontext/crashcontext.json $@ && echo >> $@

# combine the test results for all the source files
issues.exp.test: $(EXP_TESTs)
	$(QUIET)cat $^ > $@

default: compile

.PHONY: compile
compile: $(OBJECTS)

.PHONY: analyze
analyze: $(EXP_TESTs)

.PHONY: print
print: issues.exp.test

.PHONY: test
test: issues.exp.test
	$(QUIET)cd $(TESTS_DIR) && \
	$(call check_no_diff,$(TEST_REL_DIR)/issues.exp,$(TEST_REL_DIR)/issues.exp.test)

.PHONY: replace
replace: issues.exp.test
	cp $< issues.exp

.PHONY: clean
clean:
	$(REMOVE_DIR) $(INFER_OUTs) $(OBJECTS) $(EXP_TESTs)
