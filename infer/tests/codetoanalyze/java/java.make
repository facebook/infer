# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

# Makefiles that include this one must define JAVA_TEST_DIR and then include
# $(JAVA_TEST_DIR)/java.make in order for config.make to be found.
#
# Makefiles that include this one must define the ANALYZER and JAVA_SOURCE_FILES
# variables, and may optionally define INFER_OPTIONS and INFERPRINT_OPTIONS.

include $(JAVA_TEST_DIR)/config.make

INFERPRINT_OPTIONS = --issues-tests

JAVA_CLASS_FILES = $(patsubst %.java,%.class,$(JAVA_SOURCE_FILES))

$(JAVA_CLASS_FILES): $(JAVA_SOURCE_FILES)
	javac -cp $(CLASSPATH) $(JAVA_SOURCE_FILES)

infer-out/report.json: $(INFER_BIN) $(JAVA_SOURCE_FILES)
	$(call silent_on_success,\
	  $(INFER_BIN) $(INFER_OPTIONS) -a $(ANALYZER) -- javac -cp $(CLASSPATH) $(JAVA_SOURCE_FILES))

issues.exp.test: infer-out/report.json $(INFERPRINT_BIN)
	$(INFERPRINT_BIN) -q -a $(ANALYZER) $(INFERPRINT_OPTIONS) $@ --from-json-report $<
	LC_ALL=C sort -t: -k1,1 -k2n,2 -o $@ $@

default: compile

.PHONY: compile
compile: $(JAVA_CLASS_FILES)

.PHONY: analyze
analyze: infer-out/report.json

.PHONY: print
print: issues.exp.test

.PHONY: test
test: issues.exp.test
	diff -u issues.exp issues.exp.test

.PHONY: clean
clean:
	rm -rf codetoanalyze issues.exp.test infer-out $(JAVA_CLASS_FILES)
