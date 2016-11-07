# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

ANALYZER = infer
ROOT_DIR = ../../../..

include $(ROOT_DIR)/Makefile.config

default: compile

JAVA_SOURCE_FILES = $(wildcard src/infer/*.java)
JAVA_CLASS_FILES = $(patsubst %.java,ant_out/%.class,$(JAVA_SOURCE_FILES))

$(JAVA_CLASS_FILES): $(JAVA_SOURCE_FILES)
	ant

infer-out/report.json: $(INFER_BIN) $(JAVA_SOURCE_FILES)
	$(call silent_on_success,\
	  $(INFER_BIN) -a $(ANALYZER) -- ant)

issues.exp.test: infer-out/report.json $(INFERPRINT_BIN)
	$(INFERPRINT_BIN) -q -a $(ANALYZER) --issues-tests $@ --from-json-report $<
	LC_ALL=C sort -t: -k1,1 -k2n,2 -o $@ $@

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
	ant clean
	rm -rf issues.exp.test infer-out
