# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

INFER_OPTIONS = --java-debug-source-file-info
INFER_OPTIONS_EXPERIMENTAL = --java-source-parser-experimental --java-debug-source-file-info
SOURCES = Main.java

test: parser.output.test experimental_parser.output.test
	$(call check_no_diff,parser.output.exp,parser.output.test)
	$(call check_no_diff,parser.output.exp,experimental_parser.output.test)

replace: parser.output.test
	cp $< parser.output

clean:
	$(REMOVE_DIR) infer-out* *.test *.class

# we check if the java source file is valid for javac
compile:
	javac *.java

.PHONY: parser.output.test experimental_parser.output.test
parser.output.test: $(SOURCES) $(INFER_BIN)
	$(INFER_BIN) $(INFER_OPTIONS) $(SOURCES) > $@

experimental_parser.output.test: $(SOURCES) $(INFER_BIN)
	$(INFER_BIN) -o infer-out-experimental $(INFER_OPTIONS_EXPERIMENTAL) $(SOURCES) > $@

include $(TESTS_DIR)/base.make
