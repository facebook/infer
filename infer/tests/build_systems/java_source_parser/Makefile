# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

INFER_OPTIONS = --java-debug-source-file-info
SOURCES = Main.java

test: parser.output.test
	$(call check_no_diff,parser.output,parser.output.test)

replace: parser.output.test
	cp $< parser.output

clean:
	rm -fr infer-out parser.output.test *.class

# we check if the java source file is valid for javac
compile:
	javac *.java

.PHONY: parser.output.test
parser.output.test: $(SOURCES) $(INFER_BIN)
	$(INFER_BIN) $(INFER_OPTIONS) $(SOURCES) > parser.output.test

include $(TESTS_DIR)/base.make
