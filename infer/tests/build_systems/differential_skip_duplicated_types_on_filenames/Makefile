# Copyright (c) 2017 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

# E2E test involving the skip_duplicated_types_on_filenames filter

TESTS_DIR = ../..
CLEAN_EXTRA = src/Diff*.java src/Diff*.java *.class

include ../../differential.make

$(CURRENT_REPORT):
	cp src/DiffExample.java.current src/DiffExample.java
	cp src/DiffExampleTwo.java.current src/DiffExampleTwo.java
	$(call silent_on_success,\
	$(INFER_BIN) -o $(CURRENT_DIR) -- javac src/DiffExample.java src/DiffExampleTwo.java)

$(PREVIOUS_REPORT):
	cp src/DiffExample.java.previous src/DiffExample.java
	$(call silent_on_success,\
	$(INFER_BIN) -o $(PREVIOUS_DIR) -- javac src/DiffExample.java)
