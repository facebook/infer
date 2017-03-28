# Copyright (c) 2017 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

# E2E test involving the skip_anonymous_class_renamings filter

TESTS_DIR = ../..
CLEAN_EXTRA = src/Diff*.java *.class

include ../../differential.make

$(CURRENT_REPORT):
	$(QUIET)$(COPY) src/DiffExample.java.current src/DiffExample.java
	$(QUIET)$(call silent_on_success,Testing Differential skips anon class renamings: current,\
	$(INFER_BIN) -o $(CURRENT_DIR) -- $(JAVAC) src/*.java)

$(PREVIOUS_REPORT):
	$(QUIET)$(COPY) src/DiffExample.java.previous src/DiffExample.java
	$(QUIET)$(call silent_on_success,Testing Differential skips anon class renamings: previous,\
	$(INFER_BIN) -o $(PREVIOUS_DIR) -- $(JAVAC) src/*.java)
