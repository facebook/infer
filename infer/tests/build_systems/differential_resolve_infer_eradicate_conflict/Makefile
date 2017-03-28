# Copyright (c) 2017 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

# E2E test involving the resolve_infer_eradicate_conflict filter

TESTS_DIR = ../..
DIFFERENTIAL_ARGS =
CLEAN_EXTRA = src/com/example/Diff*.java src/Diff*.java *.class com/

include ../../differential.make

$(CURRENT_REPORT): .inferconfig
	$(QUIET)$(COPY) src/com/example/DiffExample.java.current src/com/example/DiffExample.java
	$(QUIET)$(COPY) src/DiffExampleTwo.java.current src/DiffExampleTwo.java
	$(QUIET)$(call silent_on_success,Testing Differential resolves Infer/Eradicate conflicts: current,\
	$(INFER_BIN) \
		-o $(CURRENT_DIR) -- $(JAVAC) src/com/example/DiffExample.java src/DiffExampleTwo.java)

$(PREVIOUS_REPORT): .inferconfig
	$(QUIET)$(COPY) src/com/example/DiffExample.java.previous src/com/example/DiffExample.java
	$(QUIET)$(call silent_on_success,Testing Differential resolves Infer/Eradicate conflicts: previous,\
	$(INFER_BIN) -o $(PREVIOUS_DIR) -- $(JAVAC) src/com/example/DiffExample.java)
