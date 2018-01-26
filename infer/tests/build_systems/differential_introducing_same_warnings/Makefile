# Copyright (c) 2017 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

# E2E test involving the skip_duplicated_types_on_filenames filter

TESTS_DIR = ../..
CLEAN_EXTRA = *.class src/*.java

CLASSPATH =

include ../../differential.make
include ../../java.make

$(CURRENT_REPORT):
	$(QUIET)$(COPY) src/DiffExample.java.current src/DiffExample.java
	$(QUIET)$(call silent_on_success,Testing\
	Differential same warning as introduced: current,\
	$(INFER_BIN) -o $(CURRENT_DIR) --eradicate-only -- $(JAVAC) -cp $(JSR305) src/DiffExample.java)

$(PREVIOUS_REPORT):
	$(QUIET)$(COPY) src/DiffExample.java.previous src/DiffExample.java
	$(QUIET)$(call silent_on_success,Testing\
	Differential same warning as introduced: previous,\
	$(INFER_BIN) -o $(PREVIOUS_DIR) --eradicate-only -- $(JAVAC) -cp $(JSR305) src/DiffExample.java)
