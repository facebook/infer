# Copyright (c) 2017 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../..
INFER_OUT = infer-out
include $(TESTS_DIR)/diff.make

SRC_DIR = $(CURDIR)/../codetoanalyze
INFER_OPTIONS = \
  --previous-to-current-script '$(COPY) $(SRC_DIR)/some_bugs.c src/hello.c' \
  --current-to-previous-script '$(COPY) $(SRC_DIR)/some_different_bugs.c src/hello.c' \
  --changed-files-index changed_files.txt \
  --report-hook '/bin/true' \
  -- clang -c src/hello.c

SOURCES = $(SRC_DIR)/some_bugs.c $(SRC_DIR)/some_different_bugs.c

$(INFER_OUT)/differential/introduced.json: $(SOURCES) $(CLANG_DEPS) changed_files.txt
	$(QUIET)$(MKDIR_P) src
	$(QUIET)$(COPY) $(SRC_DIR)/some_bugs.c src/hello.c
	$(QUIET)$(call silent_on_success,Running diff analysis in $(TEST_REL_DIR),\
	  $(INFER_BIN) --no-keep-going -o $(INFER_OUT) --project-root $(CURDIR) diff \
	        $(INFER_OPTIONS))
