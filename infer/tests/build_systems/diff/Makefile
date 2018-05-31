# Copyright (c) 2017-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..
INFER_OUT = infer-out
include $(TESTS_DIR)/diff.make

CLEAN_EXTRA = src

SRC_DIR = $(CURDIR)/../codetoanalyze
INFER_OPTIONS = \
  --previous-to-current-script '$(COPY) $(SRC_DIR)/some_bugs.c src/hello.c' \
  --current-to-previous-script '$(COPY) $(SRC_DIR)/some_different_bugs.c src/hello.c' \
  --changed-files-index changed_files.txt \
  -- clang -c src/hello.c

SOURCES = $(SRC_DIR)/some_bugs.c $(SRC_DIR)/some_different_bugs.c

$(INFER_OUT)/differential/introduced.json: $(SOURCES) $(CLANG_DEPS) changed_files.txt \
                                           $(MAKEFILE_LIST)
	$(QUIET)$(MKDIR_P) src
	$(QUIET)$(COPY) $(SRC_DIR)/some_bugs.c src/hello.c
	$(QUIET)$(call silent_on_success,Running diff analysis in $(TEST_REL_DIR),\
	  $(INFER_BIN) -o $(INFER_OUT) --project-root $(CURDIR) diff $(INFER_OPTIONS))
