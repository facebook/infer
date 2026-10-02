# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# The same changed files in two versions of a project, the current one with a new source file that
# includes the changed header

TESTS_DIR = ../..

MODIFIED_FILES_FILE = changed_files.txt
SOURCES = $(wildcard src/*.cpp src/*.h)
CLEAN_EXTRA += $(patsubst src/%.cpp,%.o,$(wildcard src/*.cpp))
INFER_OPTIONS = --changed-files-index $(MODIFIED_FILES_FILE)

include $(TESTS_DIR)/differential.make

$(CURRENT_REPORT): $(INFER_CLANG_DEPS) $(SOURCES) $(MODIFIED_FILES_FILE)
	$(QUIET)$(call silent_on_success,Testing a changed header: current,\
	  $(INFER_BIN) -P -o $(CURRENT_DIR) $(INFER_OPTIONS) -- clang++ -c src/user.cpp src/new_user.cpp)

$(PREVIOUS_REPORT): $(INFER_CLANG_DEPS) $(SOURCES) $(MODIFIED_FILES_FILE)
	$(QUIET)$(call silent_on_success,Testing a changed header: previous,\
	  $(INFER_BIN) -P -o $(PREVIOUS_DIR) $(INFER_OPTIONS) -- clang++ -c src/user.cpp)
