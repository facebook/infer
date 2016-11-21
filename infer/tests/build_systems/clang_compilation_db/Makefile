# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../..

CMAKE_DIR = ../codetoanalyze/clang_compilation_database
CMAKE_BUILD_DIR = $(CMAKE_DIR)/_build

ANALYZER = infer
CLEAN_EXTRA = $(CMAKE_BUILD_DIR)
INFER_OPTIONS = --report-custom-error --developer-mode --project-root $(CMAKE_DIR) --changed-files-index $(CMAKE_DIR)/index.txt
SOURCES = $(CMAKE_DIR)/hello.cpp
OBJECTS = $(CMAKE_BUILD_DIR)/compile_commands.json
INFERPRINT_OPTIONS = --issues-tests

include $(TESTS_DIR)/base.make

$(CMAKE_BUILD_DIR):
	$(MKDIR_P) $@

$(CMAKE_BUILD_DIR)/compile_commands.json: $(SOURCES) $(CMAKE_DIR)/CMakeLists.txt $(CMAKE_BUILD_DIR)
	cd $(CMAKE_BUILD_DIR) && \
	$(CMAKE) -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ..

infer-out/report.json: $(CMAKE_BUILD_DIR)/compile_commands.json $(INFER_BIN) $(SOURCES)
	$(call silent_on_success,\
	  $(INFER_BIN) -a $(ANALYZER) $(INFER_OPTIONS) -- clang-compilation-database $<)
