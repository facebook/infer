# Copyright (c) 2017 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../..

CMAKE_DIR = $(CURDIR)/../codetoanalyze/clang_compilation_database
CMAKE_BUILD_DIR = $(CURDIR)/_build_infer

ANALYZER = infer
CLEAN_EXTRA = $(CMAKE_BUILD_DIR)
INFER_OPTIONS = --report-custom-error --developer-mode --project-root $(CMAKE_DIR) --no-failures-allowed
SOURCES = $(CMAKE_DIR)/hello.cpp
OBJECTS = $(CMAKE_BUILD_DIR)/compile_commands.json
INFERPRINT_OPTIONS = --issues-tests

include $(TESTS_DIR)/infer.make

$(CMAKE_BUILD_DIR):
	$(QUIET)$(MKDIR_P) $@

$(CMAKE_BUILD_DIR)/compile_commands.json: $(SOURCES) $(CMAKE_DIR)/CMakeLists.txt $(CMAKE_BUILD_DIR)
	$(QUIET)cd $(CMAKE_BUILD_DIR) && \
	$(call silent_on_success,Running cmake to generate Makefiles,\
	$(CMAKE) -DCMAKE_EXPORT_COMPILE_COMMANDS=1 $(CMAKE_DIR))

infer-out/report.json: $(CMAKE_BUILD_DIR)/compile_commands.json $(CLANG_DEPS) $(SOURCES)
#	test that config/infer.conf is read and paths inside it are interpreted relative to its
#	location
	$(QUIET)$(call silent_on_success,Testing Clang compilation database with index integration,\
	  INFERCONFIG=config/infer.conf $(INFER_BIN) -a $(ANALYZER) $(INFER_OPTIONS) -o $(@D) \
	    --compilation-database $<)
