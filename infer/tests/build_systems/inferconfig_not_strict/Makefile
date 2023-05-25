# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

CMAKE_DIR = $(CURDIR)/../codetoanalyze/clang_compilation_database
CMAKE_BUILD_DIR = $(CURDIR)/_build_infer

CLEAN_EXTRA = $(CMAKE_BUILD_DIR)
INFER_OPTIONS = --report-custom-error --developer-mode --project-root $(CMAKE_DIR)
SOURCES = $(CMAKE_DIR)/hello.cpp
INFERPRINT_OPTIONS = --issues-tests

include $(TESTS_DIR)/infer.make
export INFER_STRICT_MODE=0

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
	  INFERCONFIG=config/infer.conf $(INFER_BIN) $(INFER_OPTIONS) -o $(@D) \
	    --compilation-database $<)
