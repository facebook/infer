# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

CMAKE_DIR = ../codetoanalyze/clang_compilation_database
CMAKE_BUILD_DIR = $(CMAKE_DIR)/_build_infer

CLEAN_EXTRA = $(CMAKE_BUILD_DIR) infer-out-with-index infer-out-no-index issues.exp.test.no-index issues.exp.test.with-index
INFER_OPTIONS = --report-custom-error --developer-mode --project-root $(CMAKE_DIR)
SOURCES = $(CMAKE_DIR)/hello.cpp
INFERPRINT_OPTIONS = --issues-tests

include $(TESTS_DIR)/infer.make

$(CMAKE_BUILD_DIR):
	$(QUIET)$(MKDIR_P) $@

$(CMAKE_BUILD_DIR)/compile_commands.json: $(SOURCES) $(CMAKE_DIR)/CMakeLists.txt $(CMAKE_BUILD_DIR)
	$(QUIET)cd $(CMAKE_BUILD_DIR) && \
	$(call silent_on_success,Running cmake to generate Makefiles,\
	$(CMAKE) -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ..)

infer-out/report.json:
#	make this target empty - since this test defines issues.exp.test directly

infer-out-with-index/report.json: $(CMAKE_BUILD_DIR)/compile_commands.json $(CLANG_DEPS) $(SOURCES)
	$(QUIET)$(call silent_on_success,Testing Clang compilation database with index integration,\
	  $(INFER_BIN) $(INFER_OPTIONS) -o $(@D) \
	    --changed-files-index $(CMAKE_DIR)/index.txt --compilation-database $<)

infer-out-no-index/report.json: $(CMAKE_BUILD_DIR)/compile_commands.json $(CLANG_DEPS) $(SOURCES)
	$(QUIET)$(call silent_on_success,Testing Clang compilation database integration,\
	  $(INFER_BIN) $(INFER_OPTIONS) -o $(@D) --compilation-database $<)

issues.exp.test: infer-out-with-index/report.json infer-out-no-index/report.json
	$(QUIET)$(INFER_BIN) report -q $(INFERPRINT_OPTIONS) $@.with-index \
	  -o infer-out-with-index
	$(QUIET)$(INFER_BIN) report -q $(INFERPRINT_OPTIONS) $@.no-index \
	  -o infer-out-no-index
	$(QUIET)cat $@.with-index $@.no-index > $@
