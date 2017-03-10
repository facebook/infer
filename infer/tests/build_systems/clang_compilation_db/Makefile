# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../..

CMAKE_DIR = ../codetoanalyze/clang_compilation_database
CMAKE_BUILD_DIR = $(CMAKE_DIR)/_build_infer

ANALYZER = infer
CLEAN_EXTRA = $(CMAKE_BUILD_DIR)
INFER_OPTIONS = --report-custom-error --developer-mode --project-root $(CMAKE_DIR) --no-failures-allowed
SOURCES = $(CMAKE_DIR)/hello.cpp
OBJECTS = $(CMAKE_BUILD_DIR)/compile_commands.json
INFERPRINT_OPTIONS = --issues-tests

include $(TESTS_DIR)/base.make

$(CMAKE_BUILD_DIR):
	$(MKDIR_P) $@

$(CMAKE_BUILD_DIR)/compile_commands.json: $(SOURCES) $(CMAKE_DIR)/CMakeLists.txt $(CMAKE_BUILD_DIR)
	cd $(CMAKE_BUILD_DIR) && \
	$(CMAKE) -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ..

infer-out/report.json:
#	make this target empty - since this test defines issues.exp.test directly

infer-out-with-index/report.json: $(CMAKE_BUILD_DIR)/compile_commands.json $(CLANG_DEPS) $(SOURCES)
	$(call silent_on_success,\
	  $(INFER_BIN) -a $(ANALYZER) $(INFER_OPTIONS) -o infer-out-with-index --changed-files-index $(CMAKE_DIR)/index.txt --compilation-database $<)

infer-out-no-index/report.json: $(CMAKE_BUILD_DIR)/compile_commands.json $(CLANG_DEPS) $(SOURCES)
	$(call silent_on_success,\
	  $(INFER_BIN) -a $(ANALYZER) $(INFER_OPTIONS) -o infer-out-no-index --compilation-database $<)

infer-out-reactive-capture/report.json: $(CMAKE_BUILD_DIR)/compile_commands.json $(CLANG_DEPS) $(SOURCES)
	$(call silent_on_success,\
	  $(INFER_BIN) -a $(ANALYZER) $(INFER_OPTIONS) -o infer-out-reactive-capture --reactive-capture \
	  --changed-files-index $(CMAKE_DIR)/index.txt --compilation-database $<)

issues.exp.test: infer-out-with-index/report.json infer-out-no-index/report.json infer-out-reactive-capture/report.json
	$(INFERPRINT_BIN) -q -a $(ANALYZER) $(INFERPRINT_OPTIONS) $@.with-index \
		--from-json-report infer-out-with-index/report.json
	$(INFERPRINT_BIN) -q -a $(ANALYZER) $(INFERPRINT_OPTIONS) $@.no-index \
	  --from-json-report infer-out-no-index/report.json
	$(INFERPRINT_BIN) -q -a $(ANALYZER) $(INFERPRINT_OPTIONS) $@.reactive-capture \
	  --from-json-report infer-out-reactive-capture/report.json
	cat $@.with-index $@.no-index $@.reactive-capture > $@
