# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..
ROOT_DIR = $(TESTS_DIR)/../..

BUCK_TARGET = //infer/tests/build_systems/genrule/module2:module2
INFER_TARGET = $(BUCK_TARGET)_infer
SOURCES = $(wildcard $(TESTS_DIR)/codetoanalyze/java/infer/*.java)
OBJECTS = $(ROOT_DIR)/buck-out/genruletest/gen/infer/tests/build_systems/genrule/module2/lib__module2_compile__output/module2_compile.jar
JSON_REPORT = $(ROOT_DIR)/buck-out/gen/infer/tests/build_systems/genrule/module2/module2_infer/infer_out/report.json
INFERPRINT_OPTIONS = --project-root $(ROOT_DIR) --issues-tests

# fake infer-out because we only copy the results.json from buck-out.
INFER_OUT = .

include $(TESTS_DIR)/java.make
include $(TESTS_DIR)/infer.make

$(OBJECTS): $(JAVA_SOURCE_FILES)
	$(QUIET)cd $(ROOT_DIR) && \
	$(call silent_on_success,Compiling Buck genrule for Java sources,\
	$(BUCK) build --no-cache $(BUCK_TARGET))

.PHONY: genrule
genrule: $(JSON_REPORT)


$(JSON_REPORT): $(JAVA_DEPS) $(JAVA_SOURCE_FILES) $(MAKEFILE_LIST)
	$(QUIET)cd $(ROOT_DIR) && \
	$(REMOVE_DIR) buck-out && \
	$(call silent_on_success,Testing Buck genrule for Java integration,\
	INFER_BIN="$(INFER_BIN)" PROJECT_ROOT="$(PWD)" $(BUCK) build --no-cache $(INFER_TARGET))
	$(QUIET)touch $@

report.json: $(JSON_REPORT) $(MAKEFILE_LIST)
#	the report contains absolute paths
	$(QUIET)sed -e 's#$(abspath $(TESTS_DIR))/##g' $< > $@

issues.exp.test$(TEST_SUFFIX): report.json $(INFER_BIN)
	$(QUIET)$(INFER_BIN) report -q \
	   $(INFERPRINT_OPTIONS) $@ --from-json-report $<
