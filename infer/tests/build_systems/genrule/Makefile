# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../..
ROOT_DIR = $(TESTS_DIR)/../..

ANALYZER = infer
BUCK_TARGET = //infer/tests/build_systems/genrule/module2:module2
INFER_TARGET = $(BUCK_TARGET)_infer
SOURCES = $(wildcard $(TESTS_DIR)/codetoanalyze/java/infer/*.java)
OBJECTS = $(ROOT_DIR)/buck-out/genruletest/gen/infer/tests/build_systems/genrule/module2/lib__module2_compile__output/module2_compile.jar
JSON_REPORT = $(ROOT_DIR)/buck-out/gen/infer/tests/build_systems/genrule/module2/module2_infer/infer_out/report.json
INFER_OPTIONS = --project-root $(ROOT_DIR)
INFERPRINT_OPTIONS = --project-root $(ROOT_DIR) --issues-tests
CLEAN_EXTRA = $(ROOT_DIR)/buck-out/genruletest

include $(TESTS_DIR)/java.make
include $(TESTS_DIR)/infer.make

$(OBJECTS): $(JAVA_SOURCE_FILES)
	$(QUIET)cd $(ROOT_DIR) && \
	$(call silent_on_success,Compiling Buck genrule for Java sources,\
	NO_BUCKD=1 $(BUCK) build --no-cache $(BUCK_TARGET))

.PHONY: genrule
genrule: $(JSON_REPORT)


$(JSON_REPORT): $(JAVA_DEPS) $(JAVA_SOURCE_FILES)
	$(QUIET)cd $(ROOT_DIR) && \
	$(REMOVE_DIR) buck-out && \
	$(call silent_on_success,Testing Buck genrule for Java integration,\
	INFER_BIN="$(INFER_BIN)" NO_BUCKD=1 GENERATE_INFER_GENRULES=1 \
	$(BUCK) build --no-cache $(INFER_TARGET))
	$(QUIET)touch $@

infer-out/report.json: $(JSON_REPORT)
	$(QUIET)$(MKDIR_P) infer-out
#	the report contains absolute paths
	$(QUIET)sed -e 's#$(abspath $(TESTS_DIR))/##g' $< > $@
