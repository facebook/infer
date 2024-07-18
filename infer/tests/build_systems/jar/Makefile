# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

INFERPRINT_OPTIONS = --issues-tests

MVN_DIR = ../codetoanalyze/mvn/simple_app_for_jar_test
SOURCES = $(MVN_DIR)/pom.xml $(shell find $(MVN_DIR) -type f -name '*.java')
JAR_FILE = $(MVN_DIR)/target/simple-app-1.0-SNAPSHOT.jar

CLEAN_EXTRA = $(TARGET)

include $(TESTS_DIR)/infer.make

infer-out/report.json: $(JAVA_DEPS) $(SOURCES)
	$(QUIET)cd $(MVN_DIR) && $(MVN) --quiet clean install
	$(QUIET)$(call silent_on_success,Testing mvn Java integration: $*,\
	  $(INFER_BIN) --pulse-only --project-root $(TESTS_DIR) \
	               --generated-classes $(JAR_FILE) --sourcepath $(MVN_DIR))
