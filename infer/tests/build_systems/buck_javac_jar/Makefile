# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..
ROOT_DIR = $(TESTS_DIR)/../..

TARGET = //src:target
SOURCES = src/UsingJavacJar.java
OBJECTS = buck-out/gen/src/lib__target__output/target.jar
BUCKCONFIG = .buckconfig
INFER_OPTIONS = --eradicate-only
INFERPRINT_OPTIONS = --issues-tests
CLEAN_EXTRA = buck-out

include $(TESTS_DIR)/infer.make

$(OBJECTS): $(SOURCES) $(BUCKCONFIG)
	$(TESTLOCK) $(BUCK) build --config tools.javac_jar= $(TARGET)

infer-out/report.json: $(JAVA_DEPS) $(SOURCES) $(BUCKCONFIG)
	$(QUIET)$(REMOVE_DIR) buck-out && \
	$(call silent_on_success,Testing Buck Java integration with javac_jar,\
	INFER_BIN=$(INFER_BIN) \
	$(INFER_BIN) $(INFER_OPTIONS) -- $(BUCK) build $(TARGET))
