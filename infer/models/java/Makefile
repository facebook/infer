# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

ROOT_DIR = ../../..
include $(ROOT_DIR)/Makefile.config

MODELS_OUT = models
INFER_RESULTS_DIR = $(MODELS_OUT)/infer
INFER_REPORT = $(INFER_RESULTS_DIR)/report.json
MODELS_JAR = models.jar

JAVA_SOURCES = $(shell find $(JAVA_BUILTINS_DIR) $(JAVA_MODELS_DIR) -name "*.java")

MODELS_CLASSPATH = $(ANDROID_JAR):$(GUAVA_JAR):$(JACKSON_JAR):$(JSR_305_JAR):$(INFER_ANNOTATIONS_JAR)

all: $(MODELS_JAR)

clean:
	$(QUIET)rm -fr $(MODELS_OUT)
	$(QUIET)rm -f $(MODELS_JAR)
	$(QUIET)rm -f $(JAVA_MODELS_JAR)

compile:
	$(JAVAC) -bootclasspath $(ANDROID_JAR) -d $(MODELS_OUT) -classpath $(MODELS_CLASSPATH) $(JAVA_SOURCES)

$(INFER_REPORT): $(JAVA_DEPS_NO_MODELS) $(JAVA_SOURCES) $(MAKEFILE_LIST)
	$(QUIET)rm -fr $(MODELS_OUT)
	$(QUIET)$(MKDIR_P) $(MODELS_OUT)
	$(QUIET)rm -f $(JAVA_MODELS_JAR)
	$(QUIET)$(call silent_on_success,Building Java models,\
	  $(INFER_BIN) --biabduction-only --results-dir $(INFER_RESULTS_DIR) --biabduction-models-mode -- \
	  $(JAVAC) -bootclasspath $(ANDROID_JAR) -d $(MODELS_OUT) -classpath $(MODELS_CLASSPATH) \
	    $(JAVA_SOURCES) -source 7 -target 7)

$(MODELS_JAR): $(INFER_REPORT)
	cd $(MODELS_OUT); jar cf ../$(MODELS_JAR) *

$(JAVA_MODELS_JAR): $(MODELS_JAR)
	$(INSTALL_DATA) -C $(MODELS_JAR) $(JAVA_MODELS_JAR)

install: $(JAVA_MODELS_JAR)

.PHONY: all clean install
