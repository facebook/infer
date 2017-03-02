# Copyright (c) 2015 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

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
	@rm -fr $(MODELS_OUT)
	@rm -f $(MODELS_JAR)
	@rm -f $(JAVA_MODELS_JAR)

compile:
	javac -bootclasspath $(ANDROID_JAR) -d $(MODELS_OUT) -classpath $(MODELS_CLASSPATH) $(JAVA_SOURCES)

$(INFER_REPORT): $(JAVA_DEPS_NO_MODELS) $(JAVA_SOURCES)
	@rm -fr $(MODELS_OUT)
	mkdir -p $(MODELS_OUT)
	@rm -f $(JAVA_MODELS_JAR)
	$(call silent_on_success,\
	  $(INFER_BIN) --jobs 1 --results-dir $(INFER_RESULTS_DIR) --models-mode -- \
	  javac -bootclasspath $(ANDROID_JAR) -d $(MODELS_OUT) -classpath $(MODELS_CLASSPATH) \
	    $(JAVA_SOURCES))

$(MODELS_JAR): $(INFER_REPORT)
	cd $(MODELS_OUT); jar cf ../$(MODELS_JAR) *

$(JAVA_MODELS_JAR): $(MODELS_JAR)
	$(INSTALL_DATA) -C $(MODELS_JAR) $(JAVA_MODELS_JAR)

install: $(JAVA_MODELS_JAR)

.PHONY: all clean install
