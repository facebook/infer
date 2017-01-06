# Copyright (c) 2015 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

ROOT_DIR = ../../..
include $(ROOT_DIR)/Makefile.config

ANDROID_JAR = $(LIB_DIR)/java/android/android-23.jar
GUAVA_JAR = $(DEPENDENCIES_DIR)/java/guava/guava-10.0.1-fork.jar
INFER_ANNOTATIONS_JAR = $(ANNOTATIONS_DIR)/annotations.jar
JACKSON_JAR = $(DEPENDENCIES_DIR)/java/jackson/jackson-2.2.3.jar
JSR_305_JAR = $(DEPENDENCIES_DIR)/java/jsr-305/jsr305.jar

MODELS_OUT = models
INFER_REPORT = $(MODELS_OUT)/infer/report.json
MODELS_JAR = models.jar

JAVA_BUILTINS_SOURCES = $(shell find builtins -name "*.java")
JAVA_MODELS_SOURCES = $(shell find src -name "*.java")

JAVA_SOURCES = $(JAVA_BUILTINS_SOURCES) $(JAVA_MODELS_SOURCES)

MODELS_CLASSPATH = $(ANDROID_JAR):$(GUAVA_JAR):$(JACKSON_JAR):$(JSR_305_JAR):$(INFER_ANNOTATIONS_JAR)

all: $(MODELS_JAR)

clean:
	@rm -fr $(MODELS_OUT)
	@rm -f $(MODELS_JAR)
	@rm -f $(JAVA_MODELS_JAR)

$(INFER_REPORT): $(ANDROID_JAR) $(GUAVA_JAR) $(JACKSON_JAR) $(JSR_305_JAR) $(INFER_ANNOTATIONS_JAR) $(JAVA_DEPS) $(JAVA_SOURCES)
	@rm -fr $(MODELS_OUT)
	mkdir -p $(MODELS_OUT)
	@rm -f $(JAVA_MODELS_JAR)
	$(call silent_on_success,\
	  $(INFER_BIN) --jobs 1 --buck --models-mode -- javac -bootclasspath $(ANDROID_JAR) -d $(MODELS_OUT) -classpath $(MODELS_CLASSPATH) $(JAVA_SOURCES))

$(MODELS_JAR): $(INFER_REPORT)
	cd $(MODELS_OUT); jar cf ../$(MODELS_JAR) *

$(JAVA_MODELS_JAR): $(MODELS_JAR)
	$(INSTALL_DATA) -C $(MODELS_JAR) $(JAVA_MODELS_JAR)

install: $(JAVA_MODELS_JAR)

.PHONY: all clean install
