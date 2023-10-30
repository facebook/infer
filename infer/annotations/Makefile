# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

ROOT_DIR = ../..
include $(ROOT_DIR)/Makefile.config

CWD = $(shell pwd)
JSR_JAR = $(DEPENDENCIES_DIR)/java/jsr-305/jsr305.jar
KOTLIN_ANNOT_JAR = $(DEPENDENCIES_DIR)/java/kotlin-annotations/kotlin-annotations-jvm-1.3.72.jar
SOURCES_DIR = src/main/java
ANNOT_SOURCES = $(shell find $(SOURCES_DIR)/com/facebook/infer/annotation -name "*.java")
ANNOT_CLASSES = 'annot_classes'

ANNOTATIONS_JAR = $(CWD)/annotations.jar
SOURCES_JAR = $(CWD)/annotations-src.jar

all: $(ANNOTATIONS_JAR) $(SOURCES_JAR)

$(ANNOTATIONS_JAR): $(ANNOT_SOURCES)
	$(MKDIR_P) $(ANNOT_CLASSES)
	$(JAVAC) -source 8 -target 8 -cp $(JSR_JAR):$(KOTLIN_ANNOT_JAR) $(ANNOT_SOURCES) -d $(ANNOT_CLASSES)
	cd $(ANNOT_CLASSES) && jar cvf $(ANNOTATIONS_JAR) com

$(SOURCES_JAR): $(ANNOT_SOURCES)
	jar cvf $(SOURCES_JAR) $(SOURCES_DIR)/com

clean:
	$(QUIET)$(REMOVE_DIR) $(ANNOT_CLASSES)
	$(QUIET)$(REMOVE) $(ANNOTATIONS_JAR)
	$(QUIET)$(REMOVE) $(SOURCES_JAR)
