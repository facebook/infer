# Copyright (c) 2015 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

ROOT_DIR = ../..
include $(ROOT_DIR)/Makefile.config

CWD = $(shell pwd)
JSR_JAR = $(DEPENDENCIES_DIR)/java/jsr-305/jsr305.jar
ANNOT_SOURCES = $(shell find com/facebook/infer/annotation -name "*.java")
ANNOT_CLASSES = 'annot_classes'

ANNOTATIONS_JAR = $(CWD)/annotations.jar

all: $(ANNOTATIONS_JAR)

$(ANNOTATIONS_JAR): $(ANNOT_SOURCES)
	mkdir -p $(ANNOT_CLASSES)
	javac -source 7 -target 7 -cp $(JSR_JAR) $(ANNOT_SOURCES) -d $(ANNOT_CLASSES)
	cd $(ANNOT_CLASSES) && jar cvf $(ANNOTATIONS_JAR) com

clean:
	@rm -rf $(ANNOT_CLASSES)
	@rm -f $(ANNOTATIONS_JAR)
