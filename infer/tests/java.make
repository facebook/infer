# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

ROOT_DIR = $(TESTS_DIR)/../..

include $(ROOT_DIR)/Makefile.config

ANDROID = $(JAVA_LIB_DIR)/android/android-23.jar
ANDROIDSUPPORT = $(DEPENDENCIES_DIR)/java/android/support/v4/android-support-v4.jar
ANNOTATIONS = $(ANNOTATIONS_DIR)/annotations.jar
GUAVA = $(DEPENDENCIES_DIR)/java/guava/guava-23.0.jar
JACKSON = $(DEPENDENCIES_DIR)/java/jackson/jackson-2.2.3.jar
JSR305 = $(DEPENDENCIES_DIR)/java/jsr-305/jsr305.jar
INJECT = $(DEPENDENCIES_DIR)/java/jsr-330/javax.inject.jar

CLASSPATH=$(ANDROID):$(ANDROIDSUPPORT):$(ANNOTATIONS):$(GUAVA):$(JACKSON):$(JSR305):$(INJECT):$(JAVA_BUILTINS_DIR):.
