# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

ROOT_DIR = $(TESTS_DIR)/../..

include $(ROOT_DIR)/Makefile.config

ANDROID = $(JAVA_LIB_DIR)/android/android-23.jar
ANDROIDSUPPORT = $(DEPENDENCIES_DIR)/java/android/support/v4/android-support-v4.jar
ANDROIDX_COLLECTION = $(JAVA_LIB_DIR)/androidx/collection-1.1.0.jar
GUAVA = $(DEPENDENCIES_DIR)/java/guava/guava-23.0.jar
INJECT = $(DEPENDENCIES_DIR)/java/jsr-330/javax.inject.jar
JACKSON = $(DEPENDENCIES_DIR)/java/jackson/jackson-2.2.3.jar
JSR305 = $(DEPENDENCIES_DIR)/java/jsr-305/jsr305.jar
KOTLIN_ANNOTATIONS = $(DEPENDENCIES_DIR)/java/kotlin-annotations/kotlin-annotations-jvm-1.3.72.jar
SUNTOOLS = $(DEPENDENCIES_DIR)/java/sun-tools/tools.jar

# On Windows, class path entries are separated by ';' (':' is part of drive letters such as `C:`)
# and must be Windows paths: `cygpath -m` turns `/c/dir` into `C:/dir`.
ifeq ($(WINDOWS_BUILD),yes)
CLASSPATH_SEP := ;
native_path = $(shell cygpath -m '$(1)')
else
CLASSPATH_SEP := :
native_path = $(1)
endif

EMPTY :=
SPACE := $(EMPTY) $(EMPTY)
classpath_of = \
  $(subst $(SPACE),$(CLASSPATH_SEP),$(strip $(foreach entry,$(1),$(call native_path,$(entry)))))

# the last '.' is where javac writes the classes it compiles, and stays as it is: a relative path
# needs no conversion, and every user of $(CLASSPATH) runs javac in the directory it refers to
CLASSPATH = $(call classpath_of,$(ANDROID) $(ANDROIDX_COLLECTION) $(ANDROIDSUPPORT) \
  $(INFER_ANNOTATIONS_JAR) $(GUAVA) $(JACKSON) $(JSR305) $(INJECT) $(KOTLIN_ANNOTATIONS) \
  $(SUNTOOLS) $(TEST_CLASSPATH))$(CLASSPATH_SEP).
