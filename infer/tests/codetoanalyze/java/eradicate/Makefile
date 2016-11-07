# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

JAVA_TEST_DIR = ..
include $(JAVA_TEST_DIR)/java.make

ANALYZER = eradicate

INFER_OPTIONS = --eradicate-return-over-annotated --no-filtering --debug-exceptions

JAVA_SOURCE_FILES = $(wildcard *.java)
