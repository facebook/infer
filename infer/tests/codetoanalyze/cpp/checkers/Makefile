# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../../..
include $(TESTS_DIR)/clang.make

ANALYZER = checkers
CLANG_OPTIONS = -x c++ -std=c++11 -isystem$(MODELS_DIR)/cpp/include -isystem$(CLANG_INCLUDES)/c++/v1/ -c
INFER_OPTIONS = --cxx --ml-buckets cpp --no-filtering --debug-exceptions
INFERPRINT_OPTIONS = --issues-txt

SOURCES = \
  siof/const.cpp \
  siof/const_use.cpp \
  siof/pod_across_translation_units-1.cpp \
  siof/pod_across_translation_units-2.cpp \
  siof/pod_same_translation_unit.cpp \
  siof/siof_across_translation_units-1.cpp \
  siof/siof_across_translation_units-2.cpp \
