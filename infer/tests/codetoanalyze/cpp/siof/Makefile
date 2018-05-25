# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../../..

ANALYZER = checkers
# see explanations in cpp/errors/Makefile for the custom isystem
CLANG_OPTIONS = -x c++ -std=c++11 -nostdinc++ -isystem$(MODELS_DIR)/cpp/include -isystem$(CLANG_INCLUDES)/c++/v1/ -c
INFER_OPTIONS = --siof-only --cxx-infer-headers --debug-exceptions --project-root $(TESTS_DIR)
INFERPRINT_OPTIONS = --issues-tests

SOURCES = \
  siof/const.cpp \
  siof/const_use.cpp \
  siof/duplicate_reports.cpp \
  siof/pod_across_translation_units-1.cpp \
  siof/pod_across_translation_units-2.cpp \
  siof/pod_same_translation_unit.cpp \
  siof/include_iostream.cpp \
  siof/siof.cpp \
  siof/siof_templated.cpp \
  siof/siof_different_tu.cpp \
  siof/std_ios_base_init.cpp \
  siof/smart_pointer1.cpp \
  siof/smart_pointer2.cpp \

HEADERS = siof/siof_types.h siof/smart_pointer1.h

include $(TESTS_DIR)/clang.make

infer-out$(TEST_SUFFIX)/report.json: $(MAKEFILE_LIST)
