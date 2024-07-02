# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

# see explanations in cpp/pulse/Makefile for the custom isystem
CLANG_OPTIONS = -x c++ -std=c++14 -nostdinc++ -isystem$(CLANG_INCLUDES)/c++/v1/ -c
INFER_OPTIONS = --siof-only --siof-check-iostreams --debug-exceptions --project-root $(TESTS_DIR)
INFERPRINT_OPTIONS = --issues-tests

SOURCES = \
  siof/by_ref.cpp \
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
