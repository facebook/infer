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
INFER_OPTIONS = --siof --ml-buckets cpp --no-filtering --debug-exceptions --project-root $(TESTS_DIR) --no-keep-going
INFERPRINT_OPTIONS = --issues-tests

SOURCES = \
  siof/const.cpp \
  siof/const_use.cpp \
  siof/pod_across_translation_units-1.cpp \
  siof/pod_across_translation_units-2.cpp \
  siof/pod_same_translation_unit.cpp \
  siof/siof.cpp \
  siof/siof_templated.cpp \
  siof/siof_different_tu.cpp \
  siof/std_ios_base_init.cpp \

HEADERS = siof/siof_types.h

include $(TESTS_DIR)/clang.make

infer-out/report.json: $(MAKEFILE_LIST)
