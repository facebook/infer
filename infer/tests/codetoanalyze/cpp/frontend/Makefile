# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR=../../..

# see explanations in cpp/errors/Makefile for the custom isystem
CLANG_OPTIONS = \
  -nostdinc++ -isystem$(ROOT_DIR) -isystem$(CLANG_INCLUDES)/c++/v1/ \
  -x c++ -std=c++11 -c
INFER_OPTIONS = --headers --no-keep-going


SOURCES = \
  $(wildcard */*.cpp) \
  $(wildcard shared/*/*.cpp) \

include $(TESTS_DIR)/clang-frontend.make
