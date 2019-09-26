# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR=../../..

CLANG_OPTIONS = -c $(OBJCPP_CLANG_OPTIONS)

INFER_OPTIONS =

SOURCES = \
  $(wildcard *.mm) $(wildcard */*.mm) \

include $(TESTS_DIR)/clang-frontend.make
include $(TESTS_DIR)/objc.make
