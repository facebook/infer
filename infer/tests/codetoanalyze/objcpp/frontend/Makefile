# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR=../../..

IPHONESIMULATOR_ISYSROOT_SUFFIX = /Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk

XCODEROOT = $(shell $(XCODE_SELECT) -p)

CLANG_OPTIONS = -x objective-c++ -std=c++11 \
  -isysroot $(XCODEROOT)$(IPHONESIMULATOR_ISYSROOT_SUFFIX) \
  -mios-simulator-version-min=8.2 --target=x86_64-apple-darwin14 -c \

INFER_OPTIONS =

SOURCES = \
  $(wildcard */*.mm) \

include $(TESTS_DIR)/clang-frontend.make
