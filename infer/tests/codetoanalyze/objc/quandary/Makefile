# Copyright (c) 2017 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../../..

IPHONESIMULATOR_ISYSROOT_SUFFIX = /Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk

XCODEROOT = $(shell xcode-select -p)

CLANG_OPTIONS = -x objective-c \
  -isysroot $(XCODEROOT)$(IPHONESIMULATOR_ISYSROOT_SUFFIX) \
  -mios-simulator-version-min=8.2 --target=x86_64-apple-darwin14 -c \

ANALYZER = quandary
INFER_OPTIONS = --no-filtering --debug-exceptions --project-root $(TESTS_DIR) --iphoneos-target-sdk-version 8.0
INFERPRINT_OPTIONS = --issues-tests

SOURCES = \
  $(wildcard *.m) \

include $(TESTS_DIR)/clang.make
