# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR=../../..

IPHONESIMULATOR_ISYSROOT_SUFFIX = /Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk

XCODEROOT = $(shell xcode-select -p)

CLANG_OPTIONS = -x objective-c++ -std=c++11 \
  -isysroot $(XCODEROOT)$(IPHONESIMULATOR_ISYSROOT_SUFFIX) \
  -mios-simulator-version-min=8.2 --target=x86_64-apple-darwin14 -c \

SOURCES = \
  $(wildcard */*.mm) \

include $(TESTS_DIR)/clang-frontend.make

compile:
	clang $(CLANG_OPTIONS) $(SOURCES)

capture:
	$(INFER_BIN) -a capture --cxx --frontend-tests --continue --reactive --project-root $(TESTS_DIR) -- clang $(CLANG_OPTIONS) $(SOURCES)
