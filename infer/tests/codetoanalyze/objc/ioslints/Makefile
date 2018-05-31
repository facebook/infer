# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

IPHONESIMULATOR_ISYSROOT_SUFFIX = /Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk

XCODEROOT = $(shell $(XCODE_SELECT) -p)

CLANG_OPTIONS = -x objective-c \
  -isysroot $(XCODEROOT)$(IPHONESIMULATOR_ISYSROOT_SUFFIX) \
  -mios-simulator-version-min=8.2 --target=x86_64-apple-darwin14 -fobjc-arc -c \

ANALYZER = linters
INFER_OPTIONS = --no-filtering --debug-exceptions --project-root $(TESTS_DIR) \
--iphoneos-target-sdk-version 8.0 \
--iphoneos-target-sdk-version-path-regex "codetoanalyze/objc/ioslints/filter_out_unavailable_api\.m:10.0" \
--iphoneos-target-sdk-version-path-regex "codetoanalyze/objc/ioslints/RemoveObserverInGivenSDKTest\.m:9.0" \

INFERPRINT_OPTIONS = --issues-tests

SOURCES = \
  $(wildcard *.m) \
  $(wildcard */*.m) \

include $(TESTS_DIR)/clang.make
