# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../../../..

IPHONESIMULATOR_ISYSROOT_SUFFIX = \
  /Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk

XCODEROOT = $(shell $(XCODE_SELECT) -p)

CLANG_OPTIONS = -x objective-c \
  -isysroot $(XCODEROOT)$(IPHONESIMULATOR_ISYSROOT_SUFFIX) \
  -mios-simulator-version-min=8.2 --target=x86_64-apple-darwin14 -c \
  -fobjc-arc

SOURCES = \
  ../predefined_expr/PredefinedExprExample.m \
  ../types/attributes.m \
  ../types/void_call.m \
  ../vardecl/initlist.m \
  ../shared/assertions/NSAssert_example.m \
  ../shared/block/BlockVar.m \
  ../shared/memory_leaks_benchmark/ArcExample.m \
  ../shared/memory_leaks_benchmark/arc_methods.m \
  ../shared/npe/Nonnull_attribute_example.m \

include $(TESTS_DIR)/clang-frontend.make
