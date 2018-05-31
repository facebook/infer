# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

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
  ../shared/block/BlockVar.m \
	../shared/block/Blocks_as_parameters.m \
  ../shared/memory_leaks_benchmark/ArcExample.m \
  ../shared/memory_leaks_benchmark/arc_methods.m \
	../shared/npe/Available_expr.m \
  ../shared/npe/Nonnull_attribute_example.m \

include $(TESTS_DIR)/clang-frontend.make
