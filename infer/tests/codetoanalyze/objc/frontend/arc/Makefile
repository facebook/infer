# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../../..

CLANG_OPTIONS = -c $(OBJC_CLANG_OPTIONS) -fobjc-arc

SOURCES = \
  ../autoreleasepool/Autoreleasepool.m \
  ../if_and_arc/If_and_arc.m \
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
include $(TESTS_DIR)/objc.make

CLEAN_EXTRA += *.o ../*/*.test.dot ../*/*/*.test.dot
