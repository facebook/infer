# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../../../..

IPHONESIMULATOR_ISYSROOT_SUFFIX = \
  /Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk

XCODEROOT = $(shell xcode-select -p)

CLANG_OPTIONS = -x objective-c \
  -isysroot $(XCODEROOT)$(IPHONESIMULATOR_ISYSROOT_SUFFIX) \
  -mios-simulator-version-min=8.2 --target=x86_64-apple-darwin14 -c

SOURCES = \
  ../block/retain_cycle.m \
  ../block/static.m \
  ../boxing/Boxing.m \
  ../boxing/array.m \
  ../boxing/array_literal.c \
  ../boxing/dict_literal.c \
  ../boxing/string_literal.c \
  ../conditional_operation/ConditionalOperation.m \
  ../exceptions/ExceptionExample.m \
  ../fast_enumeration/Fast_enumeration.m \
  ../property/PropertyCustomAccessor.m \
  ../property/PropertyImplSetter.m \
  ../property/Property_getter.m \
  ../property/aclass.m \
  ../property/main_car.m \
  ../property_in_protocol/Test.m \
  ../protocol/protocol.m \
  ../returnstmt/void_return.m \
  ../self_static/Self.m \
  ../self_static/static.m \
  ../shared/block/block-it.m \
  ../shared/block/block.m \
  ../shared/block/block_no_args.m \
  ../shared/block/block_release.m \
  ../shared/block/dispatch.m \
  ../shared/block/dispatch_examples.m \
  ../shared/block/dispatch_in_macro.m \
  ../shared/category_procdesc/EOCPerson.m \
  ../shared/category_procdesc/main.c \
  ../shared/field_superclass/SuperExample.m \
  ../shared/memory_leaks_benchmark/AutoreleaseExample.m \
  ../shared/memory_leaks_benchmark/MemoryLeakExample.m \
  ../shared/memory_leaks_benchmark/RetainReleaseExample.m \
  ../shared/memory_leaks_benchmark/RetainReleaseExample2.m \
  ../shared/memory_leaks_benchmark/TollBridgeExample.m \
  ../shared/npe/npe_malloc.m \
  ../shared/property/GetterExample.m \
  ../shared/property/PropertyAttributes.m \
  ../shared/protocol_procdesc/Bicycle.m \
  ../shared/protocol_procdesc/main.c \
  ../strings/global_string_literal.m \
  ../strings/string_literal.m \
  ../subclass/MyClass.m \
  ../subclass/MySubClass.m \
  ../subclass/main.c \
  ../types/testloop.m \
  ../vardecl/aclass.m \
  ../vardecl/aclass_2.m \
  ../vardecl/last_af.m \
	../shared/annotations/nullable_annotations.m \

include $(TESTS_DIR)/clang-frontend.make
