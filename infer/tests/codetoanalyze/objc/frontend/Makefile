# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

include ../../Makefile.frontend

IPHONESIMULATOR_ISYSROOT_SUFFIX = /Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk

XCODEROOT = $(shell xcode-select -p)

OPTIONS = -x objective-c \
  -isysroot $(XCODEROOT)$(IPHONESIMULATOR_ISYSROOT_SUFFIX) \
  -mios-simulator-version-min=8.2 --target=x86_64-apple-darwin14 -c \

FILES_NOARC = \
  block/BlockVar.m \
  block/block-it.m \
  block/block.m \
  block/block_no_args.m \
  block/block_release.m \
  block/dispatch.m \
  block/dispatch_examples.m \
  block/retain_cycle.m \
  block/static.m \
  boxing/Boxing.m \
  boxing/array.m \
  boxing/array_literal.c \
  boxing/dict_literal.c \
  boxing/string_literal.c \
  conditional_operation/ConditionalOperation.m \
  exceptions/ExceptionExample.m \
  fast_enumeration/Fast_enumeration.m \
  property/PropertyAttributes.m \
  property/PropertyCustomAccessor.m \
  property/PropertyImplSetter.m \
  property/Property_getter.m \
  property/aclass.m \
  property/main_car.m \
  property_in_protocol/Test.m \
  protocol/protocol.m \
  returnstmt/void_return.m \
  self_static/Self.m \
  self_static/static.m \
  strings/global_string_literal.m \
  strings/string_literal.m \
  subclass/MyClass.m \
  subclass/MySubClass.m \
  subclass/main.c \
  types/testloop.m \
  vardecl/aclass.m \
  vardecl/aclass_2.m \
  vardecl/last_af.m \
  link_to_errors/category_procdesc/EOCPerson.m \
  link_to_errors/category_procdesc/main.c \
  link_to_errors/field_superclass/SuperExample.m \
  link_to_errors/memory_leaks_benchmark/AutoreleaseExample.m \
  link_to_errors/memory_leaks_benchmark/MemoryLeakExample.m \
  link_to_errors/memory_leaks_benchmark/RetainReleaseExample.m \
  link_to_errors/memory_leaks_benchmark/RetainReleaseExample2.m \
  link_to_errors/memory_leaks_benchmark/TollBridgeExample.m \
  link_to_errors/npe/npe_malloc.m \
  link_to_errors/protocol_procdesc/main.c \

FILES_ARC = \
  assertions/NSAssert_example.m \
  predefined_expr/PredefinedExprExample.m \
  types/attributes.m \
  types/void_call.m \
  vardecl/initlist.m \
  link_to_errors/npe/Nonnull_attribute_example.m \
  link_to_errors/memory_leaks_benchmark/ArcExample.m \
  link_to_errors/memory_leaks_benchmark/arc_methods.m \

FILES = \
  $(FILES_NOARC) \
  $(FILES_ARC) \

compile:
	clang $(OPTIONS) $(FILES_NOARC)
	clang $(OPTIONS) -fobjc-arc $(FILES_ARC)

capture:
	infer -a capture --cxx --frontend-tests --continue --reactive -- clang $(OPTIONS) $(FILES_NOARC)
	infer -a capture --cxx --frontend-tests -- clang $(OPTIONS) -fobjc-arc $(FILES_ARC)



