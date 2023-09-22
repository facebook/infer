# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../../..

CLANG_OPTIONS = -c $(OBJC_CLANG_OPTIONS)

SOURCES = \
  ../block/retain_cycle.m \
  ../block/static.m \
  ../block/specialized_method_with_block_params.m \
  ../block/escaping_byref.m \
  ../boxing/Boxing.m \
  ../boxing/array.m \
  ../boxing/array_literal.c \
  ../boxing/dict_literal.c \
  ../boxing/string_literal.c \
  ../conditional_operation/ConditionalOperation.m \
  ../exceptions/ExceptionExample.m \
  ../fast_enumeration/Fast_enumeration.m \
	../internal_forward_class/ForwardClassInMethod.m \
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
  ../shared/block/dispatch_in_macro.m \
  ../shared/field_superclass/SuperExample.m \
  ../shared/memory_leaks_benchmark/AutoreleaseExample.m \
  ../shared/memory_leaks_benchmark/RetainReleaseExample.m \
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
	../shared/annotations/nonnull_annotations.m \

include $(TESTS_DIR)/clang-frontend.make
include $(TESTS_DIR)/objc.make

CLEAN_EXTRA += *.o ../*/*.test.dot ../*/*/*.test.dot
