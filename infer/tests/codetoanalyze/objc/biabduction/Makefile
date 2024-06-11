# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

ROOT_DIR = ../../../../..
include $(ROOT_DIR)/Makefile.config

SOURCES_DEFAULT = \
  memory_leaks_benchmark/NSString_models_tests.m \
  npe/dynamic_dispatch.m \
  npe/block.m \
  npe/ivar_blocks.m \
  npe/skip_method_with_nil_object.m \
  npe/Nsstring_length_no_npe.m \
  npe/Nsstring_nil_args.m \
  npe/No_null_from_array.m \
  procdescs/MethodCall.m \
  property/main.c \
  resource_leaks/ResourceLeakExample.m \
  resource_leaks/Dispatch_sources.m \
  shared/block/block-it.m \
  shared/block/dispatch.m \
  shared/memory_leaks_benchmark/ArcExample.m \
  shared/memory_leaks_benchmark/AutoreleaseExample.m \
  shared/memory_leaks_benchmark/RetainReleaseExample.m \
  shared/memory_leaks_benchmark/arc_methods.m \
  shared/npe/Available_expr.m \
  shared/npe/Nonnull_attribute_example.m \
  shared/property/GetterExample.m \
  shared/property/PropertyAttributes.m \
  shared/protocol_procdesc/Bicycle.m \
  shared/protocol_procdesc/main.c \
  shared/annotations/nullable_annotations.m \
  shared/annotations/nullable_annotations_fields.m \
  shared/annotations/nonnull_annotations.m \

SOURCES_BUCKET_ALL = \
  global_const/global_const.m \
  npe/null_returned_by_method.m \
  procdescs/main.c \
  property/main.c \
  shared/assertions/NSAssert_example.m \
  shared/block/BlockVar.m \
  shared/block/AnonymousBlock.m \
  shared/block/block.m \
  shared/block/block_no_args.m \
  shared/block/block_release.m \
  shared/block/dispatch_in_macro.m \
  shared/block/Blocks_as_parameters.m \
  shared/field_superclass/SuperExample.m \
  shared/npe/npe_malloc.m \
  warnings/ParameterNotNullableExample.m \

SOURCES_ARC = \
  npe/BoxedNumberExample.m \
  npe/ObjCMethodCallInCondition.m \
  npe/UpdateDict.m \
  npe/nil_in_array_literal.m \
  npe/nil_in_dictionary_literal.m \
  npe/npe_conditional.m \
  npe/npe_self.m \
  npe/nullable.m \
  specialized_methods_with_blocks/BlockAsReceiver.m \
  variadic_methods/premature_nil_termination.m \

.PHONY: default
default: analyze

.PHONY: analyze print replace test clean
analyze print replace test clean:
	$(QUIET)$(MAKE) -f Makefile.common TEST_SUB_TITLE="(bucket-all)" TEST_SUFFIX=-all \
	  TEST_RESULT_SUFFIX=-all MEMLEAK_BUCKET=all SOURCES="$(SOURCES_BUCKET_ALL)" $@
	$(QUIET)$(MAKE) -f Makefile.common TEST_SUB_TITLE="(arc)" TEST_SUFFIX=-arc \
	  TEST_RESULT_SUFFIX=-arc MEMLEAK_BUCKET=cf FOBJC_ARC=-fobjc-arc SOURCES="$(SOURCES_ARC)" $@
	$(QUIET)$(MAKE) -f Makefile.common TEST_SUB_TITLE="(default)" MEMLEAK_BUCKET=cf \
	  SOURCES="$(SOURCES_DEFAULT)" $@
