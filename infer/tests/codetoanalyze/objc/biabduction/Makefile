# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

CLANG_OPTIONS = -c $(OBJC_CLANG_OPTIONS)

CLEAN_EXTRA = infer-out-arc infer-out-all infer-out-all infer-out-arc \
              issues.exp.test.all issues.exp.test.arc issues.exp.test.default
INFER_OPTIONS = --biabduction-only --dump-duplicate-symbols --debug-exceptions --project-root $(TESTS_DIR)
INFERPRINT_OPTIONS = --issues-tests

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
  shared/category_procdesc/EOCPerson.m \
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
  shared/category_procdesc/main.c \
  shared/field_superclass/SuperExample.m \
  shared/npe/npe_malloc.m \
  warnings/ParameterNotNullableExample.m \

SOURCES_ARC = \
  retain_cycles/*.m \
  npe/BoxedNumberExample.m \
  npe/ObjCMethodCallInCondition.m \
  npe/UpdateDict.m \
  npe/nil_in_array_literal.m \
  npe/nil_in_dictionary_literal.m \
  npe/npe_conditional.m \
  npe/npe_self.m \
  npe/nullable.m \
  shared/block/dispatch_examples.m \
  specialized_methods_with_blocks/BlockAsReceiver.m \
  subtyping/KindOfClassExample.m \
  variadic_methods/premature_nil_termination.m \

include $(TESTS_DIR)/clang.make
include $(TESTS_DIR)/objc.make

OBJECTS_ARC = $(foreach source,$(SOURCES_ARC),$(basename $(source)).o)
OBJECTS_BUCKET_ALL = $(foreach source,$(SOURCES_BUCKET_ALL),$(basename $(source)).o)
OBJECTS_DEFAULT = $(foreach source,$(SOURCES_DEFAULT),$(basename $(source)).o)

$(OBJECTS_ARC): $(SOURCES_ARC)
	clang $(CLANG_OPTIONS) -fobjc-arc $*

$(OBJECTS_BASE) $(OBJECTS_BUCKETS_ALL): $(SOURCES_BASE) $(SOURCES_BUCKET_ALL)
	clang $(CLANG_OPTIONS) $*

infer-out-all/report.json: $(CLANG_DEPS) $(SOURCES_BUCKET_ALL)
	$(QUIET)$(call silent_on_success,Testing infer/Objective-C with all memleak buckets,\
	  $(INFER_BIN) $(INFER_OPTIONS) --biabduction-memleak-buckets all -o infer-out-all -- \
	  clang $(CLANG_OPTIONS) $(SOURCES_BUCKET_ALL))

infer-out-arc/report.json: $(CLANG_DEPS) $(SOURCES_ARC)
	$(QUIET)$(call silent_on_success,Testing infer/Objective-C with arc memleak buckets,\
	  $(INFER_BIN) $(INFER_OPTIONS) --biabduction-memleak-buckets cf -o infer-out-arc -- \
	  clang $(CLANG_OPTIONS) -fobjc-arc $(SOURCES_ARC))

infer-out/report.json: $(CLANG_DEPS) $(SOURCES_DEFAULT)
	$(QUIET)$(call silent_on_success,Testing infer/Objective-C with CF memleak buckets,\
	  $(INFER_BIN) $(INFER_OPTIONS) --biabduction-memleak-buckets cf -o infer-out -- \
	  clang $(CLANG_OPTIONS) $(SOURCES_DEFAULT))

issues.exp.test: infer-out-all/report.json infer-out-arc/report.json infer-out/report.json
	$(QUIET)$(INFER_BIN) report -q $(INFERPRINT_OPTIONS) $@.all \
	  -o infer-out-all
	$(QUIET)$(INFER_BIN) report -q $(INFERPRINT_OPTIONS) $@.arc \
	  -o infer-out-arc
	$(QUIET)$(INFER_BIN) report -q $(INFERPRINT_OPTIONS) $@.default \
	  -o infer-out
	$(QUIET)cat $@.all $@.arc $@.default > $@
	$(QUIET)cat infer-out-all/duplicates.txt infer-out-arc/duplicates.txt >> infer-out/duplicates.txt
