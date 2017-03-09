# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../../..

IPHONESIMULATOR_ISYSROOT_SUFFIX = /Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk

XCODEROOT = $(shell xcode-select -p)

ANALYZER = infer
CLANG_OPTIONS = -x objective-c \
  -isysroot $(XCODEROOT)$(IPHONESIMULATOR_ISYSROOT_SUFFIX) \
  -mios-simulator-version-min=8.2 --target=x86_64-apple-darwin14 -c \

CLEAN_EXTRA = infer-out-arc infer-out-all infer-out-all infer-out-arc \
              issues.exp.test.all issues.exp.test.arc issues.exp.test.default
INFER_OPTIONS = --check-duplicate-symbols --no-filtering --debug-exceptions --project-root $(TESTS_DIR) --no-failures-allowed
INFERPRINT_OPTIONS = --issues-tests

SOURCES_DEFAULT = \
  field_superclass/B.m \
  memory_leaks_benchmark/MemoryLeakRaii.m \
  memory_leaks_benchmark/NSMakeCollectableExample.m \
  memory_leaks_benchmark/NSString_models_tests.m \
	memory_leaks_benchmark/NSData_models_tests.m \
  memory_leaks_benchmark/RetainReleaseExampleBucketing.m \
  npe/Fraction.m \
  npe/NPD_core_foundation.m \
  npe/Npe_with_equal_names.m \
  npe/block.m \
  npe/skip_method_with_nil_object.m \
	npe/Nsstring_length_no_npe.m \
	npe/No_null_from_array.m \
  procdescs/MethodCall.m \
  property/main.c \
  resource_leaks/ResourceLeakExample.m \
  shared/block/block-it.m \
  shared/block/dispatch.m \
  shared/category_procdesc/EOCPerson.m \
  shared/memory_leaks_benchmark/ArcExample.m \
  shared/memory_leaks_benchmark/AutoreleaseExample.m \
  shared/memory_leaks_benchmark/MemoryLeakExample.m \
  shared/memory_leaks_benchmark/RetainReleaseExample.m \
  shared/memory_leaks_benchmark/arc_methods.m \
  shared/npe/Nonnull_attribute_example.m \
  shared/property/GetterExample.m \
  shared/property/PropertyAttributes.m \
  shared/protocol_procdesc/Bicycle.m \
  shared/protocol_procdesc/main.c \
	shared/annotations/nullable_annotations.m \
	shared/annotations/nullable_annotations_fields.m \
  taint/sources.m \
  taint/viewController.m \

SOURCES_BUCKET_ALL = \
  field_superclass/field.c \
  global_const/global_const.m \
  initialization/compound_literal.c \
  memory_leaks_benchmark/CADisplayLinkRetainCycle.m \
  memory_leaks_benchmark/RetainCycleStaticVar.m \
  npe/blockenum.m \
  npe/nil_param.m \
  npe/null_returned_by_method.m \
  procdescs/main.c \
  property/main.c \
  returnstmt/return_npe_test.m \
  shared/assertions/NSAssert_example.m \
  shared/block/BlockVar.m \
  shared/block/block.m \
  shared/block/block_no_args.m \
  shared/block/block_release.m \
  shared/block/dispatch_in_macro.m \
  shared/category_procdesc/main.c \
  shared/field_superclass/SuperExample.m \
  shared/memory_leaks_benchmark/RetainReleaseExample2.m \
  shared/npe/npe_malloc.m \
  warnings/ParameterNotNullableExample.m \

SOURCES_ARC = \
  field_superclass/SubtypingExample.m \
  initialization/struct_initlistexpr.c \
  memory_leaks_benchmark/RetainReleaseExampleBucketingArc.m \
  memory_leaks_benchmark/retain_cycle.m \
  memory_leaks_benchmark/retain_cycle2.m \
  npe/BoxedNumberExample.m \
  npe/ObjCMethodCallInCondition.m \
  npe/UpdateDict.m \
  npe/WeakCapturedVarsNPE.m \
  npe/nil_in_array_literal.m \
  npe/nil_in_dictionary_literal.m \
  npe/npe_conditional.m \
  npe/npe_self.m \
  npe/nullable.m \
  property/ExplicitIvarName.m \
  shared/block/dispatch_examples.m \
  shared/memory_leaks_benchmark/TollBridgeExample.m \
  subtyping/KindOfClassExample.m \
  variadic_methods/premature_nil_termination.m \

include $(TESTS_DIR)/clang.make

OBJECTS_ARC = $(foreach source,$(SOURCES_ARC),$(basename $(source)).o)
OBJECTS_BUCKET_ALL = $(foreach source,$(SOURCES_BUCKET_ALL),$(basename $(source)).o)
OBJECTS_DEFAULT = $(foreach source,$(SOURCES_DEFAULT),$(basename $(source)).o)

$(OBJECTS_ARC): $(SOURCES_ARC)
	clang $(CLANG_OPTIONS) -fobjc-arc $*

$(OBJECTS_BASE) $(OBJECTS_BUCKETS_ALL): $(SOURCES_BASE) $(SOURCES_BUCKET_ALL)
	clang $(CLANG_OPTIONS) $*

infer-out-all/report.json: $(CLANG_DEPS) $(SOURCES_BUCKET_ALL)
	$(call silent_on_success,\
	  $(INFER_BIN) -a $(ANALYZER) $(INFER_OPTIONS) --ml-buckets all -o infer-out-all -- \
	  clang $(CLANG_OPTIONS) $(SOURCES_BUCKET_ALL) 2>duplicates.txt)
	grep "DUPLICATE_SYMBOLS" duplicates.txt; test $$? -ne 0

infer-out-arc/report.json: $(CLANG_DEPS) $(SOURCES_ARC)
	$(call silent_on_success,\
	  $(INFER_BIN) -a $(ANALYZER) $(INFER_OPTIONS) --ml-buckets cf -o infer-out-arc -- \
	  clang $(CLANG_OPTIONS) -fobjc-arc $(SOURCES_ARC) 2>duplicates.txt)
	grep "DUPLICATE_SYMBOLS" duplicates.txt; test $$? -ne 0

infer-out/report.json: $(CLANG_DEPS) $(SOURCES_DEFAULT)
	$(call silent_on_success,\
	  $(INFER_BIN) -a $(ANALYZER) $(INFER_OPTIONS) --ml-buckets cf -o infer-out -- \
	  clang $(CLANG_OPTIONS) $(SOURCES_DEFAULT) 2>duplicates.txt)
	grep "DUPLICATE_SYMBOLS" duplicates.txt; test $$? -ne 0

issues.exp.test: infer-out-all/report.json infer-out-arc/report.json infer-out/report.json
	$(INFERPRINT_BIN) -q -a $(ANALYZER) $(INFERPRINT_OPTIONS) $@.all \
	  --from-json-report infer-out-all/report.json
	$(INFERPRINT_BIN) -q -a $(ANALYZER) $(INFERPRINT_OPTIONS) $@.arc \
	  --from-json-report infer-out-arc/report.json
	$(INFERPRINT_BIN) -q -a $(ANALYZER) $(INFERPRINT_OPTIONS) $@.default \
	  --from-json-report infer-out/report.json
	cat $@.all $@.arc $@.default > $@
