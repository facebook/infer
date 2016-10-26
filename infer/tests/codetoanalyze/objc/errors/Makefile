# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

include ../../Makefile.clang

IPHONESIMULATOR_ISYSROOT_SUFFIX = /Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk

XCODEROOT = $(shell xcode-select -p)

OPTIONS = -x objective-c \
  -isysroot $(XCODEROOT)$(IPHONESIMULATOR_ISYSROOT_SUFFIX) \
  -mios-simulator-version-min=8.2 --target=x86_64-apple-darwin14 -c \


FILES = \
  field_superclass/B.m \
  memory_leaks_benchmark/FBViewExample.m \
  memory_leaks_benchmark/MemoryLeakRaii.m \
  memory_leaks_benchmark/NSMakeCollectableExample.m \
  memory_leaks_benchmark/NSStringInitWithBytesNoCopyExample.m \
  memory_leaks_benchmark/RetainReleaseExampleBucketing.m \
  npe/Fraction.m \
  npe/NPD_core_foundation.m \
  npe/Npe_with_equal_names.m \
  npe/block.m \
  npe/skip_method_with_nil_object.m \
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
  taint/sources.m \
  taint/viewController.m \

FILES_BUCKET_ALL = \
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

FILES_ARC = \
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

compile:
	clang $(OPTIONS) $(FILES) $(FILES_BUCKET_ALL)
	clang $(OPTIONS) -fobjc-arc $(FILES_ARC)

CLEAN_EXTRA = infer-out-arc infer-out-all

analyze:
	$(call silent_on_success,$(INFER_BIN) -a $(ANALYZER) --cxx --ml-buckets all --check-duplicate-symbols -o infer-out-all -- clang $(OPTIONS) $(FILES_BUCKET_ALL) 2>duplicates.txt)
	grep "DUPLICATE_SYMBOLS" duplicates.txt; test $$? -ne 0
	$(call silent_on_success,$(INFER_BIN) -a $(ANALYZER) --cxx --ml-buckets cf --check-duplicate-symbols -o infer-out -- clang $(OPTIONS) $(FILES) 2>duplicates.txt)
	grep "DUPLICATE_SYMBOLS" duplicates.txt; test $$? -ne 0
	$(call silent_on_success,$(INFER_BIN) -a $(ANALYZER) --cxx --ml-buckets cf --check-duplicate-symbols -o infer-out-arc -- clang $(OPTIONS) -fobjc-arc $(FILES_ARC) 2>duplicates.txt)
	grep "DUPLICATE_SYMBOLS" duplicates.txt; test $$? -ne 0
	cp infer-out-all/specs/* infer-out/specs
	cp infer-out-arc/specs/* infer-out/specs
