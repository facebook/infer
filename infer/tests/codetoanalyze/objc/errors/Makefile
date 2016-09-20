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
  category_procdesc/EOCPerson.m \
  field_superclass/B.m \
  frontend/dispatch.m \
  frontend/GetterExample.m \
  frontend/block-it.m \
  frontend/PropertyAttributes.m \
  memory_leaks_benchmark/ArcExample.m \
  memory_leaks_benchmark/AutoreleaseExample.m \
  memory_leaks_benchmark/FBViewExample.m \
  memory_leaks_benchmark/MemoryLeakExample.m \
  memory_leaks_benchmark/MemoryLeakRaii.m \
  memory_leaks_benchmark/NSMakeCollectableExample.m \
  memory_leaks_benchmark/NSStringInitWithBytesNoCopyExample.m \
  memory_leaks_benchmark/RetainReleaseExample.m \
  memory_leaks_benchmark/RetainReleaseExampleBucketing.m \
  memory_leaks_benchmark/arc_methods.m \
  npe/Fraction.m \
  npe/NPD_core_foundation.m \
  npe/Nonnull_attribute_example.m \
  npe/Npe_with_equal_names.m \
  npe/block.m \
  npe/skip_method_with_nil_object.m \
  procdescs/MethodCall.m \
  property/main.c \
  protocol_procdesc/Bicycle.m \
  protocol_procdesc/main.c \
  resource_leaks/ResourceLeakExample.m \
  taint/sources.m \
  taint/viewController.m \

FILES_BUCKET_ALL = \
  frontend/BlockVar.m \
  frontend/NSAssert_example.m \
  frontend/block.m \
  frontend/block_no_args.m \
  frontend/block_release.m \
  frontend/compound_literal.c \
  frontend/dispatch_in_macro.m \
  category_procdesc/main.c \
  field_superclass/SuperExample.m \
  field_superclass/field.c \
  global_const/global_const.m \
  memory_leaks_benchmark/CADisplayLinkRetainCycle.m \
  memory_leaks_benchmark/RetainCycleStaticVar.m \
  memory_leaks_benchmark/RetainReleaseExample2.m \
  npe/blockenum.m \
  npe/nil_param.m \
  npe/npe_malloc.m \
  npe/null_returned_by_method.m \
  procdescs/main.c \
  property/main.c \
  returnstmt/return_npe_test.m \
  warnings/ParameterNotNullableExample.m \

FILES_ARC = \
  field_superclass/SubtypingExample.m \
  frontend/dispatch_examples.m \
  frontend/struct_initlistexpr.c \
  memory_leaks_benchmark/RetainReleaseExampleBucketingArc.m \
  memory_leaks_benchmark/TollBridgeExample.m \
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
  subtyping/KindOfClassExample.m \
  variadic_methods/premature_nil_termination.m \

compile:
	clang $(OPTIONS) $(FILES) $(FILES_BUCKET_ALL)
	clang $(OPTIONS) -fobjc-arc $(FILES_ARC)

CLEAN_EXTRA = infer-out-arc infer-out-all

analyze:
	$(INFER_BIN) -a $(ANALYZER) --cxx --ml-buckets all --check-duplicate-symbols -o infer-out-all -- clang $(OPTIONS) $(FILES_BUCKET_ALL) >/dev/null 2>duplicates.txt
	grep "DUPLICATE_SYMBOLS" duplicates.txt; test $$? -ne 0
	$(INFER_BIN) -a $(ANALYZER) --cxx --ml-buckets cf --check-duplicate-symbols -o infer-out -- clang $(OPTIONS) $(FILES) >/dev/null 2>duplicates.txt
	grep "DUPLICATE_SYMBOLS" duplicates.txt; test $$? -ne 0
	$(INFER_BIN) -a $(ANALYZER) --cxx --ml-buckets cf --check-duplicate-symbols -o infer-out-arc -- clang $(OPTIONS) -fobjc-arc $(FILES_ARC) >/dev/null 2>duplicates.txt
	grep "DUPLICATE_SYMBOLS" duplicates.txt; test $$? -ne 0
	cp infer-out-all/specs/* infer-out/specs
	cp infer-out-arc/specs/* infer-out/specs
