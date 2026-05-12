; LLVM IR captured from a buck2 build of the source-level witness at
; fbobjc/Tools/infer/tests/codetoanalyze/swift/pulse/AsyncActorGetter/RcAsyncActorGetter.swift.
;
; Source:
;
;     actor AAGHandler {
;         var utils: AAGUtils
;         func read_utils_no_cycle_good_FP() async -> Int { return utils.counter }
;     }
;
; When this is built through the fbobjc swiftc/buck2 toolchain, the async
; method body is lowered to a continuation state-machine struct that holds
; captured copies of `self` at each await suspension point. Pulse's heap
; model aliases the continuation-frame's captured-self slot with the
; original `self.field` heap location, so when the continuation later
; writes back through `self`, the analyser sees a self-referential store
; and reports a fictional retain cycle on `self->field_0->field_2`.
;
; Pairing notes:
;   - Companion `.swift.source` MUST name the EXACT path that appears in
;     this file's `!DIFile` debug entry — otherwise
;     `Llair2Textual.should_translate` filters out the async function
;     bodies (the synth-accessor name-suffix fallback only catches
;     `get`/`set`/`modify`, not the async method itself).
;   - Captured artifact is stripped of `target-cpu`, `target-features`,
;     and absolute path metadata to keep the file machine-independent;
;     debug metadata is retained because the SIL frontend uses it to
;     recover Swift-level type and parameter names AND to gate
;     translation by source-file match.

; ModuleID = 'AsyncActorGetter.o'
source_filename = "AsyncActorGetter.o"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-ios15.1.0-simulator"

%swift.async_func_pointer = type <{ i32, i32 }>
%swift.protocol_conformance_descriptor = type { i32, i32, i32, i32 }
%objc_class = type { ptr, ptr, ptr, ptr, i64 }
%swift.opaque = type opaque
%swift.method_descriptor = type { i32, i32 }
%swift.protocol = type { ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i32, i32, i32, i32, i32, i32 }
%swift.type_metadata_record = type { i32 }
%swift.type = type { i64 }
%T16AsyncActorGetter8AAGUtilsC = type <{ %swift.refcounted, %TSi }>
%swift.refcounted = type { ptr, i64 }
%TSi = type <{ i64 }>
%T16AsyncActorGetter10AAGHandlerC = type <{ %swift.refcounted, %swift.defaultactor, ptr }>
%swift.defaultactor = type { [12 x ptr] }
%"$s16AsyncActorGetter10AAGHandlerC5utilsAA8AAGUtilsCvM.Frame" = type { [24 x i8] }
%"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaF.Frame" = type { [24 x i8], [24 x i8], ptr, ptr, ptr }
%"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame" = type { ptr, ptr, ptr, i64, i64, ptr, ptr }
%swift.metadata_response = type { ptr, i64 }

@"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaFTu" = hidden global %swift.async_func_pointer <{ i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaF" to i64), i64 ptrtoint (ptr @"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaFTu" to i64)) to i32), i32 96 }>, align 8
@"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaFTu" = global %swift.async_func_pointer <{ i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF" to i64), i64 ptrtoint (ptr @"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaFTu" to i64)) to i32), i32 80 }>, align 8
@"$sS2cMScAsWL" = linkonce_odr hidden global ptr null, align 8
@"$sScMScAsMc" = external global %swift.protocol_conformance_descriptor, align 4
@"$s16AsyncActorGetter8AAGUtilsC7counterSivpWvd" = hidden constant i64 16, align 8
@"$sBoWV" = external global ptr, align 8
@"$s16AsyncActorGetter8AAGUtilsCMm" = hidden global %objc_class { ptr @"OBJC_METACLASS_$__TtCs12_SwiftObject", ptr @"OBJC_METACLASS_$__TtCs12_SwiftObject", ptr @_objc_empty_cache, ptr null, i64 ptrtoint (ptr @_METACLASS_DATA__TtC16AsyncActorGetter8AAGUtils to i64) }, align 8
@"OBJC_CLASS_$__TtCs12_SwiftObject" = external global %objc_class, align 8
@_objc_empty_cache = external global %swift.opaque
@"OBJC_METACLASS_$__TtCs12_SwiftObject" = external global %objc_class, align 8
@.str.31._TtC16AsyncActorGetter8AAGUtils = private unnamed_addr constant [32 x i8] c"_TtC16AsyncActorGetter8AAGUtils\00", section "__TEXT,__objc_classname,cstring_literals"
@_METACLASS_DATA__TtC16AsyncActorGetter8AAGUtils = internal constant { i32, i32, i32, i32, ptr, ptr, ptr, ptr, ptr, ptr, ptr } { i32 129, i32 40, i32 40, i32 0, ptr null, ptr @.str.31._TtC16AsyncActorGetter8AAGUtils, ptr null, ptr null, ptr null, ptr null, ptr null }, section "__DATA, __objc_const", align 8
@.str.7.counter = private unnamed_addr constant [8 x i8] c"counter\00", section "__TEXT,__objc_methname,cstring_literals"
@.str.0. = private unnamed_addr constant [1 x i8] zeroinitializer, section "__TEXT,__objc_methtype,cstring_literals"
@_IVARS__TtC16AsyncActorGetter8AAGUtils = internal constant { i32, i32, [1 x { ptr, ptr, ptr, i32, i32 }] } { i32 32, i32 1, [1 x { ptr, ptr, ptr, i32, i32 }] [{ ptr, ptr, ptr, i32, i32 } { ptr @"$s16AsyncActorGetter8AAGUtilsC7counterSivpWvd", ptr @.str.7.counter, ptr @.str.0., i32 3, i32 8 }] }, section "__DATA, __objc_const", align 8
@_DATA__TtC16AsyncActorGetter8AAGUtils = internal constant { i32, i32, i32, i32, ptr, ptr, ptr, ptr, ptr, ptr, ptr } { i32 128, i32 16, i32 24, i32 0, ptr null, ptr @.str.31._TtC16AsyncActorGetter8AAGUtils, ptr null, ptr null, ptr @_IVARS__TtC16AsyncActorGetter8AAGUtils, ptr null, ptr null }, section "__DATA, __objc_const", align 8
@.str.16.AsyncActorGetter = private constant [17 x i8] c"AsyncActorGetter\00"
@"$s16AsyncActorGetterMXM" = linkonce_odr hidden constant <{ i32, i32, i32 }> <{ i32 0, i32 0, i32 trunc (i64 sub (i64 ptrtoint (ptr @.str.16.AsyncActorGetter to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32 }>, ptr @"$s16AsyncActorGetterMXM", i32 0, i32 2) to i64)) to i32) }>, section "__TEXT,__constg_swiftt", align 4
@.str.8.AAGUtils = private constant [9 x i8] c"AAGUtils\00"
@"$s16AsyncActorGetter8AAGUtilsCMn" = hidden constant <{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor }> <{ i32 -2147483568, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16AsyncActorGetterMXM" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor }>, ptr @"$s16AsyncActorGetter8AAGUtilsCMn", i32 0, i32 1) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @.str.8.AAGUtils to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor }>, ptr @"$s16AsyncActorGetter8AAGUtilsCMn", i32 0, i32 2) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16AsyncActorGetter8AAGUtilsCMa" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor }>, ptr @"$s16AsyncActorGetter8AAGUtilsCMn", i32 0, i32 3) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16AsyncActorGetter8AAGUtilsCMF" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor }>, ptr @"$s16AsyncActorGetter8AAGUtilsCMn", i32 0, i32 4) to i64)) to i32), i32 0, i32 3, i32 12, i32 2, i32 1, i32 10, i32 11, i32 1, %swift.method_descriptor { i32 1, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16AsyncActorGetter8AAGUtilsCACycfC" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor }>, ptr @"$s16AsyncActorGetter8AAGUtilsCMn", i32 0, i32 13, i32 1) to i64)) to i32) } }>, section "__TEXT,__constg_swiftt", align 4
@"$s16AsyncActorGetter8AAGUtilsCMf" = internal global <{ ptr, ptr, ptr, i64, ptr, ptr, ptr, i64, i32, i32, i32, i16, i16, i32, i32, ptr, ptr, i64, ptr }> <{ ptr null, ptr @"$s16AsyncActorGetter8AAGUtilsCfD", ptr @"$sBoWV", i64 ptrtoint (ptr @"$s16AsyncActorGetter8AAGUtilsCMm" to i64), ptr @"OBJC_CLASS_$__TtCs12_SwiftObject", ptr @_objc_empty_cache, ptr null, i64 add (i64 ptrtoint (ptr @_DATA__TtC16AsyncActorGetter8AAGUtils to i64), i64 2), i32 2, i32 0, i32 24, i16 7, i16 0, i32 120, i32 24, ptr @"$s16AsyncActorGetter8AAGUtilsCMn", ptr null, i64 16, ptr @"$s16AsyncActorGetter8AAGUtilsCACycfC" }>, align 8
@"symbolic _____ 16AsyncActorGetter8AAGUtilsC" = linkonce_odr hidden constant <{ i8, i32, i8 }> <{ i8 1, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16AsyncActorGetter8AAGUtilsCMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, i8 }>, ptr @"symbolic _____ 16AsyncActorGetter8AAGUtilsC", i32 0, i32 1) to i64)) to i32), i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@"symbolic Si" = linkonce_odr hidden constant <{ [2 x i8], i8 }> <{ [2 x i8] c"Si", i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@0 = private constant [8 x i8] c"counter\00", section "__TEXT,__swift5_reflstr, regular", no_sanitize_address
@"$s16AsyncActorGetter8AAGUtilsCMF" = internal constant { i32, i32, i16, i16, i32, i32, i32, i32 } { i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____ 16AsyncActorGetter8AAGUtilsC" to i64), i64 ptrtoint (ptr @"$s16AsyncActorGetter8AAGUtilsCMF" to i64)) to i32), i32 0, i16 1, i16 12, i32 1, i32 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic Si" to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32 }, ptr @"$s16AsyncActorGetter8AAGUtilsCMF", i32 0, i32 6) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @0 to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32 }, ptr @"$s16AsyncActorGetter8AAGUtilsCMF", i32 0, i32 7) to i64)) to i32) }, section "__TEXT,__swift5_fieldmd, regular", no_sanitize_address, align 4
@"$s16AsyncActorGetter10AAGHandlerC5utilsAA8AAGUtilsCvpWvd" = hidden constant i64 112, align 8
@"$s16AsyncActorGetter10AAGHandlerCMm" = hidden global %objc_class { ptr @"OBJC_METACLASS_$__TtCs12_SwiftObject", ptr @"OBJC_METACLASS_$__TtCs12_SwiftObject", ptr @_objc_empty_cache, ptr null, i64 ptrtoint (ptr @_METACLASS_DATA__TtC16AsyncActorGetter10AAGHandler to i64) }, align 8
@.str.34._TtC16AsyncActorGetter10AAGHandler = private unnamed_addr constant [35 x i8] c"_TtC16AsyncActorGetter10AAGHandler\00", section "__TEXT,__objc_classname,cstring_literals"
@_METACLASS_DATA__TtC16AsyncActorGetter10AAGHandler = internal constant { i32, i32, i32, i32, ptr, ptr, ptr, ptr, ptr, ptr, ptr } { i32 1153, i32 40, i32 40, i32 0, ptr null, ptr @.str.34._TtC16AsyncActorGetter10AAGHandler, ptr null, ptr null, ptr null, ptr null, ptr null }, section "__DATA, __objc_const", align 8
@".str.13.$defaultActor" = private unnamed_addr constant [14 x i8] c"$defaultActor\00", section "__TEXT,__objc_methname,cstring_literals"
@.str.5.utils = private unnamed_addr constant [6 x i8] c"utils\00", section "__TEXT,__objc_methname,cstring_literals"
@_IVARS__TtC16AsyncActorGetter10AAGHandler = internal constant { i32, i32, [2 x { ptr, ptr, ptr, i32, i32 }] } { i32 32, i32 2, [2 x { ptr, ptr, ptr, i32, i32 }] [{ ptr, ptr, ptr, i32, i32 } { ptr null, ptr @".str.13.$defaultActor", ptr @.str.0., i32 4, i32 96 }, { ptr, ptr, ptr, i32, i32 } { ptr @"$s16AsyncActorGetter10AAGHandlerC5utilsAA8AAGUtilsCvpWvd", ptr @.str.5.utils, ptr @.str.0., i32 3, i32 8 }] }, section "__DATA, __objc_const", align 8
@_DATA__TtC16AsyncActorGetter10AAGHandler = internal constant { i32, i32, i32, i32, ptr, ptr, ptr, ptr, ptr, ptr, ptr } { i32 1152, i32 16, i32 120, i32 0, ptr null, ptr @.str.34._TtC16AsyncActorGetter10AAGHandler, ptr null, ptr null, ptr @_IVARS__TtC16AsyncActorGetter10AAGHandler, ptr null, ptr null }, section "__DATA, __objc_const", align 8
@.str.10.AAGHandler = private constant [11 x i8] c"AAGHandler\00"
@"$s16AsyncActorGetter10AAGHandlerCMn" = hidden constant <{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor }> <{ i32 -2122317744, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16AsyncActorGetterMXM" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor }>, ptr @"$s16AsyncActorGetter10AAGHandlerCMn", i32 0, i32 1) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @.str.10.AAGHandler to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor }>, ptr @"$s16AsyncActorGetter10AAGHandlerCMn", i32 0, i32 2) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16AsyncActorGetter10AAGHandlerCMa" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor }>, ptr @"$s16AsyncActorGetter10AAGHandlerCMn", i32 0, i32 3) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16AsyncActorGetter10AAGHandlerCMF" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor }>, ptr @"$s16AsyncActorGetter10AAGHandlerCMn", i32 0, i32 4) to i64)) to i32), i32 0, i32 3, i32 17, i32 7, i32 2, i32 10, i32 12, i32 5, %swift.method_descriptor { i32 18, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16AsyncActorGetter10AAGHandlerC5utilsAA8AAGUtilsCvg" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor }>, ptr @"$s16AsyncActorGetter10AAGHandlerCMn", i32 0, i32 13, i32 1) to i64)) to i32) }, %swift.method_descriptor { i32 19, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16AsyncActorGetter10AAGHandlerC5utilsAA8AAGUtilsCvs" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor }>, ptr @"$s16AsyncActorGetter10AAGHandlerCMn", i32 0, i32 14, i32 1) to i64)) to i32) }, %swift.method_descriptor { i32 20, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16AsyncActorGetter10AAGHandlerC5utilsAA8AAGUtilsCvM" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor }>, ptr @"$s16AsyncActorGetter10AAGHandlerCMn", i32 0, i32 15, i32 1) to i64)) to i32) }, %swift.method_descriptor { i32 1, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16AsyncActorGetter10AAGHandlerC5utilsAcA8AAGUtilsC_tcfC" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor }>, ptr @"$s16AsyncActorGetter10AAGHandlerCMn", i32 0, i32 16, i32 1) to i64)) to i32) }, %swift.method_descriptor { i32 80, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaFTu" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor }>, ptr @"$s16AsyncActorGetter10AAGHandlerCMn", i32 0, i32 17, i32 1) to i64)) to i32) } }>, section "__TEXT,__constg_swiftt", align 4
@"$s16AsyncActorGetter10AAGHandlerCMf" = internal global <{ ptr, ptr, ptr, i64, ptr, ptr, ptr, i64, i32, i32, i32, i16, i16, i32, i32, ptr, ptr, i64, i64, ptr, ptr, ptr, ptr, ptr }> <{ ptr null, ptr @"$s16AsyncActorGetter10AAGHandlerCfD", ptr @"$sBoWV", i64 ptrtoint (ptr @"$s16AsyncActorGetter10AAGHandlerCMm" to i64), ptr @"OBJC_CLASS_$__TtCs12_SwiftObject", ptr @_objc_empty_cache, ptr null, i64 add (i64 ptrtoint (ptr @_DATA__TtC16AsyncActorGetter10AAGHandler to i64), i64 2), i32 2, i32 0, i32 120, i16 15, i16 0, i32 160, i32 24, ptr @"$s16AsyncActorGetter10AAGHandlerCMn", ptr null, i64 16, i64 112, ptr @"$s16AsyncActorGetter10AAGHandlerC5utilsAA8AAGUtilsCvg", ptr @"$s16AsyncActorGetter10AAGHandlerC5utilsAA8AAGUtilsCvs", ptr @"$s16AsyncActorGetter10AAGHandlerC5utilsAA8AAGUtilsCvM", ptr @"$s16AsyncActorGetter10AAGHandlerC5utilsAcA8AAGUtilsC_tcfC", ptr @"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaFTu" }>, align 8
@"symbolic _____ 16AsyncActorGetter10AAGHandlerC" = linkonce_odr hidden constant <{ i8, i32, i8 }> <{ i8 1, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16AsyncActorGetter10AAGHandlerCMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, i8 }>, ptr @"symbolic _____ 16AsyncActorGetter10AAGHandlerC", i32 0, i32 1) to i64)) to i32), i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@"symbolic BD" = linkonce_odr hidden constant <{ [2 x i8], i8 }> <{ [2 x i8] c"BD", i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@1 = private constant [14 x i8] c"$defaultActor\00", section "__TEXT,__swift5_reflstr, regular", no_sanitize_address
@2 = private constant [6 x i8] c"utils\00", section "__TEXT,__swift5_reflstr, regular", no_sanitize_address
@"$s16AsyncActorGetter10AAGHandlerCMF" = internal constant { i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32 } { i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____ 16AsyncActorGetter10AAGHandlerC" to i64), i64 ptrtoint (ptr @"$s16AsyncActorGetter10AAGHandlerCMF" to i64)) to i32), i32 0, i16 1, i16 12, i32 2, i32 6, i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic BD" to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s16AsyncActorGetter10AAGHandlerCMF", i32 0, i32 6) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @1 to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s16AsyncActorGetter10AAGHandlerCMF", i32 0, i32 7) to i64)) to i32), i32 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____ 16AsyncActorGetter8AAGUtilsC" to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s16AsyncActorGetter10AAGHandlerCMF", i32 0, i32 9) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @2 to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s16AsyncActorGetter10AAGHandlerCMF", i32 0, i32 10) to i64)) to i32) }, section "__TEXT,__swift5_fieldmd, regular", no_sanitize_address, align 4
@"_swift_FORCE_LOAD_$_swiftFoundation_$_AsyncActorGetter" = weak_odr hidden constant ptr @"_swift_FORCE_LOAD_$_swiftFoundation"
@"_swift_FORCE_LOAD_$_swift_Builtin_float_$_AsyncActorGetter" = weak_odr hidden constant ptr @"_swift_FORCE_LOAD_$_swift_Builtin_float"
@"_swift_FORCE_LOAD_$_swiftObjectiveC_$_AsyncActorGetter" = weak_odr hidden constant ptr @"_swift_FORCE_LOAD_$_swiftObjectiveC"
@"_swift_FORCE_LOAD_$_swiftCoreFoundation_$_AsyncActorGetter" = weak_odr hidden constant ptr @"_swift_FORCE_LOAD_$_swiftCoreFoundation"
@"_swift_FORCE_LOAD_$_swiftDispatch_$_AsyncActorGetter" = weak_odr hidden constant ptr @"_swift_FORCE_LOAD_$_swiftDispatch"
@"_swift_FORCE_LOAD_$_swiftXPC_$_AsyncActorGetter" = weak_odr hidden constant ptr @"_swift_FORCE_LOAD_$_swiftXPC"
@"_swift_FORCE_LOAD_$_swiftCompatibility56_$_AsyncActorGetter" = weak_odr hidden constant ptr @"_swift_FORCE_LOAD_$_swiftCompatibility56"
@"$sScAMp" = external global %swift.protocol, align 4
@"got.$sScAMp" = private unnamed_addr constant ptr @"$sScAMp"
@"$sScA15unownedExecutorScevgTq" = external global %swift.method_descriptor, align 4
@"got.$sScA15unownedExecutorScevgTq" = private unnamed_addr constant ptr @"$sScA15unownedExecutorScevgTq"
@"$s16AsyncActorGetter10AAGHandlerCScAAAMcMK" = internal global [16 x ptr] zeroinitializer
@"$s16AsyncActorGetter10AAGHandlerCScAAAMc" = hidden constant { i32, i32, i32, i32, i32, i32, i32, i16, i16, i32, i32 } { i32 add (i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$sScAMp" to i64), i64 ptrtoint (ptr @"$s16AsyncActorGetter10AAGHandlerCScAAAMc" to i64)) to i32), i32 1), i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16AsyncActorGetter10AAGHandlerCMn" to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i32, i32, i32, i32, i32, i16, i16, i32, i32 }, ptr @"$s16AsyncActorGetter10AAGHandlerCScAAAMc", i32 0, i32 1) to i64)) to i32), i32 0, i32 196608, i32 1, i32 add (i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$sScA15unownedExecutorScevgTq" to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i32, i32, i32, i32, i32, i16, i16, i32, i32 }, ptr @"$s16AsyncActorGetter10AAGHandlerCScAAAMc", i32 0, i32 5) to i64)) to i32), i32 1), i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16AsyncActorGetter10AAGHandlerCScAAAScA15unownedExecutorScevgTW" to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i32, i32, i32, i32, i32, i16, i16, i32, i32 }, ptr @"$s16AsyncActorGetter10AAGHandlerCScAAAMc", i32 0, i32 6) to i64)) to i32), i16 0, i16 1, i32 0, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16AsyncActorGetter10AAGHandlerCScAAAMcMK" to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i32, i32, i32, i32, i32, i16, i16, i32, i32 }, ptr @"$s16AsyncActorGetter10AAGHandlerCScAAAMc", i32 0, i32 10) to i64)) to i32) }, section "__TEXT,__const", no_sanitize_address, align 4
@"$s16AsyncActorGetter10AAGHandlerCScAAAHc" = private constant i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16AsyncActorGetter10AAGHandlerCScAAAMc" to i64), i64 ptrtoint (ptr @"$s16AsyncActorGetter10AAGHandlerCScAAAHc" to i64)) to i32), section "__TEXT, __swift5_proto, regular", no_sanitize_address, align 4
@"$s16AsyncActorGetter8AAGUtilsCHn" = private constant %swift.type_metadata_record { i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16AsyncActorGetter8AAGUtilsCMn" to i64), i64 ptrtoint (ptr @"$s16AsyncActorGetter8AAGUtilsCHn" to i64)) to i32) }, section "__TEXT, __swift5_types, regular", no_sanitize_address, align 4
@"$s16AsyncActorGetter10AAGHandlerCHn" = private constant %swift.type_metadata_record { i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16AsyncActorGetter10AAGHandlerCMn" to i64), i64 ptrtoint (ptr @"$s16AsyncActorGetter10AAGHandlerCHn" to i64)) to i32) }, section "__TEXT, __swift5_types, regular", no_sanitize_address, align 4
@__swift_reflection_version = linkonce_odr hidden constant i16 3
@"objc_classes_$s16AsyncActorGetter8AAGUtilsCN" = internal global ptr @"$s16AsyncActorGetter8AAGUtilsCN", section "__DATA,__objc_classlist,regular,no_dead_strip", no_sanitize_address, align 8
@"objc_classes_$s16AsyncActorGetter10AAGHandlerCN" = internal global ptr @"$s16AsyncActorGetter10AAGHandlerCN", section "__DATA,__objc_classlist,regular,no_dead_strip", no_sanitize_address, align 8
@llvm.compiler.used = appending global [10 x ptr] [ptr @"$s16AsyncActorGetter8AAGUtilsCACycfCTq", ptr @"$s16AsyncActorGetter8AAGUtilsCMf", ptr @"$s16AsyncActorGetter8AAGUtilsCN", ptr @"$s16AsyncActorGetter10AAGHandlerC5utilsAA8AAGUtilsCvgTq", ptr @"$s16AsyncActorGetter10AAGHandlerC5utilsAA8AAGUtilsCvsTq", ptr @"$s16AsyncActorGetter10AAGHandlerC5utilsAA8AAGUtilsCvMTq", ptr @"$s16AsyncActorGetter10AAGHandlerC5utilsAcA8AAGUtilsC_tcfCTq", ptr @"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaFTq", ptr @"$s16AsyncActorGetter10AAGHandlerCMf", ptr @"$s16AsyncActorGetter10AAGHandlerCN"], section "llvm.metadata"
@__swift_async_entry_functlets = internal constant [2 x i32] [i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaF" to i64), i64 ptrtoint (ptr @__swift_async_entry_functlets to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF" to i64), i64 ptrtoint (ptr getelementptr inbounds ([2 x i32], ptr @__swift_async_entry_functlets, i32 0, i32 1) to i64)) to i32)], section "__TEXT,__swift_as_entry, coalesced, no_dead_strip", no_sanitize_address, align 4
@__swift_async_ret_functlets = internal constant [1 x i32] [i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaFTQ1_" to i64), i64 ptrtoint (ptr @__swift_async_ret_functlets to i64)) to i32)], section "__TEXT,__swift_as_ret, coalesced, no_dead_strip", no_sanitize_address, align 4
@llvm.used = appending global [27 x ptr] [ptr @"$s16AsyncActorGetter8AAGUtilsC7counterSivg", ptr @"$s16AsyncActorGetter8AAGUtilsC7counterSivs", ptr @"$s16AsyncActorGetter10AAGHandlerC5utilsAA8AAGUtilsCvg", ptr @"$s16AsyncActorGetter10AAGHandlerC5utilsAA8AAGUtilsCvs", ptr @"$s16AsyncActorGetter10AAGHandlerC5utilsAcA8AAGUtilsC_tcfC", ptr @"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaF", ptr @"$s16AsyncActorGetter10AAGHandlerC15unownedExecutorScevg", ptr @"$s16AsyncActorGetter10AAGHandlerCScAAAScA15unownedExecutorScevgTW", ptr @"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF", ptr @"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaFTu", ptr @"$s16AsyncActorGetter8AAGUtilsCMF", ptr @"$s16AsyncActorGetter10AAGHandlerCMF", ptr @"_swift_FORCE_LOAD_$_swiftFoundation_$_AsyncActorGetter", ptr @"_swift_FORCE_LOAD_$_swift_Builtin_float_$_AsyncActorGetter", ptr @"_swift_FORCE_LOAD_$_swiftObjectiveC_$_AsyncActorGetter", ptr @"_swift_FORCE_LOAD_$_swiftCoreFoundation_$_AsyncActorGetter", ptr @"_swift_FORCE_LOAD_$_swiftDispatch_$_AsyncActorGetter", ptr @"_swift_FORCE_LOAD_$_swiftXPC_$_AsyncActorGetter", ptr @"_swift_FORCE_LOAD_$_swiftCompatibility56_$_AsyncActorGetter", ptr @"$s16AsyncActorGetter10AAGHandlerCScAAAHc", ptr @"$s16AsyncActorGetter8AAGUtilsCHn", ptr @"$s16AsyncActorGetter10AAGHandlerCHn", ptr @__swift_reflection_version, ptr @"objc_classes_$s16AsyncActorGetter8AAGUtilsCN", ptr @"objc_classes_$s16AsyncActorGetter10AAGHandlerCN", ptr @__swift_async_entry_functlets, ptr @__swift_async_ret_functlets], section "llvm.metadata"

@"$s16AsyncActorGetter8AAGUtilsCACycfCTq" = hidden alias %swift.method_descriptor, getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor }>, ptr @"$s16AsyncActorGetter8AAGUtilsCMn", i32 0, i32 13)
@"$s16AsyncActorGetter8AAGUtilsCN" = hidden alias %swift.type, getelementptr inbounds (<{ ptr, ptr, ptr, i64, ptr, ptr, ptr, i64, i32, i32, i32, i16, i16, i32, i32, ptr, ptr, i64, ptr }>, ptr @"$s16AsyncActorGetter8AAGUtilsCMf", i32 0, i32 3)
@"$s16AsyncActorGetter10AAGHandlerC5utilsAA8AAGUtilsCvgTq" = hidden alias %swift.method_descriptor, getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor }>, ptr @"$s16AsyncActorGetter10AAGHandlerCMn", i32 0, i32 13)
@"$s16AsyncActorGetter10AAGHandlerC5utilsAA8AAGUtilsCvsTq" = hidden alias %swift.method_descriptor, getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor }>, ptr @"$s16AsyncActorGetter10AAGHandlerCMn", i32 0, i32 14)
@"$s16AsyncActorGetter10AAGHandlerC5utilsAA8AAGUtilsCvMTq" = hidden alias %swift.method_descriptor, getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor }>, ptr @"$s16AsyncActorGetter10AAGHandlerCMn", i32 0, i32 15)
@"$s16AsyncActorGetter10AAGHandlerC5utilsAcA8AAGUtilsC_tcfCTq" = hidden alias %swift.method_descriptor, getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor }>, ptr @"$s16AsyncActorGetter10AAGHandlerCMn", i32 0, i32 16)
@"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaFTq" = hidden alias %swift.method_descriptor, getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor, %swift.method_descriptor }>, ptr @"$s16AsyncActorGetter10AAGHandlerCMn", i32 0, i32 17)
@"$s16AsyncActorGetter10AAGHandlerCN" = hidden alias %swift.type, getelementptr inbounds (<{ ptr, ptr, ptr, i64, ptr, ptr, ptr, i64, i32, i32, i32, i16, i16, i32, i32, ptr, ptr, i64, i64, ptr, ptr, ptr, ptr, ptr }>, ptr @"$s16AsyncActorGetter10AAGHandlerCMf", i32 0, i32 3)

define hidden swiftcc i64 @"$s16AsyncActorGetter8AAGUtilsC7counterSivg"(ptr swiftself %0) #0 !dbg !68 {
  %2 = alloca [24 x i8], align 8
  %3 = getelementptr inbounds nuw %T16AsyncActorGetter8AAGUtilsC, ptr %0, i32 0, i32 1, !dbg !75
  call void @llvm.lifetime.start.p0(i64 -1, ptr %2), !dbg !75
  call void @swift_beginAccess(ptr %3, ptr %2, i64 32, ptr null) #2, !dbg !75
  %4 = getelementptr inbounds nuw %TSi, ptr %3, i32 0, i32 0, !dbg !75
  %5 = load i64, ptr %4, align 8, !dbg !75
  call void @swift_endAccess(ptr %2) #2, !dbg !75
  call void @llvm.lifetime.end.p0(i64 -1, ptr %2), !dbg !75
  ret i64 %5, !dbg !75
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.start.p0(i64 immarg, ptr captures(none)) #1

; Function Attrs: nounwind
declare void @swift_beginAccess(ptr, ptr, i64, ptr) #2

; Function Attrs: nounwind
declare void @swift_endAccess(ptr) #2

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.end.p0(i64 immarg, ptr captures(none)) #1

define hidden swiftcc void @"$s16AsyncActorGetter8AAGUtilsC7counterSivs"(i64 %0, ptr swiftself %1) #0 !dbg !76 {
  %3 = alloca [24 x i8], align 8
  %4 = getelementptr inbounds nuw %T16AsyncActorGetter8AAGUtilsC, ptr %1, i32 0, i32 1, !dbg !81
  call void @llvm.lifetime.start.p0(i64 -1, ptr %3), !dbg !81
  call void @swift_beginAccess(ptr %4, ptr %3, i64 33, ptr null) #2, !dbg !81
  %5 = getelementptr inbounds nuw %TSi, ptr %4, i32 0, i32 0, !dbg !81
  store i64 %0, ptr %5, align 8, !dbg !81
  call void @swift_endAccess(ptr %3) #2, !dbg !81
  call void @llvm.lifetime.end.p0(i64 -1, ptr %3), !dbg !81
  ret void, !dbg !81
}

define hidden swiftcc ptr @"$s16AsyncActorGetter8AAGUtilsCfd"(ptr swiftself %0) #0 !dbg !82 {
  %2 = alloca ptr, align 8
    #dbg_declare(ptr %2, !89, !DIExpression(), !91)
  call void @llvm.memset.p0.i64(ptr align 8 %2, i8 0, i64 8, i1 false)
  store ptr %0, ptr %2, align 8, !dbg !92
  ret ptr %0, !dbg !92
}

define hidden swiftcc void @"$s16AsyncActorGetter8AAGUtilsCfD"(ptr swiftself %0) #0 !dbg !93 {
  %2 = alloca ptr, align 8
    #dbg_declare(ptr %2, !98, !DIExpression(), !99)
  call void @llvm.memset.p0.i64(ptr align 8 %2, i8 0, i64 8, i1 false)
  store ptr %0, ptr %2, align 8, !dbg !100
  %3 = call swiftcc ptr @"$s16AsyncActorGetter8AAGUtilsCfd"(ptr swiftself %0), !dbg !100
  call void @swift_deallocClassInstance(ptr %3, i64 24, i64 7) #2, !dbg !100
  ret void, !dbg !100
}

define hidden swiftcc ptr @"$s16AsyncActorGetter8AAGUtilsCACycfC"(ptr swiftself %0) #0 !dbg !101 {
  %2 = call noalias ptr @swift_allocObject(ptr %0, i64 24, i64 7) #2, !dbg !106
  %3 = call swiftcc ptr @"$s16AsyncActorGetter8AAGUtilsCACycfc"(ptr swiftself %2), !dbg !106
  ret ptr %3, !dbg !106
}

define hidden swiftcc ptr @"$s16AsyncActorGetter8AAGUtilsCACycfc"(ptr swiftself %0) #0 !dbg !107 {
  %2 = alloca ptr, align 8
    #dbg_declare(ptr %2, !112, !DIExpression(), !113)
  call void @llvm.memset.p0.i64(ptr align 8 %2, i8 0, i64 8, i1 false)
  store ptr %0, ptr %2, align 8, !dbg !114
  %3 = getelementptr inbounds nuw %T16AsyncActorGetter8AAGUtilsC, ptr %0, i32 0, i32 1, !dbg !115
  %4 = getelementptr inbounds nuw %TSi, ptr %3, i32 0, i32 0, !dbg !118
  store i64 0, ptr %4, align 8, !dbg !118
  ret ptr %0, !dbg !120
}

define hidden swiftcc ptr @"$s16AsyncActorGetter10AAGHandlerC5utilsAA8AAGUtilsCvg"(ptr swiftself %0) #0 !dbg !121 {
  %2 = alloca [24 x i8], align 8
  %3 = getelementptr inbounds nuw %T16AsyncActorGetter10AAGHandlerC, ptr %0, i32 0, i32 2, !dbg !126
  call void @llvm.lifetime.start.p0(i64 -1, ptr %2), !dbg !126
  call void @swift_beginAccess(ptr %3, ptr %2, i64 32, ptr null) #2, !dbg !126
  %4 = load ptr, ptr %3, align 16, !dbg !126
  %5 = call ptr @swift_retain(ptr returned %4) #3, !dbg !126
  call void @swift_endAccess(ptr %2) #2, !dbg !126
  call void @llvm.lifetime.end.p0(i64 -1, ptr %2), !dbg !126
  ret ptr %4, !dbg !126
}

; Function Attrs: nounwind willreturn
declare ptr @swift_retain(ptr returned) #3

define hidden swiftcc void @"$s16AsyncActorGetter10AAGHandlerC5utilsAA8AAGUtilsCvs"(ptr %0, ptr swiftself %1) #0 !dbg !127 {
  %3 = alloca [24 x i8], align 8
  %4 = call ptr @swift_retain(ptr returned %0) #3, !dbg !131
  %5 = getelementptr inbounds nuw %T16AsyncActorGetter10AAGHandlerC, ptr %1, i32 0, i32 2, !dbg !131
  call void @llvm.lifetime.start.p0(i64 -1, ptr %3), !dbg !131
  call void @swift_beginAccess(ptr %5, ptr %3, i64 33, ptr null) #2, !dbg !131
  %6 = load ptr, ptr %5, align 16, !dbg !131
  store ptr %0, ptr %5, align 16, !dbg !131
  call void @swift_release(ptr %6) #2, !dbg !131
  call void @swift_endAccess(ptr %3) #2, !dbg !131
  call void @llvm.lifetime.end.p0(i64 -1, ptr %3), !dbg !131
  call void @swift_release(ptr %0) #2, !dbg !131
  ret void, !dbg !131
}

; Function Attrs: nounwind
declare void @swift_release(ptr) #2

; Function Attrs: noinline
define hidden swiftcc { ptr, ptr } @"$s16AsyncActorGetter10AAGHandlerC5utilsAA8AAGUtilsCvM"(ptr noalias dereferenceable(32) %0, ptr swiftself %1) #4 !dbg !132 {
  %3 = getelementptr inbounds %"$s16AsyncActorGetter10AAGHandlerC5utilsAA8AAGUtilsCvM.Frame", ptr %0, i32 0, i32 0, !dbg !136
  %4 = getelementptr inbounds nuw %T16AsyncActorGetter10AAGHandlerC, ptr %1, i32 0, i32 2, !dbg !136
  call void @llvm.lifetime.start.p0(i64 -1, ptr %3), !dbg !136
  call void @swift_beginAccess(ptr %4, ptr %3, i64 33, ptr null) #2, !dbg !136
  %5 = insertvalue { ptr, ptr } poison, ptr @"$s16AsyncActorGetter10AAGHandlerC5utilsAA8AAGUtilsCvM.resume.0", 0
  %6 = insertvalue { ptr, ptr } %5, ptr %4, 1
  ret { ptr, ptr } %6
}

define internal swiftcc void @"$s16AsyncActorGetter10AAGHandlerC5utilsAA8AAGUtilsCvM.resume.0"(ptr noalias noundef nonnull align 8 dereferenceable(32) %0, i1 %1) #0 !dbg !137 {
  %3 = getelementptr inbounds %"$s16AsyncActorGetter10AAGHandlerC5utilsAA8AAGUtilsCvM.Frame", ptr %0, i32 0, i32 0, !dbg !139
  call void @swift_endAccess(ptr %3) #2, !dbg !139
  call void @llvm.lifetime.end.p0(i64 -1, ptr %3), !dbg !139
  ret void, !dbg !139
}

define hidden swiftcc ptr @"$s16AsyncActorGetter10AAGHandlerC5utilsAcA8AAGUtilsC_tcfC"(ptr %0, ptr swiftself %1) #0 !dbg !140 {
  %3 = call noalias ptr @swift_allocObject(ptr %1, i64 120, i64 15) #2, !dbg !145
  %4 = call swiftcc ptr @"$s16AsyncActorGetter10AAGHandlerC5utilsAcA8AAGUtilsC_tcfc"(ptr %0, ptr swiftself %3), !dbg !145
  ret ptr %4, !dbg !145
}

; Function Attrs: nounwind
declare ptr @swift_allocObject(ptr, i64, i64) #2

define hidden swiftcc ptr @"$s16AsyncActorGetter10AAGHandlerC5utilsAcA8AAGUtilsC_tcfc"(ptr %0, ptr swiftself %1) #0 !dbg !146 {
  %3 = alloca ptr, align 8
    #dbg_declare(ptr %3, !151, !DIExpression(), !154)
  call void @llvm.memset.p0.i64(ptr align 8 %3, i8 0, i64 8, i1 false)
  %4 = alloca ptr, align 8
    #dbg_declare(ptr %4, !152, !DIExpression(), !155)
  call void @llvm.memset.p0.i64(ptr align 8 %4, i8 0, i64 8, i1 false)
  store ptr %0, ptr %3, align 8, !dbg !156
  store ptr %1, ptr %4, align 8, !dbg !156
  call swiftcc void @swift_defaultActor_initialize(ptr %1) #2, !dbg !157
  %5 = call ptr @swift_retain(ptr returned %0) #3, !dbg !159
  %6 = getelementptr inbounds nuw %T16AsyncActorGetter10AAGHandlerC, ptr %1, i32 0, i32 2, !dbg !160
  store ptr %0, ptr %6, align 16, !dbg !160
  call void @swift_release(ptr %0) #2, !dbg !161
  ret ptr %1, !dbg !161
}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: write)
declare void @llvm.memset.p0.i64(ptr writeonly captures(none), i8, i64, i1 immarg) #5

; Function Attrs: nounwind
declare swiftcc void @swift_defaultActor_initialize(ptr) #2

; Function Attrs: noinline
define hidden swifttailcc void @"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaF"(ptr swiftasync %0, ptr swiftself %1) #6 !dbg !162 {
  %3 = alloca ptr, align 8
    #dbg_declare(ptr %3, !167, !DIExpression(DW_OP_deref, DW_OP_plus_uconst, 72), !168)
  store ptr %0, ptr %3, align 8
  %4 = getelementptr inbounds i8, ptr %0, i32 16
  %5 = getelementptr inbounds %"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaF.Frame", ptr %4, i32 0, i32 2
  %6 = getelementptr inbounds %"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaF.Frame", ptr %4, i32 0, i32 3
  %7 = getelementptr inbounds %"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaF.Frame", ptr %4, i32 0, i32 0
  %8 = getelementptr inbounds %"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaF.Frame", ptr %4, i32 0, i32 1
  %9 = getelementptr inbounds %"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaF.Frame", ptr %4, i32 0, i32 4
  store ptr %1, ptr %9, align 8
  store ptr %0, ptr %5, align 8
  call void @llvm.memset.p0.i64(ptr align 8 %6, i8 0, i64 8, i1 false)
  store ptr %1, ptr %6, align 8, !dbg !169
  call void asm sideeffect "", "r"(ptr %6), !dbg !170
  %10 = ptrtoint ptr %1 to i64, !dbg !168
  %11 = load ptr, ptr %5, align 8, !dbg !168
  %12 = load ptr, ptr %5, align 8, !dbg !168
  musttail call swifttailcc void @swift_task_switch(ptr swiftasync %12, ptr @"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaFTY0_", i64 %10, i64 0) #2, !dbg !168
  ret void, !dbg !168
}

; Function Attrs: noinline
define internal swifttailcc void @"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaFTY0_"(ptr swiftasync %0) #4 !dbg !172 {
    #dbg_declare(ptr %0, !175, !DIExpression(DW_OP_LLVM_entry_value, 1, DW_OP_plus_uconst, 72), !176)
  %2 = getelementptr inbounds i8, ptr %0, i32 16
  %3 = getelementptr inbounds %"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaF.Frame", ptr %2, i32 0, i32 2
  %4 = getelementptr inbounds %"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaF.Frame", ptr %2, i32 0, i32 3
  %5 = getelementptr inbounds %"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaF.Frame", ptr %2, i32 0, i32 0
  %6 = getelementptr inbounds %"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaF.Frame", ptr %2, i32 0, i32 1
  %7 = getelementptr inbounds %"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaF.Frame", ptr %2, i32 0, i32 4
  %8 = load ptr, ptr %7, align 8
  store ptr %0, ptr %3, align 8
  %9 = getelementptr inbounds nuw %T16AsyncActorGetter10AAGHandlerC, ptr %8, i32 0, i32 2, !dbg !177
  call void @llvm.lifetime.start.p0(i64 -1, ptr %5), !dbg !177
  call void @swift_beginAccess(ptr %9, ptr %5, i64 32, ptr null) #2, !dbg !177
  %10 = load ptr, ptr %9, align 16, !dbg !177
  %11 = call ptr @swift_retain(ptr returned %10) #3, !dbg !177
  call void @swift_endAccess(ptr %5) #2, !dbg !177
  call void @llvm.lifetime.end.p0(i64 -1, ptr %5), !dbg !177
  %12 = getelementptr inbounds nuw %T16AsyncActorGetter8AAGUtilsC, ptr %10, i32 0, i32 1, !dbg !178
  call void @llvm.lifetime.start.p0(i64 -1, ptr %6), !dbg !178
  call void @swift_beginAccess(ptr %12, ptr %6, i64 32, ptr null) #2, !dbg !178
  %13 = getelementptr inbounds nuw %TSi, ptr %12, i32 0, i32 0, !dbg !178
  %14 = load i64, ptr %13, align 8, !dbg !178
  call void @swift_endAccess(ptr %6) #2, !dbg !179
  call void @llvm.lifetime.end.p0(i64 -1, ptr %6), !dbg !179
  call void @swift_release(ptr %10) #2, !dbg !180
  call void asm sideeffect "", "r"(ptr %4), !dbg !180
  %15 = load ptr, ptr %3, align 8, !dbg !180
  %16 = getelementptr inbounds nuw <{ ptr, ptr }>, ptr %15, i32 0, i32 1, !dbg !180
  %17 = load ptr, ptr %16, align 8, !dbg !180
  %18 = load ptr, ptr %3, align 8, !dbg !180
  musttail call swifttailcc void %17(ptr swiftasync %18, i64 %14) #2, !dbg !180
  ret void, !dbg !180
}

; Function Attrs: nounwind
declare swifttailcc void @swift_task_switch(ptr, ptr, i64, i64) #2

; Function Attrs: nounwind
declare ptr @llvm.swift.async.context.addr() #2

define hidden swiftcc ptr @"$s16AsyncActorGetter10AAGHandlerCfd"(ptr swiftself %0) #0 !dbg !181 {
  %2 = alloca ptr, align 8
    #dbg_declare(ptr %2, !186, !DIExpression(), !187)
  call void @llvm.memset.p0.i64(ptr align 8 %2, i8 0, i64 8, i1 false)
  store ptr %0, ptr %2, align 8, !dbg !188
  %3 = getelementptr inbounds nuw %T16AsyncActorGetter10AAGHandlerC, ptr %0, i32 0, i32 2, !dbg !188
  %4 = load ptr, ptr %3, align 16, !dbg !188
  call void @swift_release(ptr %4) #2, !dbg !188
  call swiftcc void @swift_defaultActor_destroy(ptr %0) #2, !dbg !188
  ret ptr %0, !dbg !188
}

define hidden swiftcc void @"$s16AsyncActorGetter10AAGHandlerCfD"(ptr swiftself %0) #0 !dbg !189 {
  %2 = alloca ptr, align 8
    #dbg_declare(ptr %2, !192, !DIExpression(), !193)
  call void @llvm.memset.p0.i64(ptr align 8 %2, i8 0, i64 8, i1 false)
  store ptr %0, ptr %2, align 8, !dbg !194
  %3 = call swiftcc ptr @"$s16AsyncActorGetter10AAGHandlerCfd"(ptr swiftself %0), !dbg !194
  call swiftcc void @swift_defaultActor_deallocate(ptr %3) #2, !dbg !194
  ret void, !dbg !194
}

define hidden swiftcc { i64, i64 } @"$s16AsyncActorGetter10AAGHandlerC15unownedExecutorScevg"(ptr swiftself %0) #0 !dbg !195 {
  %2 = alloca ptr, align 8
    #dbg_declare(ptr %2, !201, !DIExpression(), !202)
  call void @llvm.memset.p0.i64(ptr align 8 %2, i8 0, i64 8, i1 false)
  store ptr %0, ptr %2, align 8, !dbg !202
  %3 = ptrtoint ptr %0 to i64, !dbg !202
  %4 = call swiftcc { i64, i64 } @"$sSceySceBecfC"(i64 %3, i64 0), !dbg !202
  %5 = extractvalue { i64, i64 } %4, 0, !dbg !202
  %6 = extractvalue { i64, i64 } %4, 1, !dbg !202
  %7 = insertvalue { i64, i64 } undef, i64 %5, 0, !dbg !202
  %8 = insertvalue { i64, i64 } %7, i64 %6, 1, !dbg !202
  ret { i64, i64 } %8, !dbg !202
}

declare swiftcc { i64, i64 } @"$sSceySceBecfC"(i64, i64) #0

define internal swiftcc { i64, i64 } @"$s16AsyncActorGetter10AAGHandlerCScAAAScA15unownedExecutorScevgTW"(ptr swiftself %0, ptr %1, ptr %2) #0 !dbg !203 {
  %4 = call swiftcc { i64, i64 } @"$s16AsyncActorGetter10AAGHandlerC15unownedExecutorScevg"(ptr swiftself %0) #11, !dbg !205
  %5 = extractvalue { i64, i64 } %4, 0, !dbg !205
  %6 = extractvalue { i64, i64 } %4, 1, !dbg !205
  %7 = insertvalue { i64, i64 } undef, i64 %5, 0, !dbg !205
  %8 = insertvalue { i64, i64 } %7, i64 %6, 1, !dbg !205
  ret { i64, i64 } %8, !dbg !205
}

; Function Attrs: noinline
define swifttailcc void @"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF"(ptr swiftasync %0) #6 !dbg !206 {
  %2 = alloca ptr, align 8
    #dbg_declare(ptr %2, !210, !DIExpression(DW_OP_deref, DW_OP_plus_uconst, 24), !212)
  store ptr %0, ptr %2, align 8
  %3 = getelementptr inbounds i8, ptr %0, i32 16
  %4 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %3, i32 0, i32 0
  %5 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %3, i32 0, i32 1
  store ptr %0, ptr %4, align 8
  call void @llvm.memset.p0.i64(ptr align 8 %5, i8 0, i64 8, i1 false)
  %6 = call swiftcc %swift.metadata_response @"$sScMMa"(i64 0) #12, !dbg !213
  %7 = extractvalue %swift.metadata_response %6, 0, !dbg !213
  %8 = call swiftcc ptr @"$sScM6sharedScMvgZ"(ptr swiftself %7), !dbg !213
  %9 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %3, i32 0, i32 2, !dbg !214
  store ptr %8, ptr %9, align 8, !dbg !214
  %10 = call ptr @"$sS2cMScAsWl"() #12, !dbg !214
  %11 = call swiftcc { i64, i64 } @"$sScA15unownedExecutorScevgTj"(ptr swiftself %8, ptr %7, ptr %10), !dbg !214
  %12 = extractvalue { i64, i64 } %11, 0, !dbg !214
  %13 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %3, i32 0, i32 3, !dbg !214
  store i64 %12, ptr %13, align 8, !dbg !214
  %14 = extractvalue { i64, i64 } %11, 1, !dbg !214
  %15 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %3, i32 0, i32 4, !dbg !214
  store i64 %14, ptr %15, align 8, !dbg !214
  %16 = load ptr, ptr %4, align 8, !dbg !214
  %17 = load ptr, ptr %4, align 8, !dbg !214
  musttail call swifttailcc void @swift_task_switch(ptr swiftasync %17, ptr @"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaFTY0_", i64 %12, i64 %14) #2, !dbg !214
  ret void, !dbg !214
}

; Function Attrs: noinline
define internal swifttailcc void @"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaFTY0_"(ptr swiftasync %0) #4 !dbg !215 {
    #dbg_declare(ptr %0, !217, !DIExpression(DW_OP_LLVM_entry_value, 1, DW_OP_plus_uconst, 24), !219)
  %2 = getelementptr inbounds i8, ptr %0, i32 16
  %3 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %2, i32 0, i32 0
  %4 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %2, i32 0, i32 1
  store ptr %0, ptr %3, align 8
  %5 = call swiftcc %swift.metadata_response @"$s16AsyncActorGetter10AAGHandlerCMa"(i64 0) #12, !dbg !220
  %6 = extractvalue %swift.metadata_response %5, 0, !dbg !220
  %7 = call swiftcc %swift.metadata_response @"$s16AsyncActorGetter8AAGUtilsCMa"(i64 0) #12, !dbg !222
  %8 = extractvalue %swift.metadata_response %7, 0, !dbg !222
  %9 = call swiftcc ptr @"$s16AsyncActorGetter8AAGUtilsCACycfC"(ptr swiftself %8), !dbg !222
  %10 = call swiftcc ptr @"$s16AsyncActorGetter10AAGHandlerC5utilsAcA8AAGUtilsC_tcfC"(ptr %9, ptr swiftself %6), !dbg !220
  %11 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %2, i32 0, i32 5, !dbg !223
  store ptr %10, ptr %11, align 8, !dbg !223
  store ptr %10, ptr %4, align 8, !dbg !223
  call void asm sideeffect "", "r"(ptr %4), !dbg !224
  %12 = load i32, ptr getelementptr inbounds nuw (%swift.async_func_pointer, ptr @"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaFTu", i32 0, i32 1), align 8, !dbg !226
  %13 = zext i32 %12 to i64, !dbg !226
  %14 = call swiftcc ptr @swift_task_alloc(i64 %13) #10, !dbg !226
  %15 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %2, i32 0, i32 6, !dbg !226
  store ptr %14, ptr %15, align 8, !dbg !226
  call void @llvm.lifetime.start.p0(i64 -1, ptr %14), !dbg !226
  %16 = load ptr, ptr %3, align 8, !dbg !226
  %17 = getelementptr inbounds nuw <{ ptr, ptr }>, ptr %14, i32 0, i32 0, !dbg !226
  store ptr %16, ptr %17, align 8, !dbg !226
  %18 = getelementptr inbounds nuw <{ ptr, ptr }>, ptr %14, i32 0, i32 1, !dbg !226
  store ptr @"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaFTQ1_", ptr %18, align 8, !dbg !226
  musttail call swifttailcc void @"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaF"(ptr swiftasync %14, ptr swiftself %10) #2, !dbg !226
  ret void, !dbg !226
}

; Function Attrs: noinline
define internal swifttailcc void @"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaFTQ1_"(ptr swiftasync %0, i64 %1) #7 !dbg !227 {
    #dbg_declare(ptr %0, !229, !DIExpression(DW_OP_LLVM_entry_value, 1, DW_OP_deref, DW_OP_plus_uconst, 24), !231)
  %3 = load ptr, ptr %0, align 8, !dbg !232
  %4 = call ptr @llvm.swift.async.context.addr(), !dbg !232
  store ptr %3, ptr %4, align 8, !dbg !232
  %5 = getelementptr inbounds i8, ptr %3, i32 16
  %6 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %5, i32 0, i32 0
  %7 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %5, i32 0, i32 1
  %8 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %5, i32 0, i32 6
  %9 = load ptr, ptr %8, align 8
  %10 = load ptr, ptr %0, align 8
  store ptr %10, ptr %6, align 8
  call swiftcc void @swift_task_dealloc(ptr %9) #10, !dbg !232
  call void @llvm.lifetime.end.p0(i64 -1, ptr %9), !dbg !232
  %11 = load ptr, ptr %6, align 8, !dbg !232
  %12 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %5, i32 0, i32 4, !dbg !232
  %13 = load i64, ptr %12, align 8, !dbg !232
  %14 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %5, i32 0, i32 3, !dbg !232
  %15 = load i64, ptr %14, align 8, !dbg !232
  musttail call swifttailcc void @swift_task_switch(ptr swiftasync %11, ptr @"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaFTY2_", i64 %15, i64 %13) #2, !dbg !232
  ret void, !dbg !232
}

; Function Attrs: noinline
define internal swifttailcc void @"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaFTY2_"(ptr swiftasync %0) #4 !dbg !233 {
    #dbg_declare(ptr %0, !235, !DIExpression(DW_OP_LLVM_entry_value, 1, DW_OP_plus_uconst, 24), !237)
  %2 = getelementptr inbounds i8, ptr %0, i32 16
  %3 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %2, i32 0, i32 0
  %4 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %2, i32 0, i32 1
  %5 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %2, i32 0, i32 5
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %2, i32 0, i32 2
  %8 = load ptr, ptr %7, align 8
  store ptr %0, ptr %3, align 8
  call void @swift_release(ptr %8) #2, !dbg !238
  call void @swift_release(ptr %6) #2, !dbg !238
  call void asm sideeffect "", "r"(ptr %4), !dbg !238
  %9 = load ptr, ptr %3, align 8, !dbg !238
  %10 = getelementptr inbounds nuw <{ ptr, ptr }>, ptr %9, i32 0, i32 1, !dbg !238
  %11 = load ptr, ptr %10, align 8, !dbg !238
  %12 = load ptr, ptr %3, align 8, !dbg !238
  musttail call swifttailcc void %11(ptr swiftasync %12) #2, !dbg !238
  ret void, !dbg !238
}

declare swiftcc %swift.metadata_response @"$sScMMa"(i64) #0

declare swiftcc ptr @"$sScM6sharedScMvgZ"(ptr swiftself) #0

declare swiftcc { i64, i64 } @"$sScA15unownedExecutorScevgTj"(ptr swiftself, ptr, ptr) #0

; Function Attrs: noinline nounwind memory(none)
define linkonce_odr hidden ptr @"$sS2cMScAsWl"() #8 !dbg !239 {
  %1 = load ptr, ptr @"$sS2cMScAsWL", align 8, !dbg !241
  %2 = icmp eq ptr %1, null, !dbg !241
  br i1 %2, label %3, label %8, !dbg !241

3:                                                ; preds = %0
  %4 = call swiftcc %swift.metadata_response @"$sScMMa"(i64 255) #12, !dbg !241
  %5 = extractvalue %swift.metadata_response %4, 0, !dbg !241
  %6 = extractvalue %swift.metadata_response %4, 1, !dbg !241
  %7 = call ptr @swift_getWitnessTable(ptr @"$sScMScAsMc", ptr %5, ptr undef) #9, !dbg !241
  store atomic ptr %7, ptr @"$sS2cMScAsWL" release, align 8, !dbg !241
  br label %8, !dbg !241

8:                                                ; preds = %3, %0
  %9 = phi ptr [ %1, %0 ], [ %7, %3 ], !dbg !241
  ret ptr %9, !dbg !241
}

; Function Attrs: nounwind memory(read)
declare ptr @swift_getWitnessTable(ptr, ptr, ptr) #9

; Function Attrs: noinline nounwind memory(none)
define hidden swiftcc %swift.metadata_response @"$s16AsyncActorGetter10AAGHandlerCMa"(i64 %0) #8 !dbg !242 {
  %2 = call ptr @objc_opt_self(ptr getelementptr inbounds (<{ ptr, ptr, ptr, i64, ptr, ptr, ptr, i64, i32, i32, i32, i16, i16, i32, i32, ptr, ptr, i64, i64, ptr, ptr, ptr, ptr, ptr }>, ptr @"$s16AsyncActorGetter10AAGHandlerCMf", i32 0, i32 3)) #2, !dbg !243
  %3 = insertvalue %swift.metadata_response undef, ptr %2, 0, !dbg !243
  %4 = insertvalue %swift.metadata_response %3, i64 0, 1, !dbg !243
  ret %swift.metadata_response %4, !dbg !243
}

; Function Attrs: noinline nounwind memory(none)
define hidden swiftcc %swift.metadata_response @"$s16AsyncActorGetter8AAGUtilsCMa"(i64 %0) #8 !dbg !244 {
  %2 = call ptr @objc_opt_self(ptr getelementptr inbounds (<{ ptr, ptr, ptr, i64, ptr, ptr, ptr, i64, i32, i32, i32, i16, i16, i32, i32, ptr, ptr, i64, ptr }>, ptr @"$s16AsyncActorGetter8AAGUtilsCMf", i32 0, i32 3)) #2, !dbg !245
  %3 = insertvalue %swift.metadata_response undef, ptr %2, 0, !dbg !245
  %4 = insertvalue %swift.metadata_response %3, i64 0, 1, !dbg !245
  ret %swift.metadata_response %4, !dbg !245
}

; Function Attrs: nounwind memory(inaccessiblemem: readwrite)
declare swiftcc ptr @swift_task_alloc(i64) #10

; Function Attrs: nounwind memory(inaccessiblemem: readwrite)
declare swiftcc void @swift_task_dealloc(ptr) #10

; Function Attrs: nounwind
declare ptr @objc_opt_self(ptr) #2

declare extern_weak void @"_swift_FORCE_LOAD_$_swiftFoundation"()

declare extern_weak void @"_swift_FORCE_LOAD_$_swift_Builtin_float"()

declare extern_weak void @"_swift_FORCE_LOAD_$_swiftObjectiveC"()

declare extern_weak void @"_swift_FORCE_LOAD_$_swiftCoreFoundation"()

declare extern_weak void @"_swift_FORCE_LOAD_$_swiftDispatch"()

declare extern_weak void @"_swift_FORCE_LOAD_$_swiftXPC"()

declare extern_weak void @"_swift_FORCE_LOAD_$_swiftCompatibility56"()

; Function Attrs: nounwind
declare swiftcc void @swift_defaultActor_deallocate(ptr) #2

; Function Attrs: nounwind
declare swiftcc void @swift_defaultActor_destroy(ptr) #2

; Function Attrs: nounwind
declare void @swift_deallocClassInstance(ptr, i64, i64) #2

attributes #0 = { "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" }
attributes #1 = { nocallback nofree nosync nounwind willreturn memory(argmem: readwrite) }
attributes #2 = { nounwind }
attributes #3 = { nounwind willreturn }
attributes #4 = { noinline "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" }
attributes #5 = { nocallback nofree nounwind willreturn memory(argmem: write) }
attributes #6 = { noinline "async_entry" "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" }
attributes #7 = { noinline "async_ret" "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" }
attributes #8 = { noinline nounwind memory(none) "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" }
attributes #9 = { nounwind memory(read) }
attributes #10 = { nounwind memory(inaccessiblemem: readwrite) }
attributes #11 = { noinline }
attributes #12 = { nounwind memory(none) }

!llvm.module.flags = !{!0, !1, !2, !3, !4, !5, !6, !7, !8, !9, !10, !11, !12, !13, !14, !15, !16, !17}
!llvm.dbg.cu = !{!18, !37, !39, !41}
!swift.module.flags = !{!43}
!llvm.linker.options = !{!44, !45, !46, !47, !48, !49, !50, !51, !52, !53, !54, !55, !56, !57, !58, !59, !60, !61, !62, !63, !64, !65, !66, !67}

!0 = !{i32 2, !"SDK Version", [2 x i32] [i32 26, i32 2]}
!1 = !{i32 1, !"Objective-C Version", i32 2}
!2 = !{i32 1, !"Objective-C Image Info Version", i32 0}
!3 = !{i32 1, !"Objective-C Image Info Section", !"__DATA,__objc_imageinfo,regular,no_dead_strip"}
!4 = !{i32 1, !"Objective-C Garbage Collection", i8 0}
!5 = !{i32 1, !"Objective-C Is Simulated", i32 32}
!6 = !{i32 1, !"Objective-C Class Properties", i32 64}
!7 = !{i32 1, !"Objective-C Enforce ClassRO Pointer Signing", i8 0}
!8 = !{i32 7, !"Dwarf Version", i32 4}
!9 = !{i32 2, !"Debug Info Version", i32 3}
!10 = !{i32 1, !"wchar_size", i32 4}
!11 = !{i32 8, !"PIC Level", i32 2}
!12 = !{i32 7, !"uwtable", i32 1}
!13 = !{i32 7, !"frame-pointer", i32 1}
!14 = !{i32 1, !"Swift Version", i32 7}
!15 = !{i32 1, !"Swift ABI Version", i32 7}
!16 = !{i32 1, !"Swift Major Version", i8 6}
!17 = !{i32 1, !"Swift Minor Version", i8 3}
!18 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !19, producer: "Facebook Swift version 6.3.2-dev effective-5.10 (LLVM 0c6fa689b924b2d, Swift 999e243c7e9b3d1)", isOptimized: false, runtimeVersion: 6, emissionKind: FullDebug, imports: !20, sysroot: "buck-out/infer-fbobjc/art/fbsource/xplat/toolchains/apple/__xcode_26.2.0_17c52-iphonesimulator-sdk_genrule-actual__/e5e3cbbf737d97c8/iPhoneSimulator26.2.sdk", sdk: "iPhoneSimulator26.2.sdk")
!19 = !DIFile(filename: "fbobjc/Tools/infer/tests/codetoanalyze/swift/pulse/AsyncActorGetter/RcAsyncActorGetter.swift", directory: ".")
!20 = !{!21, !23, !25, !27, !29, !31, !33, !35}
!21 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !22, file: !19)
!22 = !DIModule(scope: null, name: "AsyncActorGetter", includePath: "fbobjc/Tools/infer/tests/codetoanalyze/swift/pulse/AsyncActorGetter")
!23 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !24, file: !19)
!24 = !DIModule(scope: null, name: "Swift", configMacros: "\22-DDEBUG=1\22 \22-DFB_MARK_AUTH_INTERNAL=1\22 \22-DENABLE_NON_PRODUCTION_TOOLS=1\22 \22-DRCT_DEV_MENU=1\22 \22-DENABLE_LOGGING=1\22 \22-DRCTLOG_ENABLED=1\22 \22-DFB_SONARKIT_ENABLED=1\22", includePath: "buck-out/infer-fbobjc/art/fbsource/xplat/toolchains/apple/__xcode_26.2.0_17c52-iphonesimulator-sdk_genrule-actual__/e5e3cbbf737d97c8/iPhoneSimulator26.2.sdk/usr/lib/swift/Swift.swiftmodule/arm64-apple-ios-simulator.swiftinterface")
!25 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !26, file: !19)
!26 = !DIModule(scope: null, name: "Foundation", configMacros: "\22-DDEBUG=1\22 \22-DFB_MARK_AUTH_INTERNAL=1\22 \22-DENABLE_NON_PRODUCTION_TOOLS=1\22 \22-DRCT_DEV_MENU=1\22 \22-DENABLE_LOGGING=1\22 \22-DRCTLOG_ENABLED=1\22 \22-DFB_SONARKIT_ENABLED=1\22", includePath: ".")
!27 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !28, file: !19, line: 33)
!28 = !DIModule(scope: null, name: "Foundation", configMacros: "\22-DDEBUG=1\22 \22-DFB_MARK_AUTH_INTERNAL=1\22 \22-DENABLE_NON_PRODUCTION_TOOLS=1\22 \22-DRCT_DEV_MENU=1\22 \22-DENABLE_LOGGING=1\22 \22-DRCTLOG_ENABLED=1\22 \22-DFB_SONARKIT_ENABLED=1\22", includePath: "buck-out/infer-fbobjc/art/fbsource/xplat/toolchains/apple/__xcode_26.2.0_17c52-iphonesimulator-sdk_genrule-actual__/e5e3cbbf737d97c8/iPhoneSimulator26.2.sdk/System/Library/Frameworks/Foundation.framework/Modules/Foundation.swiftmodule/arm64-apple-ios-simulator.swiftinterface")
!29 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !30, file: !19)
!30 = !DIModule(scope: null, name: "_StringProcessing", configMacros: "\22-DDEBUG=1\22 \22-DFB_MARK_AUTH_INTERNAL=1\22 \22-DENABLE_NON_PRODUCTION_TOOLS=1\22 \22-DRCT_DEV_MENU=1\22 \22-DENABLE_LOGGING=1\22 \22-DRCTLOG_ENABLED=1\22 \22-DFB_SONARKIT_ENABLED=1\22", includePath: "buck-out/infer-fbobjc/art/fbsource/xplat/toolchains/apple/__xcode_26.2.0_17c52-iphonesimulator-sdk_genrule-actual__/e5e3cbbf737d97c8/iPhoneSimulator26.2.sdk/usr/lib/swift/_StringProcessing.swiftmodule/arm64-apple-ios-simulator.swiftinterface")
!31 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !32, file: !19)
!32 = !DIModule(scope: null, name: "_SwiftConcurrencyShims", configMacros: "\22-DDEBUG=1\22 \22-DFB_MARK_AUTH_INTERNAL=1\22 \22-DENABLE_NON_PRODUCTION_TOOLS=1\22 \22-DRCT_DEV_MENU=1\22 \22-DENABLE_LOGGING=1\22 \22-DRCTLOG_ENABLED=1\22 \22-DFB_SONARKIT_ENABLED=1\22", includePath: ".")
!33 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !34, file: !19)
!34 = !DIModule(scope: null, name: "_Concurrency", configMacros: "\22-DDEBUG=1\22 \22-DFB_MARK_AUTH_INTERNAL=1\22 \22-DENABLE_NON_PRODUCTION_TOOLS=1\22 \22-DRCT_DEV_MENU=1\22 \22-DENABLE_LOGGING=1\22 \22-DRCTLOG_ENABLED=1\22 \22-DFB_SONARKIT_ENABLED=1\22", includePath: "buck-out/infer-fbobjc/art/fbsource/xplat/toolchains/apple/__xcode_26.2.0_17c52-iphonesimulator-sdk_genrule-actual__/e5e3cbbf737d97c8/iPhoneSimulator26.2.sdk/usr/lib/swift/_Concurrency.swiftmodule/arm64-apple-ios-simulator.swiftinterface")
!35 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !36, file: !19)
!36 = !DIModule(scope: null, name: "SwiftOnoneSupport", configMacros: "\22-DDEBUG=1\22 \22-DFB_MARK_AUTH_INTERNAL=1\22 \22-DENABLE_NON_PRODUCTION_TOOLS=1\22 \22-DRCT_DEV_MENU=1\22 \22-DENABLE_LOGGING=1\22 \22-DRCTLOG_ENABLED=1\22 \22-DFB_SONARKIT_ENABLED=1\22", includePath: "buck-out/infer-fbobjc/art/fbsource/xplat/toolchains/apple/__xcode_26.2.0_17c52-iphonesimulator-sdk_genrule-actual__/e5e3cbbf737d97c8/iPhoneSimulator26.2.sdk/usr/lib/swift/SwiftOnoneSupport.swiftmodule/arm64-apple-ios-simulator.swiftinterface")
!37 = distinct !DICompileUnit(language: DW_LANG_ObjC, file: !38, producer: "Facebook clang version 21.1.6 (https://git.internal.tfbnw.net/repos/git/rw/osmeta/external/llvm-project 0c6fa689b924b2d8693c492a432e798094d66068)", isOptimized: false, runtimeVersion: 2, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: Apple, sysroot: "buck-out/infer-fbobjc/art/fbsource/xplat/toolchains/apple/__xcode_26.2.0_17c52-iphonesimulator-sdk_genrule-actual__/e5e3cbbf737d97c8/iPhoneSimulator26.2.sdk", sdk: "iPhoneSimulator26.2.sdk")
!38 = !DIFile(filename: "<swift-imported-modules>", directory: ".")
!39 = distinct !DICompileUnit(language: DW_LANG_ObjC, file: !40, producer: "Facebook Swift version 6.3.2-dev effective-5.10 (LLVM 0c6fa689b924b2d, Swift 999e243c7e9b3d1)", isOptimized: true, runtimeVersion: 0, splitDebugFilename: "buck-out/infer-fbobjc/art-anon/fbsource/9b3f7679ef0fa17explat/toolchains/apple/sdk_deps/xcode_26.2.0_17c52/iphonesimulator_arm64/8652183ef65d6e37/__Foundation.clang__/Foundation.pcm", emissionKind: FullDebug, dwoId: 18446744073709551614)
!40 = !DIFile(filename: "Foundation", directory: ".")
!41 = distinct !DICompileUnit(language: DW_LANG_ObjC, file: !42, producer: "Facebook Swift version 6.3.2-dev effective-5.10 (LLVM 0c6fa689b924b2d, Swift 999e243c7e9b3d1)", isOptimized: true, runtimeVersion: 0, splitDebugFilename: "buck-out/infer-fbobjc/art-anon/fbsource/9b3f7679ef0fa17explat/toolchains/apple/sdk_deps/xcode_26.2.0_17c52/iphonesimulator_arm64/b88a809b9af7bc18/___SwiftConcurrencyShims.clang__/_SwiftConcurrencyShims.pcm", emissionKind: FullDebug, dwoId: 18446744073709551614)
!42 = !DIFile(filename: "_SwiftConcurrencyShims", directory: ".")
!43 = !{!"standard-library", i1 false}
!44 = !{!"-lswiftFoundation"}
!45 = !{!"-framework", !"Foundation"}
!46 = !{!"-lswiftCore"}
!47 = !{!"-lswift_DarwinFoundation3"}
!48 = !{!"-lswift_DarwinFoundation1"}
!49 = !{!"-lswift_DarwinFoundation2"}
!50 = !{!"-lswift_StringProcessing"}
!51 = !{!"-lswift_Concurrency"}
!52 = !{!"-lswiftSystem"}
!53 = !{!"-lswiftDarwin"}
!54 = !{!"-lswift_Builtin_float"}
!55 = !{!"-lswiftObservation"}
!56 = !{!"-lswiftObjectiveC"}
!57 = !{!"-lswiftCoreFoundation"}
!58 = !{!"-framework", !"CoreFoundation"}
!59 = !{!"-lswiftDispatch"}
!60 = !{!"-framework", !"Combine"}
!61 = !{!"-framework", !"Security"}
!62 = !{!"-lswiftXPC"}
!63 = !{!"-framework", !"CFNetwork"}
!64 = !{!"-lswiftSwiftOnoneSupport"}
!65 = !{!"-lobjc"}
!66 = !{!"-lswiftCompatibility56"}
!67 = !{!"-lswiftCompatibilityPacks"}
!68 = distinct !DISubprogram(name: "counter.get", linkageName: "$s16AsyncActorGetter8AAGUtilsC7counterSivg", scope: !70, file: !69, type: !71, spFlags: DISPFlagDefinition, unit: !18, declaration: !74)
!69 = !DIFile(filename: "<compiler-generated>", directory: "")
!70 = !DICompositeType(tag: DW_TAG_structure_type, name: "AAGUtils", scope: !22, file: !19, size: 64, runtimeLang: DW_LANG_Swift, identifier: "$s16AsyncActorGetter8AAGUtilsCD")
!71 = !DISubroutineType(types: !72)
!72 = !{!73, !70}
!73 = !DICompositeType(tag: DW_TAG_structure_type, name: "Int", scope: !24, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$sSiD")
!74 = !DISubprogram(name: "counter.get", linkageName: "$s16AsyncActorGetter8AAGUtilsC7counterSivg", scope: !70, file: !69, type: !71, spFlags: 0)
!75 = !DILocation(line: 0, scope: !68)
!76 = distinct !DISubprogram(name: "counter.set", linkageName: "$s16AsyncActorGetter8AAGUtilsC7counterSivs", scope: !70, file: !69, type: !77, spFlags: DISPFlagDefinition, unit: !18, declaration: !80)
!77 = !DISubroutineType(types: !78)
!78 = !{!79, !73, !70}
!79 = !DICompositeType(tag: DW_TAG_structure_type, name: "$sytD", flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$sytD")
!80 = !DISubprogram(name: "counter.set", linkageName: "$s16AsyncActorGetter8AAGUtilsC7counterSivs", scope: !70, file: !69, type: !77, spFlags: 0)
!81 = !DILocation(line: 0, scope: !76)
!82 = distinct !DISubprogram(name: "deinit", linkageName: "$s16AsyncActorGetter8AAGUtilsCfd", scope: !70, file: !19, line: 35, type: !83, scopeLine: 35, spFlags: DISPFlagDefinition, unit: !18, declaration: !87, retainedNodes: !88)
!83 = !DISubroutineType(types: !84)
!84 = !{!85, !70}
!85 = !DICompositeType(tag: DW_TAG_structure_type, name: "$sBoD", scope: !86, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$sBoD")
!86 = !DIModule(scope: null, name: "Builtin", configMacros: "\22-DDEBUG=1\22 \22-DFB_MARK_AUTH_INTERNAL=1\22 \22-DENABLE_NON_PRODUCTION_TOOLS=1\22 \22-DRCT_DEV_MENU=1\22 \22-DENABLE_LOGGING=1\22 \22-DRCTLOG_ENABLED=1\22 \22-DFB_SONARKIT_ENABLED=1\22")
!87 = !DISubprogram(name: "deinit", linkageName: "$s16AsyncActorGetter8AAGUtilsCfd", scope: !70, file: !19, line: 35, type: !83, scopeLine: 35, spFlags: 0)
!88 = !{!89}
!89 = !DILocalVariable(name: "self", arg: 1, scope: !82, file: !19, line: 35, type: !90, flags: DIFlagArtificial)
!90 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !70)
!91 = !DILocation(line: 35, column: 13, scope: !82)
!92 = !DILocation(line: 0, scope: !82)
!93 = distinct !DISubprogram(name: "deinit", linkageName: "$s16AsyncActorGetter8AAGUtilsCfD", scope: !70, file: !19, line: 35, type: !94, scopeLine: 35, spFlags: DISPFlagDefinition, unit: !18, declaration: !96, retainedNodes: !97)
!94 = !DISubroutineType(types: !95)
!95 = !{!79, !70}
!96 = !DISubprogram(name: "deinit", linkageName: "$s16AsyncActorGetter8AAGUtilsCfD", scope: !70, file: !19, line: 35, type: !94, scopeLine: 35, spFlags: 0)
!97 = !{!98}
!98 = !DILocalVariable(name: "self", arg: 1, scope: !93, file: !19, line: 35, type: !90, flags: DIFlagArtificial)
!99 = !DILocation(line: 35, column: 13, scope: !93)
!100 = !DILocation(line: 0, scope: !93)
!101 = distinct !DISubprogram(name: "init", linkageName: "$s16AsyncActorGetter8AAGUtilsCACycfC", scope: !70, file: !19, line: 35, type: !102, scopeLine: 35, spFlags: DISPFlagDefinition, unit: !18, declaration: !105)
!102 = !DISubroutineType(types: !103)
!103 = !{!70, !104}
!104 = !DICompositeType(tag: DW_TAG_structure_type, name: "$s16AsyncActorGetter8AAGUtilsCXMTD", flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$s16AsyncActorGetter8AAGUtilsCXMTD")
!105 = !DISubprogram(name: "init", linkageName: "$s16AsyncActorGetter8AAGUtilsCACycfC", scope: !70, file: !19, line: 35, type: !102, scopeLine: 35, spFlags: 0)
!106 = !DILocation(line: 0, scope: !101)
!107 = distinct !DISubprogram(name: "init", linkageName: "$s16AsyncActorGetter8AAGUtilsCACycfc", scope: !70, file: !19, line: 35, type: !108, scopeLine: 35, spFlags: DISPFlagDefinition, unit: !18, declaration: !110, retainedNodes: !111)
!108 = !DISubroutineType(types: !109)
!109 = !{!70, !70}
!110 = !DISubprogram(name: "init", linkageName: "$s16AsyncActorGetter8AAGUtilsCACycfc", scope: !70, file: !19, line: 35, type: !108, scopeLine: 35, spFlags: 0)
!111 = !{!112}
!112 = !DILocalVariable(name: "self", arg: 1, scope: !107, file: !19, line: 35, type: !90, flags: DIFlagArtificial)
!113 = !DILocation(line: 35, column: 13, scope: !107)
!114 = !DILocation(line: 0, scope: !107)
!115 = !DILocation(line: 36, column: 7, scope: !116)
!116 = distinct !DILexicalBlock(scope: !117, file: !19, line: 36, column: 7)
!117 = distinct !DILexicalBlock(scope: !107, file: !19, line: 35, column: 22)
!118 = !DILocation(line: 36, column: 22, scope: !119)
!119 = distinct !DILexicalBlock(scope: !117, file: !19, line: 36, column: 22)
!120 = !DILocation(line: 0, scope: !119)
!121 = distinct !DISubprogram(name: "utils.get", linkageName: "$s16AsyncActorGetter10AAGHandlerC5utilsAA8AAGUtilsCvg", scope: !122, file: !69, type: !123, spFlags: DISPFlagDefinition, unit: !18, declaration: !125)
!122 = !DICompositeType(tag: DW_TAG_structure_type, name: "AAGHandler", scope: !22, file: !19, size: 64, runtimeLang: DW_LANG_Swift, identifier: "$s16AsyncActorGetter10AAGHandlerCD")
!123 = !DISubroutineType(types: !124)
!124 = !{!70, !122}
!125 = !DISubprogram(name: "utils.get", linkageName: "$s16AsyncActorGetter10AAGHandlerC5utilsAA8AAGUtilsCvg", scope: !122, file: !69, type: !123, spFlags: 0)
!126 = !DILocation(line: 0, scope: !121)
!127 = distinct !DISubprogram(name: "utils.set", linkageName: "$s16AsyncActorGetter10AAGHandlerC5utilsAA8AAGUtilsCvs", scope: !122, file: !69, type: !128, spFlags: DISPFlagDefinition, unit: !18, declaration: !130)
!128 = !DISubroutineType(types: !129)
!129 = !{!79, !70, !122}
!130 = !DISubprogram(name: "utils.set", linkageName: "$s16AsyncActorGetter10AAGHandlerC5utilsAA8AAGUtilsCvs", scope: !122, file: !69, type: !128, spFlags: 0)
!131 = !DILocation(line: 0, scope: !127)
!132 = distinct !DISubprogram(name: "utils.modify", linkageName: "$s16AsyncActorGetter10AAGHandlerC5utilsAA8AAGUtilsCvM", scope: !122, file: !69, type: !133, spFlags: DISPFlagDefinition, unit: !18, declaration: !135)
!133 = !DISubroutineType(types: !134)
!134 = !{!79, !122}
!135 = !DISubprogram(name: "utils.modify", linkageName: "$s16AsyncActorGetter10AAGHandlerC5utilsAA8AAGUtilsCvM", scope: !122, file: !69, type: !133, spFlags: 0)
!136 = !DILocation(line: 0, scope: !132)
!137 = distinct !DISubprogram(name: "utils.modify", linkageName: "$s16AsyncActorGetter10AAGHandlerC5utilsAA8AAGUtilsCvM.resume.0", scope: !122, file: !69, type: !133, spFlags: DISPFlagDefinition, unit: !18, declaration: !138)
!138 = !DISubprogram(name: "utils.modify", linkageName: "$s16AsyncActorGetter10AAGHandlerC5utilsAA8AAGUtilsCvM.resume.0", scope: !122, file: !69, type: !133, spFlags: 0)
!139 = !DILocation(line: 0, scope: !137)
!140 = distinct !DISubprogram(name: "init", linkageName: "$s16AsyncActorGetter10AAGHandlerC5utilsAcA8AAGUtilsC_tcfC", scope: !122, file: !19, line: 42, type: !141, scopeLine: 42, spFlags: DISPFlagDefinition, unit: !18, declaration: !144)
!141 = !DISubroutineType(types: !142)
!142 = !{!122, !70, !143}
!143 = !DICompositeType(tag: DW_TAG_structure_type, name: "$s16AsyncActorGetter10AAGHandlerCXMTD", flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$s16AsyncActorGetter10AAGHandlerCXMTD")
!144 = !DISubprogram(name: "init", linkageName: "$s16AsyncActorGetter10AAGHandlerC5utilsAcA8AAGUtilsC_tcfC", scope: !122, file: !19, line: 42, type: !141, scopeLine: 42, spFlags: 0)
!145 = !DILocation(line: 0, scope: !140)
!146 = distinct !DISubprogram(name: "init", linkageName: "$s16AsyncActorGetter10AAGHandlerC5utilsAcA8AAGUtilsC_tcfc", scope: !122, file: !19, line: 42, type: !147, scopeLine: 42, spFlags: DISPFlagDefinition, unit: !18, declaration: !149, retainedNodes: !150)
!147 = !DISubroutineType(types: !148)
!148 = !{!122, !70, !122}
!149 = !DISubprogram(name: "init", linkageName: "$s16AsyncActorGetter10AAGHandlerC5utilsAcA8AAGUtilsC_tcfc", scope: !122, file: !19, line: 42, type: !147, scopeLine: 42, spFlags: 0)
!150 = !{!151, !152}
!151 = !DILocalVariable(name: "utils", arg: 1, scope: !146, file: !19, line: 42, type: !90)
!152 = !DILocalVariable(name: "self", arg: 2, scope: !146, file: !19, line: 42, type: !153, flags: DIFlagArtificial)
!153 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !122)
!154 = !DILocation(line: 42, column: 8, scope: !146)
!155 = !DILocation(line: 42, column: 3, scope: !146)
!156 = !DILocation(line: 0, scope: !146)
!157 = !DILocation(line: 0, scope: !158)
!158 = !DILexicalBlockFile(scope: !146, discriminator: 0)
!159 = !DILocation(line: 43, column: 18, scope: !146)
!160 = !DILocation(line: 43, column: 16, scope: !146)
!161 = !DILocation(line: 44, column: 3, scope: !146)
!162 = distinct !DISubprogram(name: "read_utils_no_cycle_good_FP", linkageName: "$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaF", scope: !122, file: !19, line: 46, type: !163, scopeLine: 46, spFlags: DISPFlagDefinition, unit: !18, declaration: !165, retainedNodes: !166)
!163 = !DISubroutineType(types: !164)
!164 = !{!73, !122}
!165 = !DISubprogram(name: "read_utils_no_cycle_good_FP", linkageName: "$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaF", scope: !122, file: !19, line: 46, type: !163, scopeLine: 46, spFlags: 0)
!166 = !{!167}
!167 = !DILocalVariable(name: "self", arg: 1, scope: !162, file: !19, line: 46, type: !153, flags: DIFlagArtificial)
!168 = !DILocation(line: 46, column: 8, scope: !162)
!169 = !DILocation(line: 0, scope: !162)
!170 = !DILocation(line: 0, scope: !171)
!171 = !DILexicalBlockFile(scope: !162, discriminator: 0)
!172 = distinct !DISubprogram(name: "read_utils_no_cycle_good_FP", linkageName: "$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaFTY0_", scope: !122, file: !19, line: 46, type: !163, scopeLine: 47, spFlags: DISPFlagDefinition, unit: !18, declaration: !173, retainedNodes: !174)
!173 = !DISubprogram(name: "read_utils_no_cycle_good_FP", linkageName: "$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaFTY0_", scope: !122, file: !19, line: 46, type: !163, scopeLine: 46, spFlags: 0)
!174 = !{!175}
!175 = !DILocalVariable(name: "self", arg: 1, scope: !172, file: !19, line: 46, type: !153, flags: DIFlagArtificial)
!176 = !DILocation(line: 46, column: 8, scope: !172)
!177 = !DILocation(line: 47, column: 12, scope: !172)
!178 = !DILocation(line: 47, column: 18, scope: !172)
!179 = !DILocation(line: 0, scope: !172)
!180 = !DILocation(line: 47, column: 5, scope: !172)
!181 = distinct !DISubprogram(name: "deinit", linkageName: "$s16AsyncActorGetter10AAGHandlerCfd", scope: !122, file: !19, line: 39, type: !182, scopeLine: 39, spFlags: DISPFlagDefinition, unit: !18, declaration: !184, retainedNodes: !185)
!182 = !DISubroutineType(types: !183)
!183 = !{!85, !122}
!184 = !DISubprogram(name: "deinit", linkageName: "$s16AsyncActorGetter10AAGHandlerCfd", scope: !122, file: !19, line: 39, type: !182, scopeLine: 39, spFlags: 0)
!185 = !{!186}
!186 = !DILocalVariable(name: "self", arg: 1, scope: !181, file: !19, line: 39, type: !153, flags: DIFlagArtificial)
!187 = !DILocation(line: 39, column: 7, scope: !181)
!188 = !DILocation(line: 0, scope: !181)
!189 = distinct !DISubprogram(name: "deinit", linkageName: "$s16AsyncActorGetter10AAGHandlerCfD", scope: !122, file: !19, line: 39, type: !133, scopeLine: 39, spFlags: DISPFlagDefinition, unit: !18, declaration: !190, retainedNodes: !191)
!190 = !DISubprogram(name: "deinit", linkageName: "$s16AsyncActorGetter10AAGHandlerCfD", scope: !122, file: !19, line: 39, type: !133, scopeLine: 39, spFlags: 0)
!191 = !{!192}
!192 = !DILocalVariable(name: "self", arg: 1, scope: !189, file: !19, line: 39, type: !153, flags: DIFlagArtificial)
!193 = !DILocation(line: 39, column: 7, scope: !189)
!194 = !DILocation(line: 0, scope: !189)
!195 = distinct !DISubprogram(name: "unownedExecutor.get", linkageName: "$s16AsyncActorGetter10AAGHandlerC15unownedExecutorScevg", scope: !122, file: !69, type: !196, spFlags: DISPFlagDefinition, unit: !18, declaration: !199, retainedNodes: !200)
!196 = !DISubroutineType(types: !197)
!197 = !{!198, !122}
!198 = !DICompositeType(tag: DW_TAG_structure_type, name: "UnownedSerialExecutor", scope: !34, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$sSceD")
!199 = !DISubprogram(name: "unownedExecutor.get", linkageName: "$s16AsyncActorGetter10AAGHandlerC15unownedExecutorScevg", scope: !122, file: !69, type: !196, spFlags: 0)
!200 = !{!201}
!201 = !DILocalVariable(name: "self", arg: 1, scope: !195, file: !19, type: !153, flags: DIFlagArtificial)
!202 = !DILocation(line: 0, scope: !195)
!203 = distinct !DISubprogram(name: "unownedExecutor.get", linkageName: "$s16AsyncActorGetter10AAGHandlerCScAAAScA15unownedExecutorScevgTW", scope: !122, file: !69, type: !196, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !18, declaration: !204)
!204 = !DISubprogram(name: "unownedExecutor.get", linkageName: "$s16AsyncActorGetter10AAGHandlerCScAAAScA15unownedExecutorScevgTW", scope: !122, file: !69, type: !196, spFlags: DISPFlagLocalToUnit)
!205 = !DILocation(line: 0, scope: !203)
!206 = distinct !DISubprogram(name: "test_actor_property_access_no_cycle_good_FP", linkageName: "$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF", scope: !22, file: !19, line: 52, type: !207, scopeLine: 52, spFlags: DISPFlagDefinition, unit: !18, retainedNodes: !209)
!207 = !DISubroutineType(types: !208)
!208 = !{!79}
!209 = !{!210}
!210 = !DILocalVariable(name: "h", scope: !211, file: !19, line: 53, type: !153)
!211 = distinct !DILexicalBlock(scope: !206, file: !19, line: 53, column: 7)
!212 = !DILocation(line: 53, column: 7, scope: !211)
!213 = !DILocation(line: 0, scope: !206)
!214 = !DILocation(line: 52, column: 13, scope: !206)
!215 = distinct !DISubprogram(name: "test_actor_property_access_no_cycle_good_FP", linkageName: "$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaFTY0_", scope: !22, file: !19, line: 52, type: !207, scopeLine: 53, spFlags: DISPFlagDefinition, unit: !18, retainedNodes: !216)
!216 = !{!217}
!217 = !DILocalVariable(name: "h", scope: !218, file: !19, line: 53, type: !153)
!218 = distinct !DILexicalBlock(scope: !215, file: !19, line: 53, column: 7)
!219 = !DILocation(line: 53, column: 7, scope: !218)
!220 = !DILocation(line: 53, column: 11, scope: !221)
!221 = distinct !DILexicalBlock(scope: !215, file: !19, line: 53, column: 11)
!222 = !DILocation(line: 53, column: 29, scope: !221)
!223 = !DILocation(line: 0, scope: !218)
!224 = !DILocation(line: 0, scope: !225)
!225 = !DILexicalBlockFile(scope: !218, discriminator: 0)
!226 = !DILocation(line: 54, column: 15, scope: !218)
!227 = distinct !DISubprogram(name: "test_actor_property_access_no_cycle_good_FP", linkageName: "$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaFTQ1_", scope: !22, file: !19, line: 52, type: !207, scopeLine: 54, spFlags: DISPFlagDefinition, unit: !18, retainedNodes: !228)
!228 = !{!229}
!229 = !DILocalVariable(name: "h", scope: !230, file: !19, line: 53, type: !153)
!230 = distinct !DILexicalBlock(scope: !227, file: !19, line: 53, column: 7)
!231 = !DILocation(line: 53, column: 7, scope: !230)
!232 = !DILocation(line: 54, column: 15, scope: !230)
!233 = distinct !DISubprogram(name: "test_actor_property_access_no_cycle_good_FP", linkageName: "$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaFTY2_", scope: !22, file: !19, line: 52, type: !207, scopeLine: 55, spFlags: DISPFlagDefinition, unit: !18, retainedNodes: !234)
!234 = !{!235}
!235 = !DILocalVariable(name: "h", scope: !236, file: !19, line: 53, type: !153)
!236 = distinct !DILexicalBlock(scope: !233, file: !19, line: 53, column: 7)
!237 = !DILocation(line: 53, column: 7, scope: !236)
!238 = !DILocation(line: 55, column: 1, scope: !236)
!239 = distinct !DISubprogram(linkageName: "$sS2cMScAsWl", scope: !22, file: !69, type: !240, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !18)
!240 = !DISubroutineType(types: null)
!241 = !DILocation(line: 0, scope: !239)
!242 = distinct !DISubprogram(linkageName: "$s16AsyncActorGetter10AAGHandlerCMa", scope: !22, file: !69, type: !240, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !18)
!243 = !DILocation(line: 0, scope: !242)
!244 = distinct !DISubprogram(linkageName: "$s16AsyncActorGetter8AAGUtilsCMa", scope: !22, file: !69, type: !240, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !18)
!245 = !DILocation(line: 0, scope: !244)
