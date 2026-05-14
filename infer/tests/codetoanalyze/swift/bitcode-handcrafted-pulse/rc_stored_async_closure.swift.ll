; LLVM IR captured from a buck2 build of the source-level witness at
; fbobjc/Tools/infer/tests/codetoanalyze/swift/pulse/StoredAsyncClosure/RcStoredAsyncClosure.swift.
;
; Source:
;
;     actor SACImportService {
;         var onAssetNeedsDeduplication: (@Sendable (...) async -> Void)?
;         func clean_no_cycle_good_FP() async throws -> Int {
;             if let callback = onAssetNeedsDeduplication { ... }
;             return cleaned
;         }
;     }
;
; Pre-D104669671 Pulse reports a fictional cycle on `self->field_0->
; field_2` (production cluster shape: `self->onAssetNeedsDeduplication->
; field_2`) — the field name is `field_0` here because the swiftasync
; continuation thunk's `self` is treated as opaque post-fix, so DI never
; surfaces the [onAssetNeedsDeduplication] field name. Same root cause as
; the actor-getter FP fixed by D104669671: an async actor method's
; continuation thunk receives a [swiftasync]-attributed parameter that DI
; types as the user-visible Swift class, but at runtime points into the
; spilled Async Frame at a non-zero offset.
;
; Regression test: D104669671 marks every [swiftasync] parameter as opaque
; in [LlvmSledgeFrontend.xlate_function_decl], which defeats Pulse's typed
; field projection on the thunk body for any access shape (not just
; getter accessors). This test pins coverage of the non-getter,
; Optional-of-async-closure stored property variant so a future change to
; the swiftasync attribute detection can't silently regress the closure-
; reading branch.
; ModuleID = '/home/dulmarod/infer-astro/infer/infer/tests/codetoanalyze/swift/bitcode-handcrafted-pulse/rc_stored_async_closure.swift.ll'
source_filename = "buck-out/v2/art/fbsource/fbobjc/Tools/infer/tests/codetoanalyze/swift/pulse/__StoredAsyncClosure__/output_artifacts/StoredAsyncClosure.o"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-ios15.1.0-simulator"

%T18StoredAsyncClosure8SACAssetC = type <{ %swift.refcounted, %TSi }>
%swift.refcounted = type { ptr, i64 }
%TSi = type <{ i64 }>
%T18StoredAsyncClosure8SACAliasC = type <{ %swift.refcounted, %TSi }>
%"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame" = type { [24 x i8], %swift.function, ptr, ptr, %TSi, ptr, ptr, ptr, i64, ptr, ptr, ptr }
%swift.function = type { ptr, ptr }
%T18StoredAsyncClosure16SACImportServiceC = type <{ %swift.refcounted, %swift.defaultactor, %T18StoredAsyncClosure8SACAssetCAA8SACAliasCIeghHgg_Sg }>
%swift.defaultactor = type { [12 x ptr] }
%T18StoredAsyncClosure8SACAssetCAA8SACAliasCIeghHgg_Sg = type <{ [16 x i8] }>
%swift.metadata_response = type { ptr, i64 }
%swift.async_func_pointer = type <{ i32, i32 }>

@"$s18StoredAsyncClosure8SACAssetCMf" = external hidden global <{ ptr, ptr, ptr, i64, ptr, ptr, ptr, i64, i32, i32, i32, i16, i16, i32, i32, ptr, ptr, i64, ptr }>, align 8
@"$s18StoredAsyncClosure8SACAliasCMf" = external hidden global <{ ptr, ptr, ptr, i64, ptr, ptr, ptr, i64, i32, i32, i32, i16, i16, i32, i32, ptr, ptr, i64, ptr }>, align 8

define hidden swiftcc ptr @"$s18StoredAsyncClosure8SACAssetC2idACSi_tcfC"(i64 %0, ptr swiftself %1) #0 !dbg !62 {
  %3 = call noalias ptr @swift_allocObject(ptr %1, i64 24, i64 7) #1, !dbg !70
  %4 = call swiftcc ptr @"$s18StoredAsyncClosure8SACAssetC2idACSi_tcfc"(i64 %0, ptr swiftself %3), !dbg !70
  ret ptr %4, !dbg !70
}

; Function Attrs: nounwind
declare ptr @swift_allocObject(ptr, i64, i64) #1

define hidden swiftcc ptr @"$s18StoredAsyncClosure8SACAssetC2idACSi_tcfc"(i64 %0, ptr swiftself %1) #0 !dbg !71 {
  %3 = alloca i64, align 8
    #dbg_declare(ptr %3, !76, !DIExpression(), !80)
  call void @llvm.memset.p0.i64(ptr align 8 %3, i8 0, i64 8, i1 false)
  %4 = alloca ptr, align 8
    #dbg_declare(ptr %4, !78, !DIExpression(), !81)
  call void @llvm.memset.p0.i64(ptr align 8 %4, i8 0, i64 8, i1 false)
  store i64 %0, ptr %3, align 8, !dbg !82
  store ptr %1, ptr %4, align 8, !dbg !82
  %5 = getelementptr inbounds nuw %T18StoredAsyncClosure8SACAssetC, ptr %1, i32 0, i32 1, !dbg !83
  %6 = getelementptr inbounds nuw %TSi, ptr %5, i32 0, i32 0, !dbg !83
  store i64 %0, ptr %6, align 8, !dbg !83
  ret ptr %1, !dbg !84
}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: write)
declare void @llvm.memset.p0.i64(ptr writeonly captures(none), i8, i64, i1 immarg) #2

define hidden swiftcc ptr @"$s18StoredAsyncClosure8SACAliasC7assetIdACSi_tcfC"(i64 %0, ptr swiftself %1) #0 !dbg !85 {
  %3 = call noalias ptr @swift_allocObject(ptr %1, i64 24, i64 7) #1, !dbg !91
  %4 = call swiftcc ptr @"$s18StoredAsyncClosure8SACAliasC7assetIdACSi_tcfc"(i64 %0, ptr swiftself %3), !dbg !91
  ret ptr %4, !dbg !91
}

define hidden swiftcc ptr @"$s18StoredAsyncClosure8SACAliasC7assetIdACSi_tcfc"(i64 %0, ptr swiftself %1) #0 !dbg !92 {
  %3 = alloca i64, align 8
    #dbg_declare(ptr %3, !97, !DIExpression(), !100)
  call void @llvm.memset.p0.i64(ptr align 8 %3, i8 0, i64 8, i1 false)
  %4 = alloca ptr, align 8
    #dbg_declare(ptr %4, !98, !DIExpression(), !101)
  call void @llvm.memset.p0.i64(ptr align 8 %4, i8 0, i64 8, i1 false)
  store i64 %0, ptr %3, align 8, !dbg !102
  store ptr %1, ptr %4, align 8, !dbg !102
  %5 = getelementptr inbounds nuw %T18StoredAsyncClosure8SACAliasC, ptr %1, i32 0, i32 1, !dbg !103
  %6 = getelementptr inbounds nuw %TSi, ptr %5, i32 0, i32 0, !dbg !103
  store i64 %0, ptr %6, align 8, !dbg !103
  ret ptr %1, !dbg !104
}

; Function Attrs: nounwind
declare void @swift_beginAccess(ptr, ptr, i64, ptr) #1

; Function Attrs: noinline nounwind
define weak_odr hidden void @"$s18StoredAsyncClosure8SACAssetCAA8SACAliasCIeghHgg_SgWOy"(i64 %0, i64 %1) #3 !dbg !105 {
  %3 = icmp eq i64 %0, 0, !dbg !108
  br i1 %3, label %8, label %4, !dbg !108

4:                                                ; preds = %2
  %5 = inttoptr i64 %0 to ptr, !dbg !108
  %6 = inttoptr i64 %1 to ptr, !dbg !108
  %7 = call ptr @swift_retain(ptr returned %6) #4, !dbg !108
  br label %8, !dbg !108

8:                                                ; preds = %4, %2
  ret void, !dbg !108
}

; Function Attrs: nounwind willreturn
declare ptr @swift_retain(ptr returned) #4

; Function Attrs: nounwind
declare void @swift_endAccess(ptr) #1

; Function Attrs: nounwind
declare void @swift_release(ptr) #1

; Function Attrs: noinline
define hidden swifttailcc void @"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF"(ptr swiftasync %0, ptr swiftself %1) #5 !dbg !109 {
  %3 = alloca ptr, align 8
    #dbg_declare(ptr %3, !129, !DIExpression(DW_OP_deref, DW_OP_plus_uconst, 88), !131)
    #dbg_declare(ptr %3, !126, !DIExpression(DW_OP_deref, DW_OP_plus_uconst, 80), !132)
    #dbg_declare(ptr %3, !121, !DIExpression(DW_OP_deref, DW_OP_plus_uconst, 40), !133)
    #dbg_declare(ptr %3, !119, !DIExpression(DW_OP_deref, DW_OP_plus_uconst, 72), !134)
    #dbg_declare(ptr %3, !117, !DIExpression(DW_OP_deref, DW_OP_plus_uconst, 64), !135)
  store ptr %0, ptr %3, align 8
  %4 = getelementptr inbounds i8, ptr %0, i32 16
  %5 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %4, i32 0, i32 2
  %6 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %4, i32 0, i32 3
  %7 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %4, i32 0, i32 4
  %8 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %4, i32 0, i32 0
  %9 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %4, i32 0, i32 1
  %10 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %4, i32 0, i32 5
  %11 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %4, i32 0, i32 6
  %12 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %4, i32 0, i32 7
  store ptr %1, ptr %12, align 8
  store ptr %0, ptr %5, align 8
  call void @llvm.memset.p0.i64(ptr align 8 %6, i8 0, i64 8, i1 false)
  call void @llvm.memset.p0.i64(ptr align 8 %7, i8 0, i64 8, i1 false)
  call void @llvm.memset.p0.i64(ptr align 8 %9, i8 0, i64 16, i1 false)
  call void @llvm.memset.p0.i64(ptr align 8 %10, i8 0, i64 8, i1 false)
  call void @llvm.memset.p0.i64(ptr align 8 %11, i8 0, i64 8, i1 false)
  store ptr %1, ptr %6, align 8, !dbg !136
  call void asm sideeffect "", "r"(ptr %6), !dbg !137
  %13 = ptrtoint ptr %1 to i64, !dbg !135
  %14 = load ptr, ptr %5, align 8, !dbg !135
  %15 = load ptr, ptr %5, align 8, !dbg !135
  musttail call swifttailcc void @swift_task_switch(ptr swiftasync %15, ptr @"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKFTY0_", i64 %13, i64 0) #1, !dbg !135
  ret void, !dbg !135
}

; Function Attrs: noinline
define hidden swifttailcc void @"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKFTY0_"(ptr swiftasync %0) #6 !dbg !139 {
    #dbg_declare(ptr %0, !151, !DIExpression(DW_OP_LLVM_entry_value, 1, DW_OP_plus_uconst, 88), !153)
    #dbg_declare(ptr %0, !148, !DIExpression(DW_OP_LLVM_entry_value, 1, DW_OP_plus_uconst, 80), !154)
    #dbg_declare(ptr %0, !145, !DIExpression(DW_OP_LLVM_entry_value, 1, DW_OP_plus_uconst, 40), !155)
    #dbg_declare(ptr %0, !143, !DIExpression(DW_OP_LLVM_entry_value, 1, DW_OP_plus_uconst, 72), !156)
    #dbg_declare(ptr %0, !142, !DIExpression(DW_OP_LLVM_entry_value, 1, DW_OP_plus_uconst, 64), !157)
  %2 = getelementptr inbounds i8, ptr %0, i32 16
  %3 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %2, i32 0, i32 2
  %4 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %2, i32 0, i32 3
  %5 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %2, i32 0, i32 4
  %6 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %2, i32 0, i32 0
  %7 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %2, i32 0, i32 1
  %8 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %2, i32 0, i32 5
  %9 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %2, i32 0, i32 6
  %10 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %2, i32 0, i32 7
  %11 = load ptr, ptr %10, align 8
  store ptr %0, ptr %3, align 8
  %12 = getelementptr inbounds nuw %TSi, ptr %5, i32 0, i32 0, !dbg !158
  store i64 0, ptr %12, align 8, !dbg !158
  %13 = getelementptr inbounds nuw %T18StoredAsyncClosure16SACImportServiceC, ptr %11, i32 0, i32 2, !dbg !160
  call void @swift_beginAccess(ptr %13, ptr %6, i64 32, ptr null) #1, !dbg !160
  %14 = getelementptr inbounds nuw { i64, i64 }, ptr %13, i32 0, i32 0, !dbg !160
  %15 = load i64, ptr %14, align 16, !dbg !160
  %16 = getelementptr inbounds nuw { i64, i64 }, ptr %13, i32 0, i32 1, !dbg !160
  %17 = load i64, ptr %16, align 8, !dbg !160
  %18 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %2, i32 0, i32 8, !dbg !160
  store i64 %17, ptr %18, align 8, !dbg !160
  call void @"$s18StoredAsyncClosure8SACAssetCAA8SACAliasCIeghHgg_SgWOy"(i64 %15, i64 %17), !dbg !160
  call void @swift_endAccess(ptr %6) #1, !dbg !160
  call void asm sideeffect "", "r"(ptr %4), !dbg !160
  %19 = icmp eq i64 %15, 0, !dbg !160
  br i1 %19, label %48, label %20, !dbg !160

20:                                               ; preds = %1
  %21 = inttoptr i64 %15 to ptr, !dbg !160
  %22 = inttoptr i64 %17 to ptr, !dbg !160
  %23 = getelementptr inbounds nuw %swift.function, ptr %7, i32 0, i32 0, !dbg !161
  store ptr %21, ptr %23, align 8, !dbg !161
  %24 = getelementptr inbounds nuw %swift.function, ptr %7, i32 0, i32 1, !dbg !161
  store ptr %22, ptr %24, align 8, !dbg !161
  call void asm sideeffect "", "r"(ptr %7), !dbg !161
  %25 = call swiftcc %swift.metadata_response @"$s18StoredAsyncClosure8SACAssetCMa"(i64 0) #10, !dbg !162
  %26 = extractvalue %swift.metadata_response %25, 0, !dbg !162
  %27 = call swiftcc ptr @"$s18StoredAsyncClosure8SACAssetC2idACSi_tcfC"(i64 0, ptr swiftself %26), !dbg !162
  %28 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %2, i32 0, i32 9, !dbg !164
  store ptr %27, ptr %28, align 8, !dbg !164
  store ptr %27, ptr %8, align 8, !dbg !164
  call void asm sideeffect "", "r"(ptr %8), !dbg !165
  %29 = call swiftcc %swift.metadata_response @"$s18StoredAsyncClosure8SACAliasCMa"(i64 0) #10, !dbg !167
  %30 = extractvalue %swift.metadata_response %29, 0, !dbg !167
  %31 = call swiftcc ptr @"$s18StoredAsyncClosure8SACAliasC7assetIdACSi_tcfC"(i64 0, ptr swiftself %30), !dbg !167
  %32 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %2, i32 0, i32 10, !dbg !169
  store ptr %31, ptr %32, align 8, !dbg !169
  store ptr %31, ptr %9, align 8, !dbg !169
  call void asm sideeffect "", "r"(ptr %9), !dbg !170
  %33 = call ptr @swift_retain(ptr returned %22) #4, !dbg !172
  %34 = getelementptr inbounds nuw %swift.async_func_pointer, ptr %21, i32 0, i32 0, !dbg !172
  %35 = load i32, ptr %34, align 8, !dbg !172
  %36 = sext i32 %35 to i64, !dbg !172
  %37 = ptrtoint ptr %34 to i64, !dbg !172
  %38 = add i64 %37, %36, !dbg !172
  %39 = inttoptr i64 %38 to ptr, !dbg !172
  %40 = getelementptr inbounds nuw %swift.async_func_pointer, ptr %21, i32 0, i32 1, !dbg !172
  %41 = load i32, ptr %40, align 8, !dbg !172
  %42 = zext i32 %41 to i64, !dbg !172
  %43 = call swiftcc ptr @swift_task_alloc(i64 %42) #9, !dbg !172
  %44 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %2, i32 0, i32 11, !dbg !172
  store ptr %43, ptr %44, align 8, !dbg !172
  %45 = load ptr, ptr %3, align 8, !dbg !172
  %46 = getelementptr inbounds nuw <{ ptr, ptr }>, ptr %43, i32 0, i32 0, !dbg !172
  store ptr %45, ptr %46, align 8, !dbg !172
  %47 = getelementptr inbounds nuw <{ ptr, ptr }>, ptr %43, i32 0, i32 1, !dbg !172
  store ptr @"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKFTQ1_", ptr %47, align 8, !dbg !172
  musttail call swifttailcc void %39(ptr swiftasync %43, ptr %27, ptr %31, ptr swiftself %22) #1, !dbg !172
  ret void, !dbg !172

48:                                               ; preds = %1
  call void asm sideeffect "", "r"(ptr %4), !dbg !160
  call void asm sideeffect "", "r"(ptr %4), !dbg !173
  %49 = load ptr, ptr %3, align 8, !dbg !173
  %50 = getelementptr inbounds nuw <{ ptr, ptr }>, ptr %49, i32 0, i32 1, !dbg !173
  %51 = load ptr, ptr %50, align 8, !dbg !173
  %52 = load ptr, ptr %3, align 8, !dbg !173
  musttail call swifttailcc void %51(ptr swiftasync %52, i64 0, ptr swiftself null) #1, !dbg !173
  ret void, !dbg !173
}

; Function Attrs: noinline
define hidden swifttailcc void @"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKFTQ1_"(ptr swiftasync %0) #7 !dbg !174 {
    #dbg_declare(ptr %0, !186, !DIExpression(DW_OP_LLVM_entry_value, 1, DW_OP_deref, DW_OP_plus_uconst, 88), !188)
    #dbg_declare(ptr %0, !183, !DIExpression(DW_OP_LLVM_entry_value, 1, DW_OP_deref, DW_OP_plus_uconst, 80), !189)
    #dbg_declare(ptr %0, !180, !DIExpression(DW_OP_LLVM_entry_value, 1, DW_OP_deref, DW_OP_plus_uconst, 40), !190)
    #dbg_declare(ptr %0, !178, !DIExpression(DW_OP_LLVM_entry_value, 1, DW_OP_deref, DW_OP_plus_uconst, 72), !191)
    #dbg_declare(ptr %0, !177, !DIExpression(DW_OP_LLVM_entry_value, 1, DW_OP_deref, DW_OP_plus_uconst, 64), !192)
  %2 = load ptr, ptr %0, align 8, !dbg !193
  %3 = call ptr @llvm.swift.async.context.addr(), !dbg !193
  store ptr %2, ptr %3, align 8, !dbg !193
  %4 = getelementptr inbounds i8, ptr %2, i32 16
  %5 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %4, i32 0, i32 2
  %6 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %4, i32 0, i32 3
  %7 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %4, i32 0, i32 4
  %8 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %4, i32 0, i32 0
  %9 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %4, i32 0, i32 1
  %10 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %4, i32 0, i32 5
  %11 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %4, i32 0, i32 6
  %12 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %4, i32 0, i32 11, !dbg !194
  %13 = load ptr, ptr %12, align 8, !dbg !194
  %14 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %4, i32 0, i32 8, !dbg !194
  %15 = load i64, ptr %14, align 8, !dbg !194
  %16 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %4, i32 0, i32 7, !dbg !194
  %17 = load ptr, ptr %16, align 8, !dbg !194
  %18 = inttoptr i64 %15 to ptr, !dbg !194
  %19 = load ptr, ptr %0, align 8
  store ptr %19, ptr %5, align 8
  call swiftcc void @swift_task_dealloc(ptr %13) #9, !dbg !193
  call void @swift_release(ptr %18) #1, !dbg !193
  %20 = load ptr, ptr %5, align 8, !dbg !193
  %21 = ptrtoint ptr %17 to i64, !dbg !192
  musttail call swifttailcc void @swift_task_switch(ptr swiftasync %20, ptr @"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKFTY2_", i64 %21, i64 0) #1, !dbg !193
  ret void, !dbg !193
}

; Function Attrs: noinline
define hidden swifttailcc void @"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKFTY2_"(ptr swiftasync %0) #6 !dbg !195 {
    #dbg_declare(ptr %0, !207, !DIExpression(DW_OP_LLVM_entry_value, 1, DW_OP_plus_uconst, 88), !209)
    #dbg_declare(ptr %0, !204, !DIExpression(DW_OP_LLVM_entry_value, 1, DW_OP_plus_uconst, 80), !210)
    #dbg_declare(ptr %0, !201, !DIExpression(DW_OP_LLVM_entry_value, 1, DW_OP_plus_uconst, 40), !211)
    #dbg_declare(ptr %0, !199, !DIExpression(DW_OP_LLVM_entry_value, 1, DW_OP_plus_uconst, 72), !212)
    #dbg_declare(ptr %0, !198, !DIExpression(DW_OP_LLVM_entry_value, 1, DW_OP_plus_uconst, 64), !213)
  %2 = getelementptr inbounds i8, ptr %0, i32 16
  %3 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %2, i32 0, i32 2
  %4 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %2, i32 0, i32 3
  %5 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %2, i32 0, i32 4
  %6 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %2, i32 0, i32 0
  %7 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %2, i32 0, i32 1
  %8 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %2, i32 0, i32 5
  %9 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %2, i32 0, i32 6
  %10 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %2, i32 0, i32 10, !dbg !214
  %11 = load ptr, ptr %10, align 8, !dbg !214
  %12 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %2, i32 0, i32 9, !dbg !214
  %13 = load ptr, ptr %12, align 8, !dbg !214
  %14 = getelementptr inbounds %"$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF.Frame", ptr %2, i32 0, i32 8, !dbg !214
  %15 = load i64, ptr %14, align 8, !dbg !214
  %16 = inttoptr i64 %15 to ptr, !dbg !214
  store ptr %0, ptr %3, align 8
  %17 = getelementptr inbounds nuw %TSi, ptr %5, i32 0, i32 0, !dbg !215
  store i64 1, ptr %17, align 8, !dbg !215
  call void @swift_release(ptr %11) #1, !dbg !216
  call void @swift_release(ptr %13) #1, !dbg !216
  call void @swift_release(ptr %16) #1, !dbg !216
  call void asm sideeffect "", "r"(ptr %4), !dbg !216
  call void asm sideeffect "", "r"(ptr %7), !dbg !216
  call void asm sideeffect "", "r"(ptr %8), !dbg !216
  call void asm sideeffect "", "r"(ptr %9), !dbg !216
  call void asm sideeffect "", "r"(ptr %4), !dbg !217
  %18 = load ptr, ptr %3, align 8, !dbg !217
  %19 = getelementptr inbounds nuw <{ ptr, ptr }>, ptr %18, i32 0, i32 1, !dbg !217
  %20 = load ptr, ptr %19, align 8, !dbg !217
  %21 = load ptr, ptr %3, align 8, !dbg !217
  musttail call swifttailcc void %20(ptr swiftasync %21, i64 1, ptr swiftself null) #1, !dbg !217
  ret void, !dbg !217
}

; Function Attrs: nounwind
declare swifttailcc void @swift_task_switch(ptr, ptr, i64, i64) #1

; Function Attrs: nounwind
declare ptr @llvm.swift.async.context.addr() #1

; Function Attrs: noinline nounwind memory(none)
define hidden swiftcc %swift.metadata_response @"$s18StoredAsyncClosure8SACAssetCMa"(i64 %0) #8 !dbg !218 {
  %2 = call ptr @objc_opt_self(ptr getelementptr inbounds (<{ ptr, ptr, ptr, i64, ptr, ptr, ptr, i64, i32, i32, i32, i16, i16, i32, i32, ptr, ptr, i64, ptr }>, ptr @"$s18StoredAsyncClosure8SACAssetCMf", i32 0, i32 3)) #1, !dbg !219
  %3 = insertvalue %swift.metadata_response undef, ptr %2, 0, !dbg !219
  %4 = insertvalue %swift.metadata_response %3, i64 0, 1, !dbg !219
  ret %swift.metadata_response %4, !dbg !219
}

; Function Attrs: noinline nounwind memory(none)
define hidden swiftcc %swift.metadata_response @"$s18StoredAsyncClosure8SACAliasCMa"(i64 %0) #8 !dbg !220 {
  %2 = call ptr @objc_opt_self(ptr getelementptr inbounds (<{ ptr, ptr, ptr, i64, ptr, ptr, ptr, i64, i32, i32, i32, i16, i16, i32, i32, ptr, ptr, i64, ptr }>, ptr @"$s18StoredAsyncClosure8SACAliasCMf", i32 0, i32 3)) #1, !dbg !221
  %3 = insertvalue %swift.metadata_response undef, ptr %2, 0, !dbg !221
  %4 = insertvalue %swift.metadata_response %3, i64 0, 1, !dbg !221
  ret %swift.metadata_response %4, !dbg !221
}

; Function Attrs: nounwind memory(inaccessiblemem: readwrite)
declare swiftcc ptr @swift_task_alloc(i64) #9

; Function Attrs: nounwind memory(inaccessiblemem: readwrite)
declare swiftcc void @swift_task_dealloc(ptr) #9

; Function Attrs: nounwind
declare ptr @objc_opt_self(ptr) #1

attributes #0 = { "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+altnzcv,+ccdp,+ccidx,+ccpp,+complxnum,+crc,+dit,+dotprod,+flagm,+fp-armv8,+fp16fml,+fptoint,+fullfp16,+jsconv,+lse,+neon,+pauth,+perfmon,+predres,+ras,+rcpc,+rdm,+sb,+sha2,+sha3,+specrestrict,+ssbs,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8a" }
attributes #1 = { nounwind }
attributes #2 = { nocallback nofree nounwind willreturn memory(argmem: write) }
attributes #3 = { noinline nounwind "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+altnzcv,+ccdp,+ccidx,+ccpp,+complxnum,+crc,+dit,+dotprod,+flagm,+fp-armv8,+fp16fml,+fptoint,+fullfp16,+jsconv,+lse,+neon,+pauth,+perfmon,+predres,+ras,+rcpc,+rdm,+sb,+sha2,+sha3,+specrestrict,+ssbs,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8a" }
attributes #4 = { nounwind willreturn }
attributes #5 = { noinline "async_entry" "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+altnzcv,+ccdp,+ccidx,+ccpp,+complxnum,+crc,+dit,+dotprod,+flagm,+fp-armv8,+fp16fml,+fptoint,+fullfp16,+jsconv,+lse,+neon,+pauth,+perfmon,+predres,+ras,+rcpc,+rdm,+sb,+sha2,+sha3,+specrestrict,+ssbs,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8a" }
attributes #6 = { noinline "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+altnzcv,+ccdp,+ccidx,+ccpp,+complxnum,+crc,+dit,+dotprod,+flagm,+fp-armv8,+fp16fml,+fptoint,+fullfp16,+jsconv,+lse,+neon,+pauth,+perfmon,+predres,+ras,+rcpc,+rdm,+sb,+sha2,+sha3,+specrestrict,+ssbs,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8a" }
attributes #7 = { noinline "async_ret" "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+altnzcv,+ccdp,+ccidx,+ccpp,+complxnum,+crc,+dit,+dotprod,+flagm,+fp-armv8,+fp16fml,+fptoint,+fullfp16,+jsconv,+lse,+neon,+pauth,+perfmon,+predres,+ras,+rcpc,+rdm,+sb,+sha2,+sha3,+specrestrict,+ssbs,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8a" }
attributes #8 = { noinline nounwind memory(none) "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+altnzcv,+ccdp,+ccidx,+ccpp,+complxnum,+crc,+dit,+dotprod,+flagm,+fp-armv8,+fp16fml,+fptoint,+fullfp16,+jsconv,+lse,+neon,+pauth,+perfmon,+predres,+ras,+rcpc,+rdm,+sb,+sha2,+sha3,+specrestrict,+ssbs,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8a" }
attributes #9 = { nounwind memory(inaccessiblemem: readwrite) }
attributes #10 = { nounwind memory(none) }

!llvm.module.flags = !{!0, !1, !2, !3, !4, !5, !6, !7, !8, !9, !10, !11, !12, !13, !14, !15, !16, !17}
!llvm.dbg.cu = !{!18}
!swift.module.flags = !{!37}
!llvm.linker.options = !{!38, !39, !40, !41, !42, !43, !44, !45, !46, !47, !48, !49, !50, !51, !52, !53, !54, !55, !56, !57, !58, !59, !60, !61}

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
!18 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !19, producer: "Facebook Swift version 6.3.2-dev effective-5.10 (LLVM 2cbe94970e91d85, Swift 999e243c7e9b3d1)", isOptimized: false, runtimeVersion: 6, emissionKind: FullDebug, imports: !20, sysroot: "buck-out/v2/art/fbsource/xplat/toolchains/apple/__xcode_26.2.0_17c52-iphonesimulator-sdk_genrule-actual__/e5e3cbbf737d97c8/iPhoneSimulator26.2.sdk", sdk: "iPhoneSimulator26.2.sdk")
!19 = !DIFile(filename: "fbobjc/Tools/infer/tests/codetoanalyze/swift/pulse/StoredAsyncClosure/RcStoredAsyncClosure.swift", directory: ".")
!20 = !{!21, !23, !25, !27, !29, !31, !33, !35}
!21 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !22, file: !19)
!22 = !DIModule(scope: null, name: "StoredAsyncClosure", includePath: "fbobjc/Tools/infer/tests/codetoanalyze/swift/pulse/StoredAsyncClosure")
!23 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !24, file: !19)
!24 = !DIModule(scope: null, name: "Swift", configMacros: "\22-DDEBUG=1\22 \22-DFB_MARK_AUTH_INTERNAL=1\22 \22-DENABLE_NON_PRODUCTION_TOOLS=1\22 \22-DRCT_DEV_MENU=1\22 \22-DENABLE_LOGGING=1\22 \22-DRCTLOG_ENABLED=1\22 \22-DFB_SONARKIT_ENABLED=1\22", includePath: "buck-out/v2/art/fbsource/xplat/toolchains/apple/__xcode_26.2.0_17c52-iphonesimulator-sdk_genrule-actual__/e5e3cbbf737d97c8/iPhoneSimulator26.2.sdk/usr/lib/swift/Swift.swiftmodule/arm64-apple-ios-simulator.swiftinterface")
!25 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !26, file: !19)
!26 = !DIModule(scope: null, name: "Foundation", configMacros: "\22-DDEBUG=1\22 \22-DFB_MARK_AUTH_INTERNAL=1\22 \22-DENABLE_NON_PRODUCTION_TOOLS=1\22 \22-DRCT_DEV_MENU=1\22 \22-DENABLE_LOGGING=1\22 \22-DRCTLOG_ENABLED=1\22 \22-DFB_SONARKIT_ENABLED=1\22", includePath: ".")
!27 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !28, file: !19, line: 25)
!28 = !DIModule(scope: null, name: "Foundation", configMacros: "\22-DDEBUG=1\22 \22-DFB_MARK_AUTH_INTERNAL=1\22 \22-DENABLE_NON_PRODUCTION_TOOLS=1\22 \22-DRCT_DEV_MENU=1\22 \22-DENABLE_LOGGING=1\22 \22-DRCTLOG_ENABLED=1\22 \22-DFB_SONARKIT_ENABLED=1\22", includePath: "buck-out/v2/art/fbsource/xplat/toolchains/apple/__xcode_26.2.0_17c52-iphonesimulator-sdk_genrule-actual__/e5e3cbbf737d97c8/iPhoneSimulator26.2.sdk/System/Library/Frameworks/Foundation.framework/Modules/Foundation.swiftmodule/arm64-apple-ios-simulator.swiftinterface")
!29 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !30, file: !19)
!30 = !DIModule(scope: null, name: "_StringProcessing", configMacros: "\22-DDEBUG=1\22 \22-DFB_MARK_AUTH_INTERNAL=1\22 \22-DENABLE_NON_PRODUCTION_TOOLS=1\22 \22-DRCT_DEV_MENU=1\22 \22-DENABLE_LOGGING=1\22 \22-DRCTLOG_ENABLED=1\22 \22-DFB_SONARKIT_ENABLED=1\22", includePath: "buck-out/v2/art/fbsource/xplat/toolchains/apple/__xcode_26.2.0_17c52-iphonesimulator-sdk_genrule-actual__/e5e3cbbf737d97c8/iPhoneSimulator26.2.sdk/usr/lib/swift/_StringProcessing.swiftmodule/arm64-apple-ios-simulator.swiftinterface")
!31 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !32, file: !19)
!32 = !DIModule(scope: null, name: "_SwiftConcurrencyShims", configMacros: "\22-DDEBUG=1\22 \22-DFB_MARK_AUTH_INTERNAL=1\22 \22-DENABLE_NON_PRODUCTION_TOOLS=1\22 \22-DRCT_DEV_MENU=1\22 \22-DENABLE_LOGGING=1\22 \22-DRCTLOG_ENABLED=1\22 \22-DFB_SONARKIT_ENABLED=1\22", includePath: ".")
!33 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !34, file: !19)
!34 = !DIModule(scope: null, name: "_Concurrency", configMacros: "\22-DDEBUG=1\22 \22-DFB_MARK_AUTH_INTERNAL=1\22 \22-DENABLE_NON_PRODUCTION_TOOLS=1\22 \22-DRCT_DEV_MENU=1\22 \22-DENABLE_LOGGING=1\22 \22-DRCTLOG_ENABLED=1\22 \22-DFB_SONARKIT_ENABLED=1\22", includePath: "buck-out/v2/art/fbsource/xplat/toolchains/apple/__xcode_26.2.0_17c52-iphonesimulator-sdk_genrule-actual__/e5e3cbbf737d97c8/iPhoneSimulator26.2.sdk/usr/lib/swift/_Concurrency.swiftmodule/arm64-apple-ios-simulator.swiftinterface")
!35 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !36, file: !19)
!36 = !DIModule(scope: null, name: "SwiftOnoneSupport", configMacros: "\22-DDEBUG=1\22 \22-DFB_MARK_AUTH_INTERNAL=1\22 \22-DENABLE_NON_PRODUCTION_TOOLS=1\22 \22-DRCT_DEV_MENU=1\22 \22-DENABLE_LOGGING=1\22 \22-DRCTLOG_ENABLED=1\22 \22-DFB_SONARKIT_ENABLED=1\22", includePath: "buck-out/v2/art/fbsource/xplat/toolchains/apple/__xcode_26.2.0_17c52-iphonesimulator-sdk_genrule-actual__/e5e3cbbf737d97c8/iPhoneSimulator26.2.sdk/usr/lib/swift/SwiftOnoneSupport.swiftmodule/arm64-apple-ios-simulator.swiftinterface")
!37 = !{!"standard-library", i1 false}
!38 = !{!"-lswiftFoundation"}
!39 = !{!"-framework", !"Foundation"}
!40 = !{!"-lswiftCore"}
!41 = !{!"-lswift_DarwinFoundation3"}
!42 = !{!"-lswift_DarwinFoundation1"}
!43 = !{!"-lswift_DarwinFoundation2"}
!44 = !{!"-lswift_StringProcessing"}
!45 = !{!"-lswift_Concurrency"}
!46 = !{!"-lswiftSystem"}
!47 = !{!"-lswiftDarwin"}
!48 = !{!"-lswift_Builtin_float"}
!49 = !{!"-lswiftObservation"}
!50 = !{!"-lswiftObjectiveC"}
!51 = !{!"-lswiftCoreFoundation"}
!52 = !{!"-framework", !"CoreFoundation"}
!53 = !{!"-lswiftDispatch"}
!54 = !{!"-framework", !"Combine"}
!55 = !{!"-framework", !"Security"}
!56 = !{!"-lswiftXPC"}
!57 = !{!"-framework", !"CFNetwork"}
!58 = !{!"-lswiftSwiftOnoneSupport"}
!59 = !{!"-lobjc"}
!60 = !{!"-lswiftCompatibility56"}
!61 = !{!"-lswiftCompatibilityPacks"}
!62 = distinct !DISubprogram(name: "init", linkageName: "$s18StoredAsyncClosure8SACAssetC2idACSi_tcfC", scope: !63, file: !19, line: 29, type: !64, scopeLine: 29, spFlags: DISPFlagDefinition, unit: !18, declaration: !69)
!63 = !DICompositeType(tag: DW_TAG_structure_type, name: "SACAsset", scope: !22, file: !19, size: 64, runtimeLang: DW_LANG_Swift, identifier: "$s18StoredAsyncClosure8SACAssetCD")
!64 = !DISubroutineType(types: !65)
!65 = !{!63, !66, !68}
!66 = !DICompositeType(tag: DW_TAG_structure_type, name: "Int", scope: !24, file: !67, size: 64, runtimeLang: DW_LANG_Swift, identifier: "$sSiD")
!67 = !DIFile(filename: "buck-out/v2/art/fbsource/xplat/toolchains/apple/__xcode_26.2.0_17c52-iphonesimulator-sdk_genrule-actual__/e5e3cbbf737d97c8/iPhoneSimulator26.2.sdk/usr/lib/swift/Swift.swiftmodule/arm64-apple-ios-simulator.swiftinterface", directory: ".")
!68 = !DICompositeType(tag: DW_TAG_structure_type, name: "$s18StoredAsyncClosure8SACAssetCXMTD", flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$s18StoredAsyncClosure8SACAssetCXMTD")
!69 = !DISubprogram(name: "init", linkageName: "$s18StoredAsyncClosure8SACAssetC2idACSi_tcfC", scope: !63, file: !19, line: 29, type: !64, scopeLine: 29, spFlags: 0)
!70 = !DILocation(line: 0, scope: !62)
!71 = distinct !DISubprogram(name: "init", linkageName: "$s18StoredAsyncClosure8SACAssetC2idACSi_tcfc", scope: !63, file: !19, line: 29, type: !72, scopeLine: 29, spFlags: DISPFlagDefinition, unit: !18, declaration: !74, retainedNodes: !75)
!72 = !DISubroutineType(types: !73)
!73 = !{!63, !66, !63}
!74 = !DISubprogram(name: "init", linkageName: "$s18StoredAsyncClosure8SACAssetC2idACSi_tcfc", scope: !63, file: !19, line: 29, type: !72, scopeLine: 29, spFlags: 0)
!75 = !{!76, !78}
!76 = !DILocalVariable(name: "id", arg: 1, scope: !71, file: !19, line: 29, type: !77)
!77 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !66)
!78 = !DILocalVariable(name: "self", arg: 2, scope: !71, file: !19, line: 29, type: !79, flags: DIFlagArtificial)
!79 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !63)
!80 = !DILocation(line: 29, column: 8, scope: !71)
!81 = !DILocation(line: 29, column: 3, scope: !71)
!82 = !DILocation(line: 0, scope: !71)
!83 = !DILocation(line: 29, column: 27, scope: !71)
!84 = !DILocation(line: 29, column: 32, scope: !71)
!85 = distinct !DISubprogram(name: "init", linkageName: "$s18StoredAsyncClosure8SACAliasC7assetIdACSi_tcfC", scope: !86, file: !19, line: 34, type: !87, scopeLine: 34, spFlags: DISPFlagDefinition, unit: !18, declaration: !90)
!86 = !DICompositeType(tag: DW_TAG_structure_type, name: "SACAlias", scope: !22, file: !19, size: 64, runtimeLang: DW_LANG_Swift, identifier: "$s18StoredAsyncClosure8SACAliasCD")
!87 = !DISubroutineType(types: !88)
!88 = !{!86, !66, !89}
!89 = !DICompositeType(tag: DW_TAG_structure_type, name: "$s18StoredAsyncClosure8SACAliasCXMTD", flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$s18StoredAsyncClosure8SACAliasCXMTD")
!90 = !DISubprogram(name: "init", linkageName: "$s18StoredAsyncClosure8SACAliasC7assetIdACSi_tcfC", scope: !86, file: !19, line: 34, type: !87, scopeLine: 34, spFlags: 0)
!91 = !DILocation(line: 0, scope: !85)
!92 = distinct !DISubprogram(name: "init", linkageName: "$s18StoredAsyncClosure8SACAliasC7assetIdACSi_tcfc", scope: !86, file: !19, line: 34, type: !93, scopeLine: 34, spFlags: DISPFlagDefinition, unit: !18, declaration: !95, retainedNodes: !96)
!93 = !DISubroutineType(types: !94)
!94 = !{!86, !66, !86}
!95 = !DISubprogram(name: "init", linkageName: "$s18StoredAsyncClosure8SACAliasC7assetIdACSi_tcfc", scope: !86, file: !19, line: 34, type: !93, scopeLine: 34, spFlags: 0)
!96 = !{!97, !98}
!97 = !DILocalVariable(name: "assetId", arg: 1, scope: !92, file: !19, line: 34, type: !77)
!98 = !DILocalVariable(name: "self", arg: 2, scope: !92, file: !19, line: 34, type: !99, flags: DIFlagArtificial)
!99 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !86)
!100 = !DILocation(line: 34, column: 8, scope: !92)
!101 = !DILocation(line: 34, column: 3, scope: !92)
!102 = !DILocation(line: 0, scope: !92)
!103 = !DILocation(line: 34, column: 37, scope: !92)
!104 = !DILocation(line: 34, column: 47, scope: !92)
!105 = distinct !DISubprogram(linkageName: "$s18StoredAsyncClosure8SACAssetCAA8SACAliasCIeghHgg_SgWOy", scope: !22, file: !106, type: !107, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !18)
!106 = !DIFile(filename: "<compiler-generated>", directory: "")
!107 = !DISubroutineType(types: null)
!108 = !DILocation(line: 0, scope: !105)
!109 = distinct !DISubprogram(name: "clean_no_cycle_good_FP", linkageName: "$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF", scope: !110, file: !19, line: 40, type: !111, scopeLine: 40, spFlags: DISPFlagDefinition, unit: !18, declaration: !113, retainedNodes: !116, thrownTypes: !114)
!110 = !DICompositeType(tag: DW_TAG_structure_type, name: "SACImportService", scope: !22, file: !19, size: 64, runtimeLang: DW_LANG_Swift, identifier: "$s18StoredAsyncClosure16SACImportServiceCD")
!111 = !DISubroutineType(types: !112)
!112 = !{!66, !110}
!113 = !DISubprogram(name: "clean_no_cycle_good_FP", linkageName: "$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKF", scope: !110, file: !19, line: 40, type: !111, scopeLine: 40, spFlags: 0, thrownTypes: !114)
!114 = !{!115}
!115 = !DICompositeType(tag: DW_TAG_structure_type, name: "Error", scope: !24, file: !67, size: 64, runtimeLang: DW_LANG_Swift, identifier: "$ss5Error_pD")
!116 = !{!117, !119, !121, !126, !129}
!117 = !DILocalVariable(name: "self", arg: 1, scope: !109, file: !19, line: 40, type: !118, flags: DIFlagArtificial)
!118 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !110)
!119 = !DILocalVariable(name: "cleaned", scope: !120, file: !19, line: 41, type: !66)
!120 = distinct !DILexicalBlock(scope: !109, file: !19, line: 41, column: 9)
!121 = !DILocalVariable(name: "callback", scope: !122, file: !19, line: 42, type: !124)
!122 = distinct !DILexicalBlock(scope: !123, file: !19, line: 42, column: 23)
!123 = distinct !DILexicalBlock(scope: !120, file: !19, line: 42, column: 5)
!124 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !125)
!125 = !DICompositeType(tag: DW_TAG_structure_type, name: "$s18StoredAsyncClosure8SACAssetCAA8SACAliasCIeghHgg_D", file: !19, size: 128, runtimeLang: DW_LANG_Swift, identifier: "$s18StoredAsyncClosure8SACAssetCAA8SACAliasCIeghHgg_D")
!126 = !DILocalVariable(name: "asset", scope: !127, file: !19, line: 43, type: !79)
!127 = distinct !DILexicalBlock(scope: !128, file: !19, line: 43, column: 11)
!128 = distinct !DILexicalBlock(scope: !122, file: !19, line: 42, column: 49)
!129 = !DILocalVariable(name: "alias", scope: !130, file: !19, line: 44, type: !99)
!130 = distinct !DILexicalBlock(scope: !127, file: !19, line: 44, column: 11)
!131 = !DILocation(line: 44, column: 11, scope: !130)
!132 = !DILocation(line: 43, column: 11, scope: !127)
!133 = !DILocation(line: 42, column: 12, scope: !122)
!134 = !DILocation(line: 41, column: 9, scope: !120)
!135 = !DILocation(line: 40, column: 8, scope: !109)
!136 = !DILocation(line: 0, scope: !109)
!137 = !DILocation(line: 0, scope: !138)
!138 = !DILexicalBlockFile(scope: !109, discriminator: 0)
!139 = distinct !DISubprogram(name: "clean_no_cycle_good_FP", linkageName: "$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKFTY0_", scope: !110, file: !19, line: 40, type: !111, scopeLine: 41, spFlags: DISPFlagDefinition, unit: !18, declaration: !140, retainedNodes: !141, thrownTypes: !114)
!140 = !DISubprogram(name: "clean_no_cycle_good_FP", linkageName: "$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKFTY0_", scope: !110, file: !19, line: 40, type: !111, scopeLine: 40, spFlags: 0, thrownTypes: !114)
!141 = !{!142, !143, !145, !148, !151}
!142 = !DILocalVariable(name: "self", arg: 1, scope: !139, file: !19, line: 40, type: !118, flags: DIFlagArtificial)
!143 = !DILocalVariable(name: "cleaned", scope: !144, file: !19, line: 41, type: !66)
!144 = distinct !DILexicalBlock(scope: !139, file: !19, line: 41, column: 9)
!145 = !DILocalVariable(name: "callback", scope: !146, file: !19, line: 42, type: !124)
!146 = distinct !DILexicalBlock(scope: !147, file: !19, line: 42, column: 23)
!147 = distinct !DILexicalBlock(scope: !144, file: !19, line: 42, column: 5)
!148 = !DILocalVariable(name: "asset", scope: !149, file: !19, line: 43, type: !79)
!149 = distinct !DILexicalBlock(scope: !150, file: !19, line: 43, column: 11)
!150 = distinct !DILexicalBlock(scope: !146, file: !19, line: 42, column: 49)
!151 = !DILocalVariable(name: "alias", scope: !152, file: !19, line: 44, type: !99)
!152 = distinct !DILexicalBlock(scope: !149, file: !19, line: 44, column: 11)
!153 = !DILocation(line: 44, column: 11, scope: !152)
!154 = !DILocation(line: 43, column: 11, scope: !149)
!155 = !DILocation(line: 42, column: 12, scope: !146)
!156 = !DILocation(line: 41, column: 9, scope: !144)
!157 = !DILocation(line: 40, column: 8, scope: !139)
!158 = !DILocation(line: 41, column: 19, scope: !159)
!159 = distinct !DILexicalBlock(scope: !139, file: !19, line: 41, column: 19)
!160 = !DILocation(line: 42, column: 5, scope: !147)
!161 = !DILocation(line: 0, scope: !146)
!162 = !DILocation(line: 43, column: 19, scope: !163)
!163 = distinct !DILexicalBlock(scope: !150, file: !19, line: 43, column: 19)
!164 = !DILocation(line: 0, scope: !149)
!165 = !DILocation(line: 0, scope: !166)
!166 = !DILexicalBlockFile(scope: !149, discriminator: 0)
!167 = !DILocation(line: 44, column: 19, scope: !168)
!168 = distinct !DILexicalBlock(scope: !149, file: !19, line: 44, column: 19)
!169 = !DILocation(line: 0, scope: !152)
!170 = !DILocation(line: 0, scope: !171)
!171 = !DILexicalBlockFile(scope: !152, discriminator: 0)
!172 = !DILocation(line: 45, column: 13, scope: !152)
!173 = !DILocation(line: 48, column: 5, scope: !144)
!174 = distinct !DISubprogram(name: "clean_no_cycle_good_FP", linkageName: "$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKFTQ1_", scope: !110, file: !19, line: 40, type: !111, scopeLine: 42, spFlags: DISPFlagDefinition, unit: !18, declaration: !175, retainedNodes: !176, thrownTypes: !114)
!175 = !DISubprogram(name: "clean_no_cycle_good_FP", linkageName: "$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKFTQ1_", scope: !110, file: !19, line: 40, type: !111, scopeLine: 40, spFlags: 0, thrownTypes: !114)
!176 = !{!177, !178, !180, !183, !186}
!177 = !DILocalVariable(name: "self", arg: 1, scope: !174, file: !19, line: 40, type: !118, flags: DIFlagArtificial)
!178 = !DILocalVariable(name: "cleaned", scope: !179, file: !19, line: 41, type: !66)
!179 = distinct !DILexicalBlock(scope: !174, file: !19, line: 41, column: 9)
!180 = !DILocalVariable(name: "callback", scope: !181, file: !19, line: 42, type: !124)
!181 = distinct !DILexicalBlock(scope: !182, file: !19, line: 42, column: 23)
!182 = distinct !DILexicalBlock(scope: !179, file: !19, line: 42, column: 5)
!183 = !DILocalVariable(name: "asset", scope: !184, file: !19, line: 43, type: !79)
!184 = distinct !DILexicalBlock(scope: !185, file: !19, line: 43, column: 11)
!185 = distinct !DILexicalBlock(scope: !181, file: !19, line: 42, column: 49)
!186 = !DILocalVariable(name: "alias", scope: !187, file: !19, line: 44, type: !99)
!187 = distinct !DILexicalBlock(scope: !184, file: !19, line: 44, column: 11)
!188 = !DILocation(line: 44, column: 11, scope: !187)
!189 = !DILocation(line: 43, column: 11, scope: !184)
!190 = !DILocation(line: 42, column: 12, scope: !181)
!191 = !DILocation(line: 41, column: 9, scope: !179)
!192 = !DILocation(line: 40, column: 8, scope: !174)
!193 = !DILocation(line: 45, column: 13, scope: !187)
!194 = !DILocation(line: 42, column: 5, scope: !182)
!195 = distinct !DISubprogram(name: "clean_no_cycle_good_FP", linkageName: "$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKFTY2_", scope: !110, file: !19, line: 40, type: !111, scopeLine: 42, spFlags: DISPFlagDefinition, unit: !18, declaration: !196, retainedNodes: !197, thrownTypes: !114)
!196 = !DISubprogram(name: "clean_no_cycle_good_FP", linkageName: "$s18StoredAsyncClosure16SACImportServiceC22clean_no_cycle_good_FPSiyYaKFTY2_", scope: !110, file: !19, line: 40, type: !111, scopeLine: 40, spFlags: 0, thrownTypes: !114)
!197 = !{!198, !199, !201, !204, !207}
!198 = !DILocalVariable(name: "self", arg: 1, scope: !195, file: !19, line: 40, type: !118, flags: DIFlagArtificial)
!199 = !DILocalVariable(name: "cleaned", scope: !200, file: !19, line: 41, type: !66)
!200 = distinct !DILexicalBlock(scope: !195, file: !19, line: 41, column: 9)
!201 = !DILocalVariable(name: "callback", scope: !202, file: !19, line: 42, type: !124)
!202 = distinct !DILexicalBlock(scope: !203, file: !19, line: 42, column: 23)
!203 = distinct !DILexicalBlock(scope: !200, file: !19, line: 42, column: 5)
!204 = !DILocalVariable(name: "asset", scope: !205, file: !19, line: 43, type: !79)
!205 = distinct !DILexicalBlock(scope: !206, file: !19, line: 43, column: 11)
!206 = distinct !DILexicalBlock(scope: !202, file: !19, line: 42, column: 49)
!207 = !DILocalVariable(name: "alias", scope: !208, file: !19, line: 44, type: !99)
!208 = distinct !DILexicalBlock(scope: !205, file: !19, line: 44, column: 11)
!209 = !DILocation(line: 44, column: 11, scope: !208)
!210 = !DILocation(line: 43, column: 11, scope: !205)
!211 = !DILocation(line: 42, column: 12, scope: !202)
!212 = !DILocation(line: 41, column: 9, scope: !200)
!213 = !DILocation(line: 40, column: 8, scope: !195)
!214 = !DILocation(line: 42, column: 5, scope: !203)
!215 = !DILocation(line: 46, column: 15, scope: !208)
!216 = !DILocation(line: 47, column: 5, scope: !208)
!217 = !DILocation(line: 48, column: 5, scope: !200)
!218 = distinct !DISubprogram(linkageName: "$s18StoredAsyncClosure8SACAssetCMa", scope: !22, file: !106, type: !107, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !18)
!219 = !DILocation(line: 0, scope: !218)
!220 = distinct !DISubprogram(linkageName: "$s18StoredAsyncClosure8SACAliasCMa", scope: !22, file: !106, type: !107, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !18)
!221 = !DILocation(line: 0, scope: !220)
