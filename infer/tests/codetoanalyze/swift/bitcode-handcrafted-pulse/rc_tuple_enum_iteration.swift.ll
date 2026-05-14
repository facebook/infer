; LLVM IR captured from a buck2 build of the source-level witness at
; fbobjc/Tools/infer/tests/codetoanalyze/swift/pulse/TupleEnumIteration/RcTupleEnumIteration.swift.
;
; Source:
;
;     enum TEISyncStatus {
;         case ...
;         case uploading(progress: Double)
;         case syncError(Error)            // class-bound payload
;     }
;     for syncItem in [(projectId: String, status: TEISyncStatus)] {
;         switch syncItem.status { ... }
;     }
;
; A `for` loop over an array of named tuples whose tuple element is a
; payload-bearing enum (with at least one class-bound payload case).
; Pre-D104086113 Pulse reports a fictional cycle on `syncItem->__infer_
; tuple_field_1->field_1` — `__infer_tuple_field_1` is the `status`
; element, `field_1` is a fictional sub-field of the enum slot. Same root
; pattern as the inline payload-bearing enum FP fixed by D104086113, but
; on a tuple element rather than a struct field.
;
; Regression test: D104086113's inline-enum gate in
; [Llair2TextualType.to_textual_field_decls] is parameterised by [~tuple]
; but the gate condition itself is shape-only (it triggers on any field
; whose Llair type is [Struct {name; elts}] with [is_enum_mangled_name
; name && IArray.length elts > 1]), so it covers the tuple-element case
; for free. This test pins that coverage so a future change to that gate
; can't silently regress only the tuple branch.

; ModuleID = '/tmp/tei_baseline.ll'
source_filename = "TupleEnumIteration.o"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-ios15.1.0-simulator"

%swift.protocol_conformance_descriptor = type { i32, i32, i32, i32 }
%Ts16IndexingIteratorVySaySS9projectId_18TupleEnumIteration13TEISyncStatusO6statustGG = type <{ %TSa, %TSi }>
%TSa = type <{ %Ts12_ArrayBufferV }>
%Ts12_ArrayBufferV = type <{ %Ts14_BridgeStorageV }>
%Ts14_BridgeStorageV = type <{ ptr }>
%TSi = type <{ i64 }>
%TSS9projectId_18TupleEnumIteration13TEISyncStatusO6statustSg = type <{ [25 x i8] }>
%TSS = type <{ %Ts11_StringGutsV }>
%Ts11_StringGutsV = type <{ %Ts13_StringObjectV }>
%Ts13_StringObjectV = type <{ %Ts6UInt64V, ptr }>
%Ts6UInt64V = type <{ i64 }>
%T18TupleEnumIteration13TEISyncStatusO = type <{ [8 x i8], [1 x i8] }>
%swift.opaque = type opaque

@"$sSaySS9projectId_18TupleEnumIteration13TEISyncStatusO6statustGMD" = external hidden global { i32, i32 }, align 8
@"$sSaySS9projectId_18TupleEnumIteration13TEISyncStatusO6statustGSayxGSlsWL" = external hidden global ptr, align 8
@"$sSayxGSlsMc" = external global %swift.protocol_conformance_descriptor, align 4
@"$ss16IndexingIteratorVySaySS9projectId_18TupleEnumIteration13TEISyncStatusO6statustGGMD" = external hidden global { i32, i32 }, align 8

define hidden swiftcc void @"$s18TupleEnumIteration13TEIControllerC24iterate_no_cycle_good_FP11activeSyncsySaySS9projectId_AA13TEISyncStatusO6statustG_tF"(ptr %0, ptr swiftself %1) #0 !dbg !62 {
  %3 = alloca ptr, align 8
    #dbg_declare(ptr %3, !70, !DIExpression(), !91)
  call void @llvm.memset.p0.i64(ptr align 8 %3, i8 0, i64 8, i1 false)
  %4 = alloca ptr, align 8
    #dbg_declare(ptr %4, !80, !DIExpression(), !92)
  call void @llvm.memset.p0.i64(ptr align 8 %4, i8 0, i64 8, i1 false)
  %5 = alloca %Ts16IndexingIteratorVySaySS9projectId_18TupleEnumIteration13TEISyncStatusO6statustGG, align 8
  call void @llvm.memset.p0.i64(ptr align 8 %5, i8 0, i64 16, i1 false)
  %6 = alloca %TSa, align 8
  %7 = alloca %TSS9projectId_18TupleEnumIteration13TEISyncStatusO6statustSg, align 8
  %8 = alloca <{ %TSS, %T18TupleEnumIteration13TEISyncStatusO }>, align 8
    #dbg_declare(ptr %8, !82, !DIExpression(), !93)
  call void @llvm.memset.p0.i64(ptr align 8 %8, i8 0, i64 25, i1 false)
  %9 = alloca double, align 8
    #dbg_declare(ptr %9, !86, !DIExpression(), !94)
  call void @llvm.memset.p0.i64(ptr align 8 %9, i8 0, i64 8, i1 false)
  store ptr %0, ptr %3, align 8, !dbg !95
  store ptr %1, ptr %4, align 8, !dbg !95
  call void @llvm.lifetime.start.p0(i64 16, ptr %5), !dbg !96
  %10 = call ptr @swift_bridgeObjectRetain(ptr returned %0) #3, !dbg !98
  call void @llvm.lifetime.start.p0(i64 8, ptr %6), !dbg !96
  %11 = getelementptr inbounds nuw %TSa, ptr %6, i32 0, i32 0, !dbg !98
  %12 = getelementptr inbounds nuw %Ts12_ArrayBufferV, ptr %11, i32 0, i32 0, !dbg !98
  %13 = getelementptr inbounds nuw %Ts14_BridgeStorageV, ptr %12, i32 0, i32 0, !dbg !98
  store ptr %0, ptr %13, align 8, !dbg !98
  %14 = call ptr @__swift_instantiateConcreteTypeFromMangledName(ptr @"$sSaySS9projectId_18TupleEnumIteration13TEISyncStatusO6statustGMD") #8, !dbg !99
  %15 = call ptr @"$sSaySS9projectId_18TupleEnumIteration13TEISyncStatusO6statustGSayxGSlsWl"() #10, !dbg !99
  call swiftcc void @"$sSlss16IndexingIteratorVyxG0B0RtzrlE04makeB0ACyF"(ptr noalias sret(%swift.opaque) %5, ptr %14, ptr %15, ptr noalias swiftself %6), !dbg !99
  call void @llvm.lifetime.end.p0(i64 8, ptr %6), !dbg !100
  br label %16, !dbg !100

16:                                               ; preds = %57, %2
  call void @llvm.lifetime.start.p0(i64 25, ptr %7), !dbg !96
  %17 = call ptr @__swift_instantiateConcreteTypeFromMangledName(ptr @"$ss16IndexingIteratorVySaySS9projectId_18TupleEnumIteration13TEISyncStatusO6statustGGMD") #8, !dbg !99
  call swiftcc void @"$ss16IndexingIteratorV4next7ElementQzSgyF"(ptr noalias sret(%swift.opaque) %7, ptr %17, ptr swiftself %5), !dbg !99
  %18 = getelementptr inbounds nuw { i64, i64, i64, i8 }, ptr %7, i32 0, i32 0, !dbg !99
  %19 = load i64, ptr %18, align 8, !dbg !99
  %20 = getelementptr inbounds nuw { i64, i64, i64, i8 }, ptr %7, i32 0, i32 1, !dbg !99
  %21 = load i64, ptr %20, align 8, !dbg !99
  %22 = getelementptr inbounds nuw { i64, i64, i64, i8 }, ptr %7, i32 0, i32 2, !dbg !99
  %23 = load i64, ptr %22, align 8, !dbg !99
  %24 = getelementptr inbounds nuw { i64, i64, i64, i8 }, ptr %7, i32 0, i32 3, !dbg !99
  %25 = load i8, ptr %24, align 8, !dbg !99
  call void @llvm.lifetime.end.p0(i64 25, ptr %7), !dbg !100
  %26 = icmp eq i64 %21, 0, !dbg !100
  br i1 %26, label %59, label %27, !dbg !100

27:                                               ; preds = %16
  %28 = inttoptr i64 %21 to ptr, !dbg !100
  br label %29, !dbg !100

29:                                               ; preds = %27
  %30 = phi i64 [ %19, %27 ], !dbg !101
  %31 = phi ptr [ %28, %27 ], !dbg !101
  %32 = phi i64 [ %23, %27 ], !dbg !101
  %33 = phi i8 [ %25, %27 ], !dbg !101
  call void @llvm.lifetime.start.p0(i64 25, ptr %8), !dbg !101
  %34 = getelementptr inbounds nuw <{ %TSS, %T18TupleEnumIteration13TEISyncStatusO }>, ptr %8, i32 0, i32 0, !dbg !103
  %35 = getelementptr inbounds nuw %TSS, ptr %34, i32 0, i32 0, !dbg !103
  %36 = getelementptr inbounds nuw %Ts11_StringGutsV, ptr %35, i32 0, i32 0, !dbg !103
  %37 = getelementptr inbounds nuw %Ts13_StringObjectV, ptr %36, i32 0, i32 0, !dbg !103
  %38 = getelementptr inbounds nuw %Ts6UInt64V, ptr %37, i32 0, i32 0, !dbg !103
  store i64 %30, ptr %38, align 8, !dbg !103
  %39 = getelementptr inbounds nuw %Ts13_StringObjectV, ptr %36, i32 0, i32 1, !dbg !103
  store ptr %31, ptr %39, align 8, !dbg !103
  %40 = getelementptr inbounds nuw <{ %TSS, %T18TupleEnumIteration13TEISyncStatusO }>, ptr %8, i32 0, i32 1, !dbg !103
  store i64 %32, ptr %40, align 8, !dbg !103
  %41 = getelementptr inbounds nuw %T18TupleEnumIteration13TEISyncStatusO, ptr %40, i32 0, i32 1, !dbg !103
  store i8 %33, ptr %41, align 8, !dbg !103
  switch i8 %33, label %43 [
    i8 0, label %44
    i8 1, label %46
    i8 2, label %42
  ], !dbg !104

42:                                               ; preds = %29
  switch i64 %32, label %43 [
    i64 0, label %51
    i64 1, label %52
    i64 2, label %50
    i64 3, label %53
  ], !dbg !104

43:                                               ; preds = %42, %29
  unreachable, !dbg !104

44:                                               ; preds = %29
  %45 = bitcast i64 %32 to double, !dbg !104
  br label %48, !dbg !104

46:                                               ; preds = %29
  %47 = inttoptr i64 %32 to ptr, !dbg !104
  br label %54, !dbg !104

48:                                               ; preds = %44
  %49 = phi double [ %45, %44 ], !dbg !105
  store double %49, ptr %9, align 8, !dbg !106
  br label %58, !dbg !107

50:                                               ; preds = %42
  br label %58, !dbg !109

51:                                               ; preds = %42
  br label %56, !dbg !112

52:                                               ; preds = %42
  br label %56, !dbg !112

53:                                               ; preds = %42
  br label %56, !dbg !112

54:                                               ; preds = %46
  %55 = phi ptr [ %47, %46 ], !dbg !112
  br label %56, !dbg !112

56:                                               ; preds = %54, %53, %52, %51
  call void @swift_bridgeObjectRelease(ptr %31) #3, !dbg !114
  call void @"$s18TupleEnumIteration13TEISyncStatusOWOe"(i64 %32, i8 %33), !dbg !114
  br label %57, !dbg !114

57:                                               ; preds = %58, %56
  br label %16, !dbg !96

58:                                               ; preds = %50, %48
  call void @swift_bridgeObjectRelease(ptr %31) #3, !dbg !116
  call void @"$s18TupleEnumIteration13TEISyncStatusOWOe"(i64 %32, i8 %33), !dbg !116
  br label %57, !dbg !116

59:                                               ; preds = %16
  %60 = call ptr @"$ss16IndexingIteratorVySaySS9projectId_18TupleEnumIteration13TEISyncStatusO6statustGGWOh"(ptr %5), !dbg !117
  call void @llvm.lifetime.end.p0(i64 16, ptr %5), !dbg !117
  ret void, !dbg !117
}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: write)
declare void @llvm.memset.p0.i64(ptr writeonly captures(none), i8, i64, i1 immarg) #1

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.start.p0(i64 immarg, ptr captures(none)) #2

; Function Attrs: nounwind
declare ptr @swift_bridgeObjectRetain(ptr returned) #3

declare swiftcc void @"$sSlss16IndexingIteratorVyxG0B0RtzrlE04makeB0ACyF"(ptr noalias sret(%swift.opaque), ptr, ptr, ptr noalias swiftself) #0

; Function Attrs: noinline nounwind willreturn memory(read)
define weak_odr hidden ptr @__swift_instantiateConcreteTypeFromMangledName(ptr %0) #4 !dbg !118 {
  %2 = load atomic i64, ptr %0 monotonic, align 8, !dbg !121
  %3 = icmp slt i64 %2, 0, !dbg !121
  %4 = call i1 @llvm.expect.i1(i1 %3, i1 false), !dbg !121
  br i1 %4, label %8, label %5, !dbg !121

5:                                                ; preds = %8, %1
  %6 = phi i64 [ %2, %1 ], [ %17, %8 ], !dbg !121
  %7 = inttoptr i64 %6 to ptr, !dbg !121
  ret ptr %7, !dbg !121

8:                                                ; preds = %1
  %9 = ashr i64 %2, 32, !dbg !121
  %10 = sub i64 0, %9, !dbg !121
  %11 = trunc i64 %2 to i32, !dbg !121
  %12 = sext i32 %11 to i64, !dbg !121
  %13 = ptrtoint ptr %0 to i64, !dbg !121
  %14 = add i64 %13, %12, !dbg !121
  %15 = inttoptr i64 %14 to ptr, !dbg !121
  %16 = call swiftcc ptr @swift_getTypeByMangledNameInContext(ptr %15, i64 %10, ptr null, ptr null) #11, !dbg !121
  %17 = ptrtoint ptr %16 to i64, !dbg !121
  store atomic i64 %17, ptr %0 monotonic, align 8, !dbg !121
  br label %5, !dbg !121
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(none)
declare i1 @llvm.expect.i1(i1, i1) #5

; Function Attrs: nounwind memory(argmem: readwrite)
declare swiftcc ptr @swift_getTypeByMangledNameInContext(ptr, i64, ptr, ptr) #6

; Function Attrs: noinline nounwind memory(none)
define weak_odr hidden ptr @"$sSaySS9projectId_18TupleEnumIteration13TEISyncStatusO6statustGSayxGSlsWl"() #7 !dbg !122 {
  %1 = load ptr, ptr @"$sSaySS9projectId_18TupleEnumIteration13TEISyncStatusO6statustGSayxGSlsWL", align 8, !dbg !123
  %2 = icmp eq ptr %1, null, !dbg !123
  br i1 %2, label %3, label %6, !dbg !123

3:                                                ; preds = %0
  %4 = call ptr @__swift_instantiateConcreteTypeFromMangledNameAbstract(ptr @"$sSaySS9projectId_18TupleEnumIteration13TEISyncStatusO6statustGMD") #8, !dbg !123
  %5 = call ptr @swift_getWitnessTable(ptr @"$sSayxGSlsMc", ptr %4, ptr undef) #8, !dbg !123
  store atomic ptr %5, ptr @"$sSaySS9projectId_18TupleEnumIteration13TEISyncStatusO6statustGSayxGSlsWL" release, align 8, !dbg !123
  br label %6, !dbg !123

6:                                                ; preds = %3, %0
  %7 = phi ptr [ %1, %0 ], [ %5, %3 ], !dbg !123
  ret ptr %7, !dbg !123
}

; Function Attrs: noinline nounwind willreturn memory(read)
define weak_odr hidden ptr @__swift_instantiateConcreteTypeFromMangledNameAbstract(ptr %0) #4 !dbg !124 {
  %2 = load atomic i64, ptr %0 monotonic, align 8, !dbg !125
  %3 = icmp slt i64 %2, 0, !dbg !125
  %4 = call i1 @llvm.expect.i1(i1 %3, i1 false), !dbg !125
  br i1 %4, label %8, label %5, !dbg !125

5:                                                ; preds = %8, %1
  %6 = phi i64 [ %2, %1 ], [ %17, %8 ], !dbg !125
  %7 = inttoptr i64 %6 to ptr, !dbg !125
  ret ptr %7, !dbg !125

8:                                                ; preds = %1
  %9 = ashr i64 %2, 32, !dbg !125
  %10 = sub i64 0, %9, !dbg !125
  %11 = trunc i64 %2 to i32, !dbg !125
  %12 = sext i32 %11 to i64, !dbg !125
  %13 = ptrtoint ptr %0 to i64, !dbg !125
  %14 = add i64 %13, %12, !dbg !125
  %15 = inttoptr i64 %14 to ptr, !dbg !125
  %16 = call swiftcc ptr @swift_getTypeByMangledNameInContextInMetadataState(i64 255, ptr %15, i64 %10, ptr null, ptr null) #11, !dbg !125
  %17 = ptrtoint ptr %16 to i64, !dbg !125
  store atomic i64 %17, ptr %0 monotonic, align 8, !dbg !125
  br label %5, !dbg !125
}

; Function Attrs: nounwind memory(argmem: readwrite)
declare swiftcc ptr @swift_getTypeByMangledNameInContextInMetadataState(i64, ptr, i64, ptr, ptr) #6

; Function Attrs: nounwind memory(read)
declare ptr @swift_getWitnessTable(ptr, ptr, ptr) #8

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.end.p0(i64 immarg, ptr captures(none)) #2

declare swiftcc void @"$ss16IndexingIteratorV4next7ElementQzSgyF"(ptr noalias sret(%swift.opaque), ptr, ptr swiftself) #0

; Function Attrs: noinline nounwind
define weak_odr hidden ptr @"$ss16IndexingIteratorVySaySS9projectId_18TupleEnumIteration13TEISyncStatusO6statustGGWOh"(ptr %0) #9 !dbg !126 {
  %2 = getelementptr inbounds nuw %Ts16IndexingIteratorVySaySS9projectId_18TupleEnumIteration13TEISyncStatusO6statustGG, ptr %0, i32 0, i32 0, !dbg !127
  %3 = getelementptr inbounds nuw %TSa, ptr %2, i32 0, i32 0, !dbg !127
  %4 = getelementptr inbounds nuw %Ts12_ArrayBufferV, ptr %3, i32 0, i32 0, !dbg !127
  %5 = getelementptr inbounds nuw %Ts14_BridgeStorageV, ptr %4, i32 0, i32 0, !dbg !127
  %6 = load ptr, ptr %5, align 8, !dbg !127
  call void @swift_bridgeObjectRelease(ptr %6) #3, !dbg !127
  ret ptr %0, !dbg !127
}

; Function Attrs: nounwind
declare void @swift_bridgeObjectRelease(ptr) #3

; Function Attrs: noinline nounwind
define weak_odr hidden void @"$s18TupleEnumIteration13TEISyncStatusOWOe"(i64 %0, i8 %1) #9 !dbg !128 {
  %3 = icmp ne i8 %1, 1
  br i1 %3, label %6, label %4

4:                                                ; preds = %2
  %5 = inttoptr i64 %0 to ptr, !dbg !129
  call void @swift_errorRelease(ptr %5) #3, !dbg !129
  br label %6, !dbg !129

6:                                                ; preds = %4, %2
  ret void, !dbg !129
}

; Function Attrs: nounwind
declare void @swift_errorRelease(ptr) #3

attributes #0 = { "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" }
attributes #1 = { nocallback nofree nounwind willreturn memory(argmem: write) }
attributes #2 = { nocallback nofree nosync nounwind willreturn memory(argmem: readwrite) }
attributes #3 = { nounwind }
attributes #4 = { noinline nounwind willreturn memory(read) "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" }
attributes #5 = { nocallback nofree nosync nounwind willreturn memory(none) }
attributes #6 = { nounwind memory(argmem: readwrite) }
attributes #7 = { noinline nounwind memory(none) "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" }
attributes #8 = { nounwind memory(read) }
attributes #9 = { noinline nounwind "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" }
attributes #10 = { nounwind memory(none) }
attributes #11 = { nounwind memory(argmem: read) }

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
!18 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !19, producer: "Facebook Swift version 6.3.2-dev effective-5.10 (LLVM 0c6fa689b924b2d, Swift 999e243c7e9b3d1)", isOptimized: false, runtimeVersion: 6, emissionKind: FullDebug, imports: !20, sysroot: "buck-out/v2/art/fbsource/xplat/toolchains/apple/__xcode_26.2.0_17c52-iphonesimulator-sdk_genrule-actual__/e5e3cbbf737d97c8/iPhoneSimulator26.2.sdk", sdk: "iPhoneSimulator26.2.sdk")
!19 = !DIFile(filename: "fbobjc/Tools/infer/tests/codetoanalyze/swift/pulse/TupleEnumIteration/RcTupleEnumIteration.swift", directory: ".")
!20 = !{!21, !23, !25, !27, !29, !31, !33, !35}
!21 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !22, file: !19)
!22 = !DIModule(scope: null, name: "TupleEnumIteration", includePath: "fbobjc/Tools/infer/tests/codetoanalyze/swift/pulse/TupleEnumIteration")
!23 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !24, file: !19)
!24 = !DIModule(scope: null, name: "Swift", configMacros: "\22-DDEBUG=1\22 \22-DFB_MARK_AUTH_INTERNAL=1\22 \22-DENABLE_NON_PRODUCTION_TOOLS=1\22 \22-DRCT_DEV_MENU=1\22 \22-DENABLE_LOGGING=1\22 \22-DRCTLOG_ENABLED=1\22 \22-DFB_SONARKIT_ENABLED=1\22", includePath: "buck-out/v2/art/fbsource/xplat/toolchains/apple/__xcode_26.2.0_17c52-iphonesimulator-sdk_genrule-actual__/e5e3cbbf737d97c8/iPhoneSimulator26.2.sdk/usr/lib/swift/Swift.swiftmodule/arm64-apple-ios-simulator.swiftinterface")
!25 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !26, file: !19)
!26 = !DIModule(scope: null, name: "Foundation", configMacros: "\22-DDEBUG=1\22 \22-DFB_MARK_AUTH_INTERNAL=1\22 \22-DENABLE_NON_PRODUCTION_TOOLS=1\22 \22-DRCT_DEV_MENU=1\22 \22-DENABLE_LOGGING=1\22 \22-DRCTLOG_ENABLED=1\22 \22-DFB_SONARKIT_ENABLED=1\22", includePath: ".")
!27 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !28, file: !19, line: 24)
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
!62 = distinct !DISubprogram(name: "iterate_no_cycle_good_FP", linkageName: "$s18TupleEnumIteration13TEIControllerC24iterate_no_cycle_good_FP11activeSyncsySaySS9projectId_AA13TEISyncStatusO6statustG_tF", scope: !63, file: !19, line: 36, type: !64, scopeLine: 36, spFlags: DISPFlagDefinition, unit: !18, declaration: !68, retainedNodes: !69)
!63 = !DICompositeType(tag: DW_TAG_structure_type, name: "TEIController", scope: !22, file: !19, size: 64, runtimeLang: DW_LANG_Swift, identifier: "$s18TupleEnumIteration13TEIControllerCD")
!64 = !DISubroutineType(types: !65)
!65 = !{!66, !67, !63}
!66 = !DICompositeType(tag: DW_TAG_structure_type, name: "$sytD", flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$sytD")
!67 = !DICompositeType(tag: DW_TAG_structure_type, name: "Array", scope: !24, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$sSaySS9projectId_18TupleEnumIteration13TEISyncStatusO6statustGD")
!68 = !DISubprogram(name: "iterate_no_cycle_good_FP", linkageName: "$s18TupleEnumIteration13TEIControllerC24iterate_no_cycle_good_FP11activeSyncsySaySS9projectId_AA13TEISyncStatusO6statustG_tF", scope: !63, file: !19, line: 36, type: !64, scopeLine: 36, spFlags: 0)
!69 = !{!70, !80, !82, !86}
!70 = !DILocalVariable(name: "activeSyncs", arg: 1, scope: !62, file: !19, line: 37, type: !71)
!71 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !72)
!72 = !DICompositeType(tag: DW_TAG_structure_type, scope: !24, file: !73, size: 64, elements: !74, runtimeLang: DW_LANG_Swift)
!73 = !DIFile(filename: "buck-out/v2/art/fbsource/xplat/toolchains/apple/__xcode_26.2.0_17c52-iphonesimulator-sdk_genrule-actual__/e5e3cbbf737d97c8/iPhoneSimulator26.2.sdk/usr/lib/swift/Swift.swiftmodule/arm64-apple-ios-simulator.swiftinterface", directory: ".")
!74 = !{!75}
!75 = !DIDerivedType(tag: DW_TAG_member, scope: !24, file: !73, baseType: !76, size: 64)
!76 = !DICompositeType(tag: DW_TAG_structure_type, name: "$sSaySS9projectId_18TupleEnumIteration13TEISyncStatusO6statustGD", scope: !24, file: !73, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, templateParams: !77)
!77 = !{!78}
!78 = !DITemplateTypeParameter(type: !79)
!79 = !DICompositeType(tag: DW_TAG_structure_type, name: "$sSS9projectId_18TupleEnumIteration13TEISyncStatusO6statustD", file: !19, size: 200, runtimeLang: DW_LANG_Swift, identifier: "$sSS9projectId_18TupleEnumIteration13TEISyncStatusO6statustD")
!80 = !DILocalVariable(name: "self", arg: 2, scope: !62, file: !19, line: 36, type: !81, flags: DIFlagArtificial)
!81 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !63)
!82 = !DILocalVariable(name: "syncItem", scope: !83, file: !19, line: 39, type: !85)
!83 = distinct !DILexicalBlock(scope: !84, file: !19, line: 39, column: 33)
!84 = distinct !DILexicalBlock(scope: !62, file: !19, line: 39, column: 5)
!85 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !79)
!86 = !DILocalVariable(name: "progress", scope: !87, file: !19, line: 41, type: !89)
!87 = distinct !DILexicalBlock(scope: !88, file: !19, line: 41, column: 7)
!88 = distinct !DILexicalBlock(scope: !83, file: !19, line: 40, column: 7)
!89 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !90)
!90 = !DICompositeType(tag: DW_TAG_structure_type, name: "Double", scope: !24, file: !73, size: 64, runtimeLang: DW_LANG_Swift, identifier: "$sSdD")
!91 = !DILocation(line: 37, column: 5, scope: !62)
!92 = !DILocation(line: 36, column: 8, scope: !62)
!93 = !DILocation(line: 39, column: 9, scope: !83)
!94 = !DILocation(line: 41, column: 27, scope: !87)
!95 = !DILocation(line: 0, scope: !62)
!96 = !DILocation(line: 0, scope: !97)
!97 = !DILexicalBlockFile(scope: !84, discriminator: 0)
!98 = !DILocation(line: 39, column: 21, scope: !84)
!99 = !DILocation(line: 0, scope: !84)
!100 = !DILocation(line: 39, column: 5, scope: !84)
!101 = !DILocation(line: 0, scope: !102)
!102 = !DILexicalBlockFile(scope: !83, discriminator: 0)
!103 = !DILocation(line: 0, scope: !83)
!104 = !DILocation(line: 41, column: 12, scope: !88)
!105 = !DILocation(line: 41, column: 22, scope: !87)
!106 = !DILocation(line: 0, scope: !87)
!107 = !DILocation(line: 0, scope: !108)
!108 = distinct !DILexicalBlock(scope: !87, file: !19, line: 42, column: 9)
!109 = !DILocation(line: 44, column: 9, scope: !110)
!110 = distinct !DILexicalBlock(scope: !111, file: !19, line: 44, column: 9)
!111 = distinct !DILexicalBlock(scope: !88, file: !19, line: 43, column: 7)
!112 = !DILocation(line: 45, column: 7, scope: !113)
!113 = distinct !DILexicalBlock(scope: !88, file: !19, line: 45, column: 7)
!114 = !DILocation(line: 46, column: 9, scope: !115)
!115 = distinct !DILexicalBlock(scope: !113, file: !19, line: 46, column: 9)
!116 = !DILocation(line: 48, column: 5, scope: !83)
!117 = !DILocation(line: 49, column: 3, scope: !84)
!118 = distinct !DISubprogram(linkageName: "__swift_instantiateConcreteTypeFromMangledName", scope: !22, file: !119, type: !120, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !18)
!119 = !DIFile(filename: "<compiler-generated>", directory: "")
!120 = !DISubroutineType(types: null)
!121 = !DILocation(line: 0, scope: !118)
!122 = distinct !DISubprogram(linkageName: "$sSaySS9projectId_18TupleEnumIteration13TEISyncStatusO6statustGSayxGSlsWl", scope: !22, file: !119, type: !120, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !18)
!123 = !DILocation(line: 0, scope: !122)
!124 = distinct !DISubprogram(linkageName: "__swift_instantiateConcreteTypeFromMangledNameAbstract", scope: !22, file: !119, type: !120, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !18)
!125 = !DILocation(line: 0, scope: !124)
!126 = distinct !DISubprogram(linkageName: "$ss16IndexingIteratorVySaySS9projectId_18TupleEnumIteration13TEISyncStatusO6statustGGWOh", scope: !22, file: !119, type: !120, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !18)
!127 = !DILocation(line: 0, scope: !126)
!128 = distinct !DISubprogram(linkageName: "$s18TupleEnumIteration13TEISyncStatusOWOe", scope: !22, file: !119, type: !120, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !18)
!129 = !DILocation(line: 0, scope: !128)
