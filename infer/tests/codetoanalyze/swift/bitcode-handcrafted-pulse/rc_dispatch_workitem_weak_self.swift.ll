; LLVM IR pinning a RETAIN_CYCLE false positive on a DispatchWorkItem
; that captures [weak self].
;
; The reported cycle is self -> pendingPersist (DispatchWorkItem?) ->
; the work item's captured closure -> self, even though the closure
; captures self *weakly*. The weak slot of the closure box is off the
; reported cycle, so Pulse closes the cycle through the strong holder
; (captured_env) path instead -- the genuine `box.weak -> self` edge
; exists in the heap but is not on the reported cycle. A later
; analysis-side fix should flip this to no-report.
;
; Repro source (schematic, reconstructed from the IR):
;
;   actor RcWeakSelfReproCls {
;     private var entries: [String: SerializedData]
;     private var isDirty: Bool
;     private let queue: DispatchQueue
;     private var isPersisting: Bool
;     private var pendingPersist: DispatchWorkItem?   // self->field_8
;     private let persistDelay: Double
;
;     private func scheduleDelayedPersist() {
;       let work = DispatchWorkItem(flags: []) { [weak self] in
;         guard let self else { return }
;         self.persistAllEntries(SerializedData(payload: [:]))
;       }
;       pendingPersist?.cancel()
;       pendingPersist = work                          // self strongly retains the work item
;       queue.asyncAfter(deadline: .now() + persistDelay, execute: work)
;     }
;
;     private func persistAllEntries(_ data: SerializedData) {
;       queue.async { /* flush `data` to disk */ }
;     }
;   }
;
;   struct SerializedData { let payload: [String: Int] }
;
; (The NULLPTR_DEREFERENCE on persistAllEntries is an incidental
; artefact of the IR reduction, not part of the modelled FP.)
;
; ModuleID = 'rc_dispatch_workitem_weak_self.ll'
source_filename = "infer/tests/codetoanalyze/swift/bitcode-handcrafted-pulse/rc_dispatch_workitem_weak_self.swift"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-ios16.3.0-simulator"

%swift.full_boxmetadata = type { ptr, ptr, %swift.type, i32, ptr }
%swift.type = type { i64 }
%swift.metadata_response = type { ptr, i64 }
%T8Dispatch0A8WorkItemCSg = type <{ [8 x i8] }>
%objc_block = type { ptr, i32, i32, ptr, ptr }
%swift.function = type { ptr, ptr }
%swift.vwtable = type { ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, i64, i32, i32 }
%T18RcWeakSelfReproClsAAC = type <{ %swift.refcounted, %swift.defaultactor, %TSD, %TSb, [7 x i8], ptr, %TSb, [7 x i8], %T8Dispatch0A8WorkItemCSg, %TSd }>
%swift.refcounted = type { ptr, i64 }
%swift.defaultactor = type { [12 x ptr] }
%TSD = type <{ %TSD8_VariantV }>
%TSD8_VariantV = type <{ %Ts14_BridgeStorageV }>
%Ts14_BridgeStorageV = type <{ ptr }>
%TSb = type <{ i1 }>
%TSd = type <{ double }>
%swift.weak = type { ptr }
%swift.opaque = type opaque
%T18RcWeakSelfReproCls010SerializedC4Data33_4ACD3E86E4EC54986DC19A1C6FD01C50LLV = type <{ %TSD }>

@metadata.5 = external constant %swift.full_boxmetadata

; Function Attrs: nounwind
declare ptr @swift_bridgeObjectRetain(ptr returned) #0

; Function Attrs: nounwind
declare ptr @swift_allocObject(ptr, i64, i64) #0

; Function Attrs: nounwind willreturn
declare ptr @swift_retain(ptr returned) #1

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.start.p0(i64 immarg, ptr captures(none)) #2

; Function Attrs: nounwind
declare void @swift_beginAccess(ptr, ptr, i64, ptr) #0

; Function Attrs: nounwind
declare void @swift_endAccess(ptr) #0

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.end.p0(i64 immarg, ptr captures(none)) #2

; Function Attrs: nounwind
declare ptr @llvm.objc.retain(ptr returned) #0

; Function Attrs: nounwind
declare void @swift_release(ptr) #0

declare swiftcc %swift.metadata_response @"$s8Dispatch0A3QoSVMa"(i64) #3

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: write)
declare void @llvm.memset.p0.i64(ptr writeonly captures(none), i8, i64, i1 immarg) #4

define swiftcc void @"$s18RcWeakSelfReproClsAAC22scheduleDelayedPersist33_4ACD3E86E4EC54986DC19A1C6FD01C50LLyyF"(ptr swiftself %0) #3 !dbg !218 {
  %2 = alloca ptr, align 8
    #dbg_declare(ptr %2, !224, !DIExpression(), !231)
  call void @llvm.memset.p0.i64(ptr align 8 %2, i8 0, i64 8, i1 false)
  %3 = alloca %T8Dispatch0A8WorkItemCSg, align 8
  %4 = alloca [24 x i8], align 8
  %5 = alloca { %objc_block, %swift.function }, align 8
  %6 = alloca ptr, align 8
    #dbg_declare(ptr %6, !225, !DIExpression(), !232)
  call void @llvm.memset.p0.i64(ptr align 8 %6, i8 0, i64 8, i1 false)
  %7 = alloca [24 x i8], align 8
  %8 = call swiftcc %swift.metadata_response @"$s8Dispatch0A4TimeVMa"(i64 0) #6, !dbg !233
  %9 = extractvalue %swift.metadata_response %8, 0, !dbg !233
  %10 = getelementptr inbounds ptr, ptr %9, i64 -1, !dbg !233
  %11 = load ptr, ptr %10, align 8, !dbg !233, !invariant.load !235, !dereferenceable !236
  %12 = getelementptr inbounds nuw %swift.vwtable, ptr %11, i32 0, i32 8, !dbg !233
  %13 = load i64, ptr %12, align 8, !dbg !233, !invariant.load !235
  %14 = alloca i8, i64 %13, align 16, !dbg !233
  call void @llvm.lifetime.start.p0(i64 -1, ptr %14), !dbg !233
  %15 = alloca i8, i64 %13, align 16, !dbg !233
  call void @llvm.lifetime.start.p0(i64 -1, ptr %15), !dbg !233
  %16 = call swiftcc %swift.metadata_response @"$s8Dispatch0A13WorkItemFlagsVMa"(i64 0) #6, !dbg !237
  %17 = extractvalue %swift.metadata_response %16, 0, !dbg !237
  %18 = getelementptr inbounds ptr, ptr %17, i64 -1, !dbg !237
  %19 = load ptr, ptr %18, align 8, !dbg !237, !invariant.load !235, !dereferenceable !236
  %20 = getelementptr inbounds nuw %swift.vwtable, ptr %19, i32 0, i32 8, !dbg !237
  %21 = load i64, ptr %20, align 8, !dbg !237, !invariant.load !235
  %22 = alloca i8, i64 %21, align 16, !dbg !237
  call void @llvm.lifetime.start.p0(i64 -1, ptr %22), !dbg !237
  store ptr %0, ptr %2, align 8, !dbg !240
  call void @llvm.lifetime.start.p0(i64 8, ptr %3), !dbg !241
  %23 = getelementptr inbounds nuw %T18RcWeakSelfReproClsAAC, ptr %0, i32 0, i32 8, !dbg !243
  call void @llvm.lifetime.start.p0(i64 -1, ptr %4), !dbg !243
  call void @swift_beginAccess(ptr %23, ptr %4, i64 32, ptr null) #0, !dbg !243
  %24 = load i64, ptr %23, align 16, !dbg !243
  %25 = inttoptr i64 %24 to ptr, !dbg !243
  %26 = call ptr @swift_retain(ptr returned %25) #1, !dbg !243
  call void @swift_endAccess(ptr %4) #0, !dbg !243
  call void @llvm.lifetime.end.p0(i64 -1, ptr %4), !dbg !243
  store i64 %24, ptr %3, align 8, !dbg !243
  %27 = load i64, ptr %3, align 8, !dbg !243
  %28 = icmp eq i64 %27, 0, !dbg !243
  %29 = xor i1 %28, true, !dbg !243
  %30 = load ptr, ptr %3, align 8, !dbg !243
  %31 = call ptr @swift_retain(ptr returned %30) #1, !dbg !243
  %32 = call ptr @"$s8Dispatch0A8WorkItemCSgWOh"(ptr %3), !dbg !244
  call swiftcc void @"$s8Dispatch0A8WorkItemC6cancelyyFTj"(ptr swiftself %30), !dbg !247
  call void @swift_release(ptr %30) #0, !dbg !244
  call void @llvm.lifetime.end.p0(i64 8, ptr %3), !dbg !244
  %33 = call swiftcc %swift.metadata_response @"$s8Dispatch0A8WorkItemCMa"(i64 0) #6, !dbg !248
  %34 = extractvalue %swift.metadata_response %33, 0, !dbg !248
  %35 = call noalias ptr @swift_allocObject(ptr getelementptr inbounds (%swift.full_boxmetadata, ptr @metadata.5, i32 0, i32 2), i64 24, i64 7) #0, !dbg !249
  %36 = getelementptr inbounds nuw <{ %swift.refcounted, %swift.weak }>, ptr %35, i32 0, i32 1, !dbg !249
  %37 = call ptr @swift_retain(ptr returned %0) #1, !dbg !252
  %38 = ptrtoint ptr %0 to i64, !dbg !253
  %39 = inttoptr i64 %38 to ptr, !dbg !253
  %40 = call ptr @swift_weakInit(ptr returned %36, ptr %39) #0, !dbg !253
  %41 = inttoptr i64 %38 to ptr, !dbg !253
  call void @swift_release(ptr %41) #0, !dbg !253
  %42 = call ptr @swift_retain(ptr returned %35) #1, !dbg !253
  call void @llvm.lifetime.start.p0(i64 48, ptr %5), !dbg !237
  %43 = getelementptr inbounds nuw { %objc_block, %swift.function }, ptr %5, i32 0, i32 1, !dbg !253
  %44 = getelementptr inbounds nuw %swift.function, ptr %43, i32 0, i32 0, !dbg !253
  store ptr null, ptr %44, align 8, !dbg !253
  %45 = getelementptr inbounds nuw %swift.function, ptr %43, i32 0, i32 1, !dbg !253
  store ptr %35, ptr %45, align 8, !dbg !253
  %46 = getelementptr inbounds nuw { %objc_block, %swift.function }, ptr %5, i32 0, i32 0, !dbg !253
  %47 = getelementptr inbounds nuw %objc_block, ptr %46, i32 0, i32 0, !dbg !253
  store ptr null, ptr %47, align 8, !dbg !253
  %48 = getelementptr inbounds nuw %objc_block, ptr %46, i32 0, i32 1, !dbg !253
  store i32 1107296256, ptr %48, align 8, !dbg !253
  %49 = getelementptr inbounds nuw %objc_block, ptr %46, i32 0, i32 2, !dbg !253
  store i32 0, ptr %49, align 4, !dbg !253
  %50 = getelementptr inbounds nuw %objc_block, ptr %46, i32 0, i32 3, !dbg !253
  store ptr null, ptr %50, align 8, !dbg !253
  %51 = getelementptr inbounds nuw %objc_block, ptr %46, i32 0, i32 4, !dbg !253
  store ptr null, ptr %51, align 8, !dbg !253
  %52 = call ptr @_Block_copy(ptr %5), !dbg !253
  call swiftcc void @"$s8Dispatch0A8WorkItemC5flags5blockAcA0abC5FlagsV_yyXBtcfcfA_"(ptr noalias sret(%swift.opaque) %22), !dbg !253
  %53 = call swiftcc ptr @"$s8Dispatch0A8WorkItemC5flags5blockAcA0abC5FlagsV_yyXBtcfC"(ptr noalias %22, ptr %52, ptr swiftself %34), !dbg !248
  %54 = getelementptr inbounds nuw %swift.function, ptr %43, i32 0, i32 1, !dbg !254
  %55 = load ptr, ptr %54, align 8, !dbg !254
  call void @swift_release(ptr %55) #0, !dbg !254
  call void @llvm.lifetime.end.p0(i64 48, ptr %5), !dbg !254
  call void @swift_release(ptr %35) #0, !dbg !254
  store ptr %53, ptr %6, align 8, !dbg !256
  %56 = call ptr @swift_retain(ptr returned %53) #1, !dbg !257
  %57 = ptrtoint ptr %53 to i64, !dbg !256
  %58 = inttoptr i64 %57 to ptr, !dbg !258
  %59 = call ptr @swift_retain(ptr returned %58) #1, !dbg !258
  %60 = getelementptr inbounds nuw %T18RcWeakSelfReproClsAAC, ptr %0, i32 0, i32 8, !dbg !258
  call void @llvm.lifetime.start.p0(i64 -1, ptr %7), !dbg !258
  call void @swift_beginAccess(ptr %60, ptr %7, i64 33, ptr null) #0, !dbg !258
  %61 = load i64, ptr %60, align 16, !dbg !258
  store i64 %57, ptr %60, align 16, !dbg !258
  %62 = inttoptr i64 %61 to ptr, !dbg !258
  call void @swift_release(ptr %62) #0, !dbg !258
  call void @swift_endAccess(ptr %7) #0, !dbg !258
  call void @llvm.lifetime.end.p0(i64 -1, ptr %7), !dbg !258
  %63 = inttoptr i64 %57 to ptr, !dbg !258
  call void @swift_release(ptr %63) #0, !dbg !258
  %64 = getelementptr inbounds nuw %T18RcWeakSelfReproClsAAC, ptr %0, i32 0, i32 5, !dbg !259
  %65 = load ptr, ptr %64, align 16, !dbg !259
  %66 = call ptr @llvm.objc.retain(ptr %65), !dbg !259
  call swiftcc void @"$s8Dispatch0A4TimeV3nowACyFZ"(ptr noalias sret(%swift.opaque) %14), !dbg !260
  %67 = getelementptr inbounds nuw %T18RcWeakSelfReproClsAAC, ptr %0, i32 0, i32 9, !dbg !261
  %68 = getelementptr inbounds nuw %TSd, ptr %67, i32 0, i32 0, !dbg !261
  %69 = load double, ptr %68, align 8, !dbg !261
  call swiftcc void @"$s8Dispatch1poiyAA0A4TimeVAD_SdtF"(ptr noalias sret(%swift.opaque) %15, ptr noalias %14, double %69), !dbg !262
  %70 = getelementptr inbounds ptr, ptr %11, i32 1, !dbg !263
  %71 = load ptr, ptr %70, align 8, !dbg !263, !invariant.load !235
  call void %71(ptr noalias %14, ptr %9) #0, !dbg !263
  call swiftcc void @"$sSo17OS_dispatch_queueC8DispatchE10asyncAfter8deadline7executeyAC0D4TimeV_AC0D8WorkItemCtF"(ptr noalias %15, ptr %53, ptr swiftself %65), !dbg !265
  call void @llvm.objc.release(ptr %65), !dbg !266
  call void %71(ptr noalias %15, ptr %9) #0, !dbg !266
  call void @swift_release(ptr %53) #0, !dbg !266
  call void @llvm.lifetime.end.p0(i64 -1, ptr %22), !dbg !266
  call void @llvm.lifetime.end.p0(i64 -1, ptr %15), !dbg !266
  call void @llvm.lifetime.end.p0(i64 -1, ptr %14), !dbg !266
  ret void, !dbg !266
}

declare swiftcc void @"$s8Dispatch0A8WorkItemC5flags5blockAcA0abC5FlagsV_yyXBtcfcfA_"(ptr noalias sret(%swift.opaque)) #3

define swiftcc void @"$s18RcWeakSelfReproClsAAC17persistAllEntries33_4ACD3E86E4EC54986DC19A1C6FD01C50LLyyAA010SerializedC4DataADLLVF"(ptr %0, ptr swiftself %1) #3 !dbg !267 {
  %3 = alloca ptr, align 8
    #dbg_declare(ptr %3, !274, !DIExpression(), !277)
  call void @llvm.memset.p0.i64(ptr align 8 %3, i8 0, i64 8, i1 false)
  %4 = alloca ptr, align 8
    #dbg_declare(ptr %4, !276, !DIExpression(), !278)
  call void @llvm.memset.p0.i64(ptr align 8 %4, i8 0, i64 8, i1 false)
  %5 = alloca { %objc_block, %swift.function }, align 8
  %6 = call swiftcc %swift.metadata_response @"$s8Dispatch0A13WorkItemFlagsVMa"(i64 0) #6, !dbg !279
  %7 = extractvalue %swift.metadata_response %6, 0, !dbg !279
  %8 = getelementptr inbounds ptr, ptr %7, i64 -1, !dbg !279
  %9 = load ptr, ptr %8, align 8, !dbg !279, !invariant.load !235, !dereferenceable !236
  %10 = getelementptr inbounds nuw %swift.vwtable, ptr %9, i32 0, i32 8, !dbg !279
  %11 = load i64, ptr %10, align 8, !dbg !279, !invariant.load !235
  %12 = alloca i8, i64 %11, align 16, !dbg !279
  call void @llvm.lifetime.start.p0(i64 -1, ptr %12), !dbg !279
  %13 = call swiftcc %swift.metadata_response @"$s8Dispatch0A3QoSVMa"(i64 0) #6, !dbg !279
  %14 = extractvalue %swift.metadata_response %13, 0, !dbg !279
  %15 = getelementptr inbounds ptr, ptr %14, i64 -1, !dbg !279
  %16 = load ptr, ptr %15, align 8, !dbg !279, !invariant.load !235, !dereferenceable !236
  %17 = getelementptr inbounds nuw %swift.vwtable, ptr %16, i32 0, i32 8, !dbg !279
  %18 = load i64, ptr %17, align 8, !dbg !279, !invariant.load !235
  %19 = alloca i8, i64 %18, align 16, !dbg !279
  call void @llvm.lifetime.start.p0(i64 -1, ptr %19), !dbg !279
  store ptr %0, ptr %3, align 8, !dbg !281
  store ptr %1, ptr %4, align 8, !dbg !281
  %20 = getelementptr inbounds nuw %T18RcWeakSelfReproClsAAC, ptr %1, i32 0, i32 5, !dbg !282
  %21 = load ptr, ptr %20, align 16, !dbg !282
  %22 = call ptr @llvm.objc.retain(ptr %21), !dbg !282
  %23 = call ptr @swift_bridgeObjectRetain(ptr returned %0) #0, !dbg !281
  %24 = call noalias ptr @swift_allocObject(ptr getelementptr inbounds (%swift.full_boxmetadata, ptr null, i32 0, i32 2), i64 24, i64 7) #0, !dbg !283
  %25 = getelementptr inbounds nuw <{ %swift.refcounted, %T18RcWeakSelfReproCls010SerializedC4Data33_4ACD3E86E4EC54986DC19A1C6FD01C50LLV }>, ptr %24, i32 0, i32 1, !dbg !283
  %26 = getelementptr inbounds nuw %T18RcWeakSelfReproCls010SerializedC4Data33_4ACD3E86E4EC54986DC19A1C6FD01C50LLV, ptr %25, i32 0, i32 0, !dbg !283
  %27 = getelementptr inbounds nuw %TSD, ptr %26, i32 0, i32 0, !dbg !283
  %28 = getelementptr inbounds nuw %TSD8_VariantV, ptr %27, i32 0, i32 0, !dbg !283
  %29 = getelementptr inbounds nuw %Ts14_BridgeStorageV, ptr %28, i32 0, i32 0, !dbg !283
  store ptr %0, ptr %29, align 8, !dbg !283
  call void @llvm.lifetime.start.p0(i64 48, ptr %5), !dbg !279
  %30 = getelementptr inbounds nuw { %objc_block, %swift.function }, ptr %5, i32 0, i32 1, !dbg !281
  %31 = getelementptr inbounds nuw %swift.function, ptr %30, i32 0, i32 0, !dbg !281
  store ptr null, ptr %31, align 8, !dbg !281
  %32 = getelementptr inbounds nuw %swift.function, ptr %30, i32 0, i32 1, !dbg !281
  store ptr %24, ptr %32, align 8, !dbg !281
  %33 = getelementptr inbounds nuw { %objc_block, %swift.function }, ptr %5, i32 0, i32 0, !dbg !281
  %34 = getelementptr inbounds nuw %objc_block, ptr %33, i32 0, i32 0, !dbg !281
  store ptr null, ptr %34, align 8, !dbg !281
  %35 = getelementptr inbounds nuw %objc_block, ptr %33, i32 0, i32 1, !dbg !281
  store i32 1107296256, ptr %35, align 8, !dbg !281
  %36 = getelementptr inbounds nuw %objc_block, ptr %33, i32 0, i32 2, !dbg !281
  store i32 0, ptr %36, align 4, !dbg !281
  %37 = getelementptr inbounds nuw %objc_block, ptr %33, i32 0, i32 3, !dbg !281
  store ptr null, ptr %37, align 8, !dbg !281
  %38 = getelementptr inbounds nuw %objc_block, ptr %33, i32 0, i32 4, !dbg !281
  store ptr null, ptr %38, align 8, !dbg !281
  %39 = call ptr @_Block_copy(ptr %5), !dbg !281
  call swiftcc void @"$sSo17OS_dispatch_queueC8DispatchE5async5group3qos5flags7executeySo0a1_b1_F0CSg_AC0D3QoSVAC0D13WorkItemFlagsVyyXBtFfA0_"(ptr noalias sret(%swift.opaque) %19), !dbg !281
  call swiftcc void @"$sSo17OS_dispatch_queueC8DispatchE5async5group3qos5flags7executeySo0a1_b1_F0CSg_AC0D3QoSVAC0D13WorkItemFlagsVyyXBtFfA1_"(ptr noalias sret(%swift.opaque) %12), !dbg !281
  call swiftcc void @"$sSo17OS_dispatch_queueC8DispatchE5async5group3qos5flags7executeySo0a1_b1_F0CSg_AC0D3QoSVAC0D13WorkItemFlagsVyyXBtF"(i64 0, ptr noalias %19, ptr noalias %12, ptr %39, ptr swiftself %21), !dbg !284
  call void @_Block_release(ptr %39) #0, !dbg !285
  call void @llvm.objc.release(ptr %21), !dbg !285
  %40 = getelementptr inbounds ptr, ptr %9, i32 1, !dbg !285
  %41 = load ptr, ptr %40, align 8, !dbg !285, !invariant.load !235
  call void %41(ptr noalias %12, ptr %7) #0, !dbg !285
  %42 = getelementptr inbounds ptr, ptr %16, i32 1, !dbg !285
  %43 = load ptr, ptr %42, align 8, !dbg !285, !invariant.load !235
  call void %43(ptr noalias %19, ptr %14) #0, !dbg !285
  %44 = getelementptr inbounds nuw %swift.function, ptr %30, i32 0, i32 1, !dbg !285
  %45 = load ptr, ptr %44, align 8, !dbg !285
  call void @swift_release(ptr %45) #0, !dbg !285
  call void @llvm.lifetime.end.p0(i64 48, ptr %5), !dbg !285
  call void @llvm.lifetime.end.p0(i64 -1, ptr %19), !dbg !285
  call void @llvm.lifetime.end.p0(i64 -1, ptr %12), !dbg !285
  ret void, !dbg !285
}

declare swiftcc void @"$sSo17OS_dispatch_queueC8DispatchE5async5group3qos5flags7executeySo0a1_b1_F0CSg_AC0D3QoSVAC0D13WorkItemFlagsVyyXBtFfA0_"(ptr noalias sret(%swift.opaque)) #3

declare swiftcc void @"$sSo17OS_dispatch_queueC8DispatchE5async5group3qos5flags7executeySo0a1_b1_F0CSg_AC0D3QoSVAC0D13WorkItemFlagsVyyXBtFfA1_"(ptr noalias sret(%swift.opaque)) #3

; Function Attrs: nounwind
declare void @llvm.objc.release(ptr) #0

; Function Attrs: noinline nounwind
declare ptr @"$s8Dispatch0A8WorkItemCSgWOh"(ptr) #5

declare swiftcc void @"$s8Dispatch0A8WorkItemC6cancelyyFTj"(ptr swiftself) #3

declare swiftcc %swift.metadata_response @"$s8Dispatch0A4TimeVMa"(i64) #3

declare swiftcc %swift.metadata_response @"$s8Dispatch0A13WorkItemFlagsVMa"(i64) #3

declare swiftcc %swift.metadata_response @"$s8Dispatch0A8WorkItemCMa"(i64) #3

; Function Attrs: nounwind willreturn
declare ptr @swift_weakInit(ptr returned, ptr) #1

declare ptr @_Block_copy(ptr)

declare swiftcc ptr @"$s8Dispatch0A8WorkItemC5flags5blockAcA0abC5FlagsV_yyXBtcfC"(ptr noalias, ptr, ptr swiftself) #3

declare swiftcc void @"$s8Dispatch0A4TimeV3nowACyFZ"(ptr noalias sret(%swift.opaque)) #3

declare swiftcc void @"$s8Dispatch1poiyAA0A4TimeVAD_SdtF"(ptr noalias sret(%swift.opaque), ptr noalias, double) #3

declare swiftcc void @"$sSo17OS_dispatch_queueC8DispatchE10asyncAfter8deadline7executeyAC0D4TimeV_AC0D8WorkItemCtF"(ptr noalias, ptr, ptr swiftself) #3

declare swiftcc void @"$sSo17OS_dispatch_queueC8DispatchE5async5group3qos5flags7executeySo0a1_b1_F0CSg_AC0D3QoSVAC0D13WorkItemFlagsVyyXBtF"(i64, ptr noalias, ptr noalias, ptr, ptr swiftself) #3

; Function Attrs: nounwind
declare void @_Block_release(ptr) #0

; uselistorder directives
uselistorder ptr @llvm.objc.release, { 1, 0 }
uselistorder ptr @"$s8Dispatch0A13WorkItemFlagsVMa", { 1, 0 }
uselistorder ptr @_Block_copy, { 1, 0 }

attributes #0 = { nounwind }
attributes #1 = { nounwind willreturn }
attributes #2 = { nocallback nofree nosync nounwind willreturn memory(argmem: readwrite) }
attributes #3 = { "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+altnzcv,+ccdp,+ccidx,+ccpp,+complxnum,+crc,+dit,+dotprod,+flagm,+fp-armv8,+fp16fml,+fptoint,+fullfp16,+jsconv,+lse,+neon,+pauth,+perfmon,+predres,+ras,+rcpc,+rdm,+sb,+sha2,+sha3,+specrestrict,+ssbs,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8a" }
attributes #4 = { nocallback nofree nounwind willreturn memory(argmem: write) }
attributes #5 = { noinline nounwind "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+altnzcv,+ccdp,+ccidx,+ccpp,+complxnum,+crc,+dit,+dotprod,+flagm,+fp-armv8,+fp16fml,+fptoint,+fullfp16,+jsconv,+lse,+neon,+pauth,+perfmon,+predres,+ras,+rcpc,+rdm,+sb,+sha2,+sha3,+specrestrict,+ssbs,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8a" }
attributes #6 = { nounwind memory(none) }

!llvm.module.flags = !{!0, !1, !2, !3, !4, !5, !6, !7, !8, !9, !10, !11, !12, !13, !14, !15, !16, !17}
!llvm.dbg.cu = !{!18, !144, !146, !148, !150, !152, !154, !156, !158, !160, !162, !164}
!swift.module.flags = !{!166}
!llvm.linker.options = !{!167, !168, !169, !170, !171, !172, !173, !174, !175, !176, !177, !178, !179, !180, !181, !182, !183, !184, !185, !186, !187, !188, !189, !190, !191, !192, !193, !194, !195, !196, !197, !198, !199, !200, !201, !202, !203, !204, !205, !206, !207, !208, !209, !210, !211, !212, !213, !214, !215, !216, !217}

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
!18 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !19, producer: "Swift version (redacted)", isOptimized: false, runtimeVersion: 6, emissionKind: FullDebug, retainedTypes: !20, globals: !35, imports: !85, sysroot: "build/cache/", sdk: "iPhoneSimulator26.2.sdk")
!19 = !DIFile(filename: "infer/tests/codetoanalyze/swift/bitcode-handcrafted-pulse/rc_dispatch_workitem_weak_self.swift", directory: ".")
!20 = !{!21, !29}
!21 = !DIDerivedType(tag: DW_TAG_typedef, name: "$s10Foundation12NotificationV4NameaD", scope: !23, file: !22, baseType: !25)
!22 = !DIFile(filename: "build/cache/", directory: ".")
!23 = !DICompositeType(tag: DW_TAG_structure_type, name: "Notification", scope: !24, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$s10Foundation12NotificationVD")
!24 = !DIModule(scope: null, name: "Foundation", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!25 = !DICompositeType(tag: DW_TAG_structure_type, name: "Name", scope: !27, file: !26, size: 64, runtimeLang: DW_LANG_Swift, identifier: "$sSo18NSNotificationNameaD")
!26 = !DIFile(filename: "build/cache/", directory: ".")
!27 = !DIModule(scope: !28, name: "NSNotification", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!28 = !DIModule(scope: null, name: "Foundation", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!29 = !DIDerivedType(tag: DW_TAG_typedef, name: "$sSo14NSTimeIntervalaD", scope: !31, file: !30, baseType: !32)
!30 = !DIFile(filename: "build/cache/", directory: ".")
!31 = !DIModule(scope: !28, name: "NSDate", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!32 = !DICompositeType(tag: DW_TAG_structure_type, name: "Double", scope: !34, file: !33, size: 64, runtimeLang: DW_LANG_Swift, identifier: "$sSdD")
!33 = !DIFile(filename: "build/cache/", directory: ".")
!34 = !DIModule(scope: null, name: "Swift", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!35 = !{!36, !41, !44, !49, !51, !53, !55, !57, !61, !63, !65, !68, !73, !78}
!36 = !DIGlobalVariableExpression(var: !37, expr: !DIExpression())
!37 = distinct !DIGlobalVariable(name: "shared", linkageName: "$s18RcWeakSelfReproClsAAC6sharedABvpZ", scope: !38, file: !19, line: 20, type: !39, isLocal: false, isDefinition: true)
!38 = !DIModule(scope: null, name: "RcWeakSelfReproCls", includePath: "build/cache/")
!39 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !40)
!40 = !DICompositeType(tag: DW_TAG_structure_type, name: "RcWeakSelfReproCls", scope: !38, file: !19, size: 64, runtimeLang: DW_LANG_Swift, identifier: "$s18RcWeakSelfReproClsAACD")
!41 = !DIGlobalVariableExpression(var: !42, expr: !DIExpression())
!42 = distinct !DIGlobalVariable(name: "igDirectBadgeCacheCountDidChange", linkageName: "$sSo18NSNotificationNamea18RcWeakSelfReproClsE08igDirectdE14CountDidChangeABvpZ", scope: !38, file: !19, line: 321, type: !43, isLocal: false, isDefinition: true)
!43 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !21)
!44 = !DIGlobalVariableExpression(var: !45, expr: !DIExpression())
!45 = distinct !DIGlobalVariable(name: "storageKey", linkageName: "$s18RcWeakSelfReproCls0abC9ConstantsO10storageKeySSvpZ", scope: !38, file: !46, line: 7, type: !47, isLocal: false, isDefinition: true)
!46 = !DIFile(filename: "infer/tests/codetoanalyze/swift/bitcode-handcrafted-pulse/rc_dispatch_workitem_weak_self.swift", directory: ".")
!47 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !48)
!48 = !DICompositeType(tag: DW_TAG_structure_type, name: "String", scope: !34, file: !33, size: 128, runtimeLang: DW_LANG_Swift, identifier: "$sSSD")
!49 = !DIGlobalVariableExpression(var: !50, expr: !DIExpression())
!50 = distinct !DIGlobalVariable(name: "threadsKey", linkageName: "$s18RcWeakSelfReproCls0abC9ConstantsO10threadsKeySSvpZ", scope: !38, file: !46, line: 9, type: !47, isLocal: false, isDefinition: true)
!51 = !DIGlobalVariableExpression(var: !52, expr: !DIExpression())
!52 = distinct !DIGlobalVariable(name: "threadIDKey", linkageName: "$s18RcWeakSelfReproCls0abC9ConstantsO11threadIDKeySSvpZ", scope: !38, file: !46, line: 10, type: !47, isLocal: false, isDefinition: true)
!53 = !DIGlobalVariableExpression(var: !54, expr: !DIExpression())
!54 = distinct !DIGlobalVariable(name: "timestampKey", linkageName: "$s18RcWeakSelfReproCls0abC9ConstantsO12timestampKeySSvpZ", scope: !38, file: !46, line: 11, type: !47, isLocal: false, isDefinition: true)
!55 = !DIGlobalVariableExpression(var: !56, expr: !DIExpression())
!56 = distinct !DIGlobalVariable(name: "isUnreadKey", linkageName: "$s18RcWeakSelfReproCls0abC9ConstantsO11isUnreadKeySSvpZ", scope: !38, file: !46, line: 12, type: !47, isLocal: false, isDefinition: true)
!57 = !DIGlobalVariableExpression(var: !58, expr: !DIExpression())
!58 = distinct !DIGlobalVariable(name: "maxThreadsPerUser", linkageName: "$s18RcWeakSelfReproCls0abC9ConstantsO17maxThreadsPerUserSivpZ", scope: !38, file: !46, line: 15, type: !59, isLocal: false, isDefinition: true)
!59 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !60)
!60 = !DICompositeType(tag: DW_TAG_structure_type, name: "Int", scope: !34, file: !33, size: 64, runtimeLang: DW_LANG_Swift, identifier: "$sSiD")
!61 = !DIGlobalVariableExpression(var: !62, expr: !DIExpression())
!62 = distinct !DIGlobalVariable(name: "persistenceQueueLabel", linkageName: "$s18RcWeakSelfReproCls0abC9ConstantsO21persistenceQueueLabelSSvpZ", scope: !38, file: !46, line: 16, type: !47, isLocal: false, isDefinition: true)
!63 = !DIGlobalVariableExpression(var: !64, expr: !DIExpression())
!64 = distinct !DIGlobalVariable(name: "persistenceDelayMs", linkageName: "$s18RcWeakSelfReproCls0abC9ConstantsO18persistenceDelayMsSivpZ", scope: !38, file: !46, line: 17, type: !59, isLocal: false, isDefinition: true)
!65 = !DIGlobalVariableExpression(var: !66, expr: !DIExpression())
!66 = distinct !DIGlobalVariable(name: "countDidChangeNotificationName", linkageName: "$s18RcWeakSelfReproCls0abC13ObjCConstantsC30countDidChangeNotificationNameSo014NSNotificationJ0avpZ", scope: !38, file: !46, line: 24, type: !67, isLocal: false, isDefinition: true)
!67 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !25)
!68 = !DIGlobalVariableExpression(var: !69, expr: !DIExpression())
!69 = distinct !DIGlobalVariable(name: "shared", linkageName: "$s18RcWeakSelfReproCls0abC14ListenerBridgeC6sharedACvpZ", scope: !38, file: !70, line: 23, type: !71, isLocal: false, isDefinition: true)
!70 = !DIFile(filename: "infer/tests/codetoanalyze/swift/bitcode-handcrafted-pulse/rc_dispatch_workitem_weak_self.swift", directory: ".")
!71 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !72)
!72 = !DICompositeType(tag: DW_TAG_structure_type, name: "RcWeakSelfReproClsListenerBridge", scope: !38, file: !70, size: 64, runtimeLang: DW_LANG_Swift, identifier: "$s18RcWeakSelfReproCls0abC14ListenerBridgeCD")
!73 = !DIGlobalVariableExpression(var: !74, expr: !DIExpression())
!74 = distinct !DIGlobalVariable(name: "shared", linkageName: "$s18RcWeakSelfReproCls0abC14MainAppServiceC6sharedACvpZ", scope: !38, file: !75, line: 25, type: !76, isLocal: false, isDefinition: true)
!75 = !DIFile(filename: "infer/tests/codetoanalyze/swift/bitcode-handcrafted-pulse/rc_dispatch_workitem_weak_self.swift", directory: ".")
!76 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !77)
!77 = !DICompositeType(tag: DW_TAG_structure_type, name: "RcWeakSelfReproClsMainAppService", scope: !38, file: !75, size: 64, runtimeLang: DW_LANG_Swift, identifier: "$s18RcWeakSelfReproCls0abC14MainAppServiceCD")
!78 = !DIGlobalVariableExpression(var: !79, expr: !DIExpression())
!79 = distinct !DIGlobalVariable(name: "persistenceQueue", linkageName: "$s18RcWeakSelfReproCls0abC10NSEServiceC16persistenceQueue33_D17F55FB484A6647B43BC9DF893DE4E7LLSo17OS_dispatch_queueCvpZ", scope: !38, file: !80, line: 9, type: !81, isLocal: true, isDefinition: true)
!80 = !DIFile(filename: "infer/tests/codetoanalyze/swift/bitcode-handcrafted-pulse/rc_dispatch_workitem_weak_self.swift", directory: ".")
!81 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !82)
!82 = !DICompositeType(tag: DW_TAG_structure_type, name: "DispatchQueue", scope: !84, file: !83, size: 64, runtimeLang: DW_LANG_Swift, identifier: "$sSo17OS_dispatch_queueCD")
!83 = !DIFile(filename: "build/cache/", directory: ".")
!84 = !DIModule(scope: null, name: "Dispatch", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!85 = !{!86, !87, !88, !90, !92, !93, !94, !95, !97, !98, !100, !101, !103, !104, !105, !106, !108, !109, !110, !111, !113, !114, !115, !116, !117, !118, !119, !121, !122, !123, !125, !126, !128, !129, !130, !131, !132, !133, !135, !136, !138, !140, !142}
!86 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !38, file: !19)
!87 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: null, entity: !34)
!88 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !89, file: !19, line: 3)
!89 = !DIModule(scope: null, name: "FakeSwiftExtensionsTask", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!90 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !91, file: !19)
!91 = !DIModule(scope: null, name: "FakeConfigContextProvider", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!92 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !91, file: !19, line: 4)
!93 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !28, file: !19)
!94 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !24, file: !19, line: 5)
!95 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !96, file: !19)
!96 = !DIModule(scope: null, name: "RcWeakSelfReproClsMobileConfigs", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!97 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !96, file: !19, line: 6)
!98 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !99, file: !19)
!99 = !DIModule(scope: null, name: "FakeFeatureSets", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!100 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !99, file: !19, line: 7)
!101 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !102, file: !19)
!102 = !DIModule(scope: null, name: "FakeUserDefaults", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!103 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !102, file: !19, line: 8)
!104 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !46, entity: !28, file: !46)
!105 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !46, entity: !24, file: !46, line: 3)
!106 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !107, entity: !28, file: !107)
!107 = !DIFile(filename: "infer/tests/codetoanalyze/swift/bitcode-handcrafted-pulse/rc_dispatch_workitem_weak_self.swift", directory: ".")
!108 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !107, entity: !24, file: !107, line: 3)
!109 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !70, entity: !28, file: !70)
!110 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !70, entity: !24, file: !70, line: 3)
!111 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !70, entity: !112, file: !70, line: 4)
!112 = !DIModule(scope: null, name: "FakeConcurrency", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!113 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !75, entity: !91, file: !75)
!114 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !75, entity: !91, file: !75, line: 3)
!115 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !75, entity: !28, file: !75)
!116 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !75, entity: !24, file: !75, line: 4)
!117 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !75, entity: !96, file: !75)
!118 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !75, entity: !96, file: !75, line: 5)
!119 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !75, entity: !120, file: !75, line: 6)
!120 = !DIModule(scope: null, name: "FakeDeviceTrackingService", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!121 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !75, entity: !99, file: !75)
!122 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !75, entity: !99, file: !75, line: 7)
!123 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !75, entity: !124, file: !75)
!124 = !DIModule(scope: null, name: "FakeFeatureGating", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!125 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !75, entity: !124, file: !75, line: 8)
!126 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !75, entity: !127, file: !75)
!127 = !DIModule(scope: null, name: "FakeUserSession", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!128 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !75, entity: !127, file: !75, line: 9)
!129 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !80, entity: !28, file: !80)
!130 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !80, entity: !24, file: !80, line: 3)
!131 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !80, entity: !102, file: !80)
!132 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !80, entity: !102, file: !80, line: 4)
!133 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !134, entity: !28, file: !134)
!134 = !DIFile(filename: "infer/tests/codetoanalyze/swift/bitcode-handcrafted-pulse/rc_dispatch_workitem_weak_self.swift", directory: ".")
!135 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !134, entity: !24, file: !134, line: 3)
!136 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !137, file: !19)
!137 = !DIModule(scope: null, name: "_StringProcessing", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!138 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !139, file: !19)
!139 = !DIModule(scope: null, name: "_SwiftConcurrencyShims", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!140 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !141, file: !19)
!141 = !DIModule(scope: null, name: "_Concurrency", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!142 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !143, file: !19)
!143 = !DIModule(scope: null, name: "SwiftOnoneSupport", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!144 = distinct !DICompileUnit(language: DW_LANG_ObjC, file: !145, producer: "clang version (redacted)", isOptimized: false, runtimeVersion: 2, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: Apple, sysroot: "build/cache/", sdk: "iPhoneSimulator26.2.sdk")
!145 = !DIFile(filename: "<swift-imported-modules>", directory: ".")
!146 = distinct !DICompileUnit(language: DW_LANG_ObjC, file: !147, producer: "Swift version (redacted)", isOptimized: true, runtimeVersion: 0, splitDebugFilename: "build/cache/", emissionKind: FullDebug, dwoId: 18446744073709551614)
!147 = !DIFile(filename: "Foundation", directory: ".")
!148 = distinct !DICompileUnit(language: DW_LANG_ObjC, file: !149, producer: "Swift version (redacted)", isOptimized: true, runtimeVersion: 0, splitDebugFilename: "build/cache/", emissionKind: FullDebug, dwoId: 18446744073709551614)
!149 = !DIFile(filename: "Dispatch", directory: ".")
!150 = distinct !DICompileUnit(language: DW_LANG_ObjC, file: !151, producer: "Swift version (redacted)", isOptimized: true, runtimeVersion: 0, splitDebugFilename: "build/cache/", emissionKind: FullDebug, dwoId: 18446744073709551614)
!151 = !DIFile(filename: "FakeUserDefaults", directory: ".")
!152 = distinct !DICompileUnit(language: DW_LANG_ObjC, file: !153, producer: "Swift version (redacted)", isOptimized: true, runtimeVersion: 0, splitDebugFilename: "build/cache/", emissionKind: FullDebug, dwoId: 18446744073709551614)
!153 = !DIFile(filename: "FakeUserSession", directory: ".")
!154 = distinct !DICompileUnit(language: DW_LANG_ObjC, file: !155, producer: "Swift version (redacted)", isOptimized: true, runtimeVersion: 0, splitDebugFilename: "build/cache/", emissionKind: FullDebug, dwoId: 18446744073709551614)
!155 = !DIFile(filename: "FakeConfigContext", directory: ".")
!156 = distinct !DICompileUnit(language: DW_LANG_ObjC, file: !157, producer: "Swift version (redacted)", isOptimized: true, runtimeVersion: 0, splitDebugFilename: "build/cache/", emissionKind: FullDebug, dwoId: 18446744073709551614)
!157 = !DIFile(filename: "FakeConfigContextProvider", directory: ".")
!158 = distinct !DICompileUnit(language: DW_LANG_ObjC, file: !159, producer: "Swift version (redacted)", isOptimized: true, runtimeVersion: 0, splitDebugFilename: "build/cache/", emissionKind: FullDebug, dwoId: 18446744073709551614)
!159 = !DIFile(filename: "RcWeakSelfReproClsMobileConfigs", directory: ".")
!160 = distinct !DICompileUnit(language: DW_LANG_ObjC, file: !161, producer: "Swift version (redacted)", isOptimized: true, runtimeVersion: 0, splitDebugFilename: "build/cache/", emissionKind: FullDebug, dwoId: 18446744073709551614)
!161 = !DIFile(filename: "FakeFeatureSets", directory: ".")
!162 = distinct !DICompileUnit(language: DW_LANG_ObjC, file: !163, producer: "Swift version (redacted)", isOptimized: true, runtimeVersion: 0, splitDebugFilename: "build/cache/", emissionKind: FullDebug, dwoId: 18446744073709551614)
!163 = !DIFile(filename: "FakeFeatureGating", directory: ".")
!164 = distinct !DICompileUnit(language: DW_LANG_ObjC, file: !165, producer: "Swift version (redacted)", isOptimized: true, runtimeVersion: 0, splitDebugFilename: "build/cache/", emissionKind: FullDebug, dwoId: 18446744073709551614)
!165 = !DIFile(filename: "_SwiftConcurrencyShims", directory: ".")
!166 = !{!"standard-library", i1 false}
!167 = !{!"-lswiftFoundation"}
!168 = !{!"-framework", !"Foundation"}
!169 = !{!"-lswiftCore"}
!170 = !{!"-lswift_DarwinFoundation3"}
!171 = !{!"-lswift_DarwinFoundation1"}
!172 = !{!"-lswift_DarwinFoundation2"}
!173 = !{!"-lswift_StringProcessing"}
!174 = !{!"-lswift_Concurrency"}
!175 = !{!"-lswiftSystem"}
!176 = !{!"-lswiftDarwin"}
!177 = !{!"-lswift_Builtin_float"}
!178 = !{!"-lswiftObservation"}
!179 = !{!"-lswiftObjectiveC"}
!180 = !{!"-lswiftCoreFoundation"}
!181 = !{!"-framework", !"CoreFoundation"}
!182 = !{!"-lswiftDispatch"}
!183 = !{!"-framework", !"Combine"}
!184 = !{!"-framework", !"Security"}
!185 = !{!"-lswiftXPC"}
!186 = !{!"-framework", !"CFNetwork"}
!187 = !{!"-lswiftSwiftOnoneSupport"}
!188 = !{!"-lswiftQuartzCore"}
!189 = !{!"-framework", !"QuartzCore"}
!190 = !{!"-framework", !"CoreGraphics"}
!191 = !{!"-framework", !"CoreVideo"}
!192 = !{!"-framework", !"IOSurface"}
!193 = !{!"-lswiftMetal"}
!194 = !{!"-framework", !"Metal"}
!195 = !{!"-framework", !"OpenGLES"}
!196 = !{!"-lswiftUIKit"}
!197 = !{!"-framework", !"UIKit"}
!198 = !{!"-framework", !"UIUtilities"}
!199 = !{!"-lswiftCoreImage"}
!200 = !{!"-framework", !"CoreImage"}
!201 = !{!"-framework", !"ImageIO"}
!202 = !{!"-framework", !"CoreText"}
!203 = !{!"-lswiftUniformTypeIdentifiers"}
!204 = !{!"-framework", !"UniformTypeIdentifiers"}
!205 = !{!"-framework", !"Symbols"}
!206 = !{!"-framework", !"UserNotifications"}
!207 = !{!"-framework", !"FileProvider"}
!208 = !{!"-framework", !"SwiftUICore"}
!209 = !{!"-lswiftos"}
!210 = !{!"-lswiftOSLog"}
!211 = !{!"-framework", !"OSLog"}
!212 = !{!"-framework", !"DeveloperToolsSupport"}
!213 = !{!"-framework", !"CoreTransferable"}
!214 = !{!"-framework", !"Accessibility"}
!215 = !{!"-framework", !"DataDetection"}
!216 = !{!"-lobjc"}
!217 = !{!"-lswiftCompatibilityPacks"}
!218 = distinct !DISubprogram(name: "scheduleDelayedPersist", linkageName: "$s18RcWeakSelfReproClsAAC22scheduleDelayedPersist33_4ACD3E86E4EC54986DC19A1C6FD01C50LLyyF", scope: !40, file: !19, line: 225, type: !219, scopeLine: 225, spFlags: DISPFlagDefinition, unit: !18, declaration: !222, retainedNodes: !223)
!219 = !DISubroutineType(types: !220)
!220 = !{!221, !40}
!221 = !DICompositeType(tag: DW_TAG_structure_type, name: "$sytD", flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$sytD")
!222 = !DISubprogram(name: "scheduleDelayedPersist", linkageName: "$s18RcWeakSelfReproClsAAC22scheduleDelayedPersist33_4ACD3E86E4EC54986DC19A1C6FD01C50LLyyF", scope: !40, file: !19, line: 225, type: !219, scopeLine: 225, spFlags: 0)
!223 = !{!224, !225}
!224 = !DILocalVariable(name: "self", arg: 1, scope: !218, file: !19, line: 225, type: !39, flags: DIFlagArtificial)
!225 = !DILocalVariable(name: "workItem", scope: !226, file: !19, line: 228, type: !227)
!226 = distinct !DILexicalBlock(scope: !218, file: !19, line: 228, column: 13)
!227 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !228)
!228 = !DICompositeType(tag: DW_TAG_structure_type, name: "DispatchWorkItem", scope: !230, file: !229, size: 64, runtimeLang: DW_LANG_Swift, identifier: "$s8Dispatch0A8WorkItemCD")
!229 = !DIFile(filename: "build/cache/", directory: ".")
!230 = !DIModule(scope: null, name: "Dispatch", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!231 = !DILocation(line: 225, column: 18, scope: !218)
!232 = !DILocation(line: 228, column: 13, scope: !226)
!233 = !DILocation(line: 0, scope: !234)
!234 = !DILexicalBlockFile(scope: !226, discriminator: 0)
!235 = !{}
!236 = !{i64 96}
!237 = !DILocation(line: 0, scope: !238)
!238 = !DILexicalBlockFile(scope: !239, discriminator: 0)
!239 = distinct !DILexicalBlock(scope: !218, file: !19, line: 228, column: 24)
!240 = !DILocation(line: 0, scope: !218)
!241 = !DILocation(line: 0, scope: !242)
!242 = !DILexicalBlockFile(scope: !218, discriminator: 0)
!243 = !DILocation(line: 226, column: 31, scope: !218)
!244 = !DILocation(line: 0, scope: !245)
!245 = !DILexicalBlockFile(scope: !218, file: !246, discriminator: 0)
!246 = !DIFile(filename: "<compiler-generated>", directory: "")
!247 = !DILocation(line: 226, column: 33, scope: !218)
!248 = !DILocation(line: 228, column: 24, scope: !239)
!249 = !DILocation(line: 0, scope: !250)
!250 = !DILexicalBlockFile(scope: !251, discriminator: 0)
!251 = distinct !DILexicalBlock(scope: !239, file: !19, line: 229, column: 13)
!252 = !DILocation(line: 228, column: 49, scope: !239)
!253 = !DILocation(line: 0, scope: !239)
!254 = !DILocation(line: 0, scope: !255)
!255 = !DILexicalBlockFile(scope: !239, file: !246, discriminator: 0)
!256 = !DILocation(line: 0, scope: !226)
!257 = !DILocation(line: 234, column: 34, scope: !226)
!258 = !DILocation(line: 234, column: 32, scope: !226)
!259 = !DILocation(line: 235, column: 9, scope: !226)
!260 = !DILocation(line: 235, column: 48, scope: !226)
!261 = !DILocation(line: 235, column: 56, scope: !226)
!262 = !DILocation(line: 235, column: 54, scope: !226)
!263 = !DILocation(line: 0, scope: !264)
!264 = !DILexicalBlockFile(scope: !226, file: !246, discriminator: 0)
!265 = !DILocation(line: 235, column: 26, scope: !226)
!266 = !DILocation(line: 236, column: 5, scope: !226)
!267 = distinct !DISubprogram(name: "persistAllEntries", linkageName: "$s18RcWeakSelfReproClsAAC17persistAllEntries33_4ACD3E86E4EC54986DC19A1C6FD01C50LLyyAA010SerializedC4DataADLLVF", scope: !40, file: !19, line: 244, type: !268, scopeLine: 244, spFlags: DISPFlagDefinition, unit: !18, declaration: !272, retainedNodes: !273)
!268 = !DISubroutineType(types: !269)
!269 = !{!221, !270, !40}
!270 = !DICompositeType(tag: DW_TAG_structure_type, name: "SerializedCacheData", scope: !271, file: !19, size: 64, runtimeLang: DW_LANG_Swift, identifier: "$s18RcWeakSelfReproCls010SerializedC4Data33_4ACD3E86E4EC54986DC19A1C6FD01C50LLVD")
!271 = !DINamespace(name: "_4ACD3E86E4EC54986DC19A1C6FD01C50", scope: !38, exportSymbols: true)
!272 = !DISubprogram(name: "persistAllEntries", linkageName: "$s18RcWeakSelfReproClsAAC17persistAllEntries33_4ACD3E86E4EC54986DC19A1C6FD01C50LLyyAA010SerializedC4DataADLLVF", scope: !40, file: !19, line: 244, type: !268, scopeLine: 244, spFlags: 0)
!273 = !{!274, !276}
!274 = !DILocalVariable(name: "serialized", arg: 1, scope: !267, file: !19, line: 244, type: !275)
!275 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !270)
!276 = !DILocalVariable(name: "self", arg: 2, scope: !267, file: !19, line: 244, type: !39, flags: DIFlagArtificial)
!277 = !DILocation(line: 244, column: 48, scope: !267)
!278 = !DILocation(line: 244, column: 30, scope: !267)
!279 = !DILocation(line: 0, scope: !280)
!280 = !DILexicalBlockFile(scope: !267, discriminator: 0)
!281 = !DILocation(line: 0, scope: !267)
!282 = !DILocation(line: 245, column: 9, scope: !267)
!283 = !DILocation(line: 245, column: 32, scope: !267)
!284 = !DILocation(line: 245, column: 26, scope: !267)
!285 = !DILocation(line: 253, column: 5, scope: !267)
