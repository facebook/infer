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

; ModuleID = '/home/dulmarod/infer-astro/infer/infer/tests/codetoanalyze/swift/bitcode-handcrafted-pulse/rc_async_actor_getter.swift.ll'
source_filename = "AsyncActorGetter.o"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-ios15.1.0-simulator"

%swift.async_func_pointer = type <{ i32, i32 }>
%swift.protocol_conformance_descriptor = type { i32, i32, i32, i32 }
%T16AsyncActorGetter8AAGUtilsC = type <{ %swift.refcounted, %TSi }>
%swift.refcounted = type { ptr, i64 }
%TSi = type <{ i64 }>
%T16AsyncActorGetter10AAGHandlerC = type <{ %swift.refcounted, %swift.defaultactor, ptr }>
%swift.defaultactor = type { [12 x ptr] }
%"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaF.Frame" = type { [24 x i8], [24 x i8], ptr, ptr, ptr }
%"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame" = type { ptr, ptr, ptr, i64, i64, ptr, ptr }
%swift.metadata_response = type { ptr, i64 }

@"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaFTu" = external hidden global %swift.async_func_pointer, align 8
@"$sS2cMScAsWL" = external hidden global ptr, align 8
@"$sScMScAsMc" = external global %swift.protocol_conformance_descriptor, align 4
@"$s16AsyncActorGetter8AAGUtilsCMf" = external hidden global <{ ptr, ptr, ptr, i64, ptr, ptr, ptr, i64, i32, i32, i32, i16, i16, i32, i32, ptr, ptr, i64, ptr }>, align 8
@"$s16AsyncActorGetter10AAGHandlerCMf" = external hidden global <{ ptr, ptr, ptr, i64, ptr, ptr, ptr, i64, i32, i32, i32, i16, i16, i32, i32, ptr, ptr, i64, i64, ptr, ptr, ptr, ptr, ptr }>, align 8

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.start.p0(i64 immarg, ptr captures(none)) #0

; Function Attrs: nounwind
declare void @swift_beginAccess(ptr, ptr, i64, ptr) #1

; Function Attrs: nounwind
declare void @swift_endAccess(ptr) #1

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.end.p0(i64 immarg, ptr captures(none)) #0

define hidden swiftcc ptr @"$s16AsyncActorGetter8AAGUtilsCACycfC"(ptr swiftself %0) #2 !dbg !62 {
  %2 = call noalias ptr @swift_allocObject(ptr %0, i64 24, i64 7) #1, !dbg !68
  %3 = call swiftcc ptr @"$s16AsyncActorGetter8AAGUtilsCACycfc"(ptr swiftself %2), !dbg !68
  ret ptr %3, !dbg !68
}

define hidden swiftcc ptr @"$s16AsyncActorGetter8AAGUtilsCACycfc"(ptr swiftself %0) #2 !dbg !69 {
  %2 = alloca ptr, align 8
    #dbg_declare(ptr %2, !74, !DIExpression(), !76)
  call void @llvm.memset.p0.i64(ptr align 8 %2, i8 0, i64 8, i1 false)
  store ptr %0, ptr %2, align 8, !dbg !77
  %3 = getelementptr inbounds nuw %T16AsyncActorGetter8AAGUtilsC, ptr %0, i32 0, i32 1, !dbg !78
  %4 = getelementptr inbounds nuw %TSi, ptr %3, i32 0, i32 0, !dbg !81
  store i64 0, ptr %4, align 8, !dbg !81
  ret ptr %0, !dbg !83
}

; Function Attrs: nounwind willreturn
declare ptr @swift_retain(ptr returned) #3

; Function Attrs: nounwind
declare void @swift_release(ptr) #1

define hidden swiftcc ptr @"$s16AsyncActorGetter10AAGHandlerC5utilsAcA8AAGUtilsC_tcfC"(ptr %0, ptr swiftself %1) #2 !dbg !84 {
  %3 = call noalias ptr @swift_allocObject(ptr %1, i64 120, i64 15) #1, !dbg !90
  %4 = call swiftcc ptr @"$s16AsyncActorGetter10AAGHandlerC5utilsAcA8AAGUtilsC_tcfc"(ptr %0, ptr swiftself %3), !dbg !90
  ret ptr %4, !dbg !90
}

; Function Attrs: nounwind
declare ptr @swift_allocObject(ptr, i64, i64) #1

define hidden swiftcc ptr @"$s16AsyncActorGetter10AAGHandlerC5utilsAcA8AAGUtilsC_tcfc"(ptr %0, ptr swiftself %1) #2 !dbg !91 {
  %3 = alloca ptr, align 8
    #dbg_declare(ptr %3, !96, !DIExpression(), !99)
  call void @llvm.memset.p0.i64(ptr align 8 %3, i8 0, i64 8, i1 false)
  %4 = alloca ptr, align 8
    #dbg_declare(ptr %4, !97, !DIExpression(), !100)
  call void @llvm.memset.p0.i64(ptr align 8 %4, i8 0, i64 8, i1 false)
  store ptr %0, ptr %3, align 8, !dbg !101
  store ptr %1, ptr %4, align 8, !dbg !101
  call swiftcc void @swift_defaultActor_initialize(ptr %1) #1, !dbg !102
  %5 = call ptr @swift_retain(ptr returned %0) #3, !dbg !104
  %6 = getelementptr inbounds nuw %T16AsyncActorGetter10AAGHandlerC, ptr %1, i32 0, i32 2, !dbg !105
  store ptr %0, ptr %6, align 16, !dbg !105
  call void @swift_release(ptr %0) #1, !dbg !106
  ret ptr %1, !dbg !106
}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: write)
declare void @llvm.memset.p0.i64(ptr writeonly captures(none), i8, i64, i1 immarg) #4

; Function Attrs: nounwind
declare swiftcc void @swift_defaultActor_initialize(ptr) #1

; Function Attrs: noinline
define hidden swifttailcc void @"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaF"(ptr swiftasync %0, ptr swiftself %1) #5 !dbg !107 {
  %3 = alloca ptr, align 8
    #dbg_declare(ptr %3, !113, !DIExpression(DW_OP_deref, DW_OP_plus_uconst, 72), !114)
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
  store ptr %1, ptr %6, align 8, !dbg !115
  call void asm sideeffect "", "r"(ptr %6), !dbg !116
  %10 = ptrtoint ptr %1 to i64, !dbg !114
  %11 = load ptr, ptr %5, align 8, !dbg !114
  %12 = load ptr, ptr %5, align 8, !dbg !114
  musttail call swifttailcc void @swift_task_switch(ptr swiftasync %12, ptr @"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaFTY0_", i64 %10, i64 0) #1, !dbg !114
  ret void, !dbg !114
}

; Function Attrs: noinline
define hidden swifttailcc void @"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaFTY0_"(ptr swiftasync %0) #6 !dbg !118 {
    #dbg_declare(ptr %0, !121, !DIExpression(DW_OP_LLVM_entry_value, 1, DW_OP_plus_uconst, 72), !122)
  %2 = getelementptr inbounds i8, ptr %0, i32 16
  %3 = getelementptr inbounds %"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaF.Frame", ptr %2, i32 0, i32 2
  %4 = getelementptr inbounds %"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaF.Frame", ptr %2, i32 0, i32 3
  %5 = getelementptr inbounds %"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaF.Frame", ptr %2, i32 0, i32 0
  %6 = getelementptr inbounds %"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaF.Frame", ptr %2, i32 0, i32 1
  %7 = getelementptr inbounds %"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaF.Frame", ptr %2, i32 0, i32 4
  %8 = load ptr, ptr %7, align 8
  store ptr %0, ptr %3, align 8
  %9 = getelementptr inbounds nuw %T16AsyncActorGetter10AAGHandlerC, ptr %8, i32 0, i32 2, !dbg !123
  call void @llvm.lifetime.start.p0(i64 -1, ptr %5), !dbg !123
  call void @swift_beginAccess(ptr %9, ptr %5, i64 32, ptr null) #1, !dbg !123
  %10 = load ptr, ptr %9, align 16, !dbg !123
  %11 = call ptr @swift_retain(ptr returned %10) #3, !dbg !123
  call void @swift_endAccess(ptr %5) #1, !dbg !123
  call void @llvm.lifetime.end.p0(i64 -1, ptr %5), !dbg !123
  %12 = getelementptr inbounds nuw %T16AsyncActorGetter8AAGUtilsC, ptr %10, i32 0, i32 1, !dbg !124
  call void @llvm.lifetime.start.p0(i64 -1, ptr %6), !dbg !124
  call void @swift_beginAccess(ptr %12, ptr %6, i64 32, ptr null) #1, !dbg !124
  %13 = getelementptr inbounds nuw %TSi, ptr %12, i32 0, i32 0, !dbg !124
  %14 = load i64, ptr %13, align 8, !dbg !124
  call void @swift_endAccess(ptr %6) #1, !dbg !125
  call void @llvm.lifetime.end.p0(i64 -1, ptr %6), !dbg !125
  call void @swift_release(ptr %10) #1, !dbg !126
  call void asm sideeffect "", "r"(ptr %4), !dbg !126
  %15 = load ptr, ptr %3, align 8, !dbg !126
  %16 = getelementptr inbounds nuw <{ ptr, ptr }>, ptr %15, i32 0, i32 1, !dbg !126
  %17 = load ptr, ptr %16, align 8, !dbg !126
  %18 = load ptr, ptr %3, align 8, !dbg !126
  musttail call swifttailcc void %17(ptr swiftasync %18, i64 %14) #1, !dbg !126
  ret void, !dbg !126
}

; Function Attrs: nounwind
declare swifttailcc void @swift_task_switch(ptr, ptr, i64, i64) #1

; Function Attrs: nounwind
declare ptr @llvm.swift.async.context.addr() #1

; Function Attrs: noinline
define swifttailcc void @"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF"(ptr swiftasync %0) #5 !dbg !127 {
  %2 = alloca ptr, align 8
    #dbg_declare(ptr %2, !132, !DIExpression(DW_OP_deref, DW_OP_plus_uconst, 24), !134)
  store ptr %0, ptr %2, align 8
  %3 = getelementptr inbounds i8, ptr %0, i32 16
  %4 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %3, i32 0, i32 0
  %5 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %3, i32 0, i32 1
  store ptr %0, ptr %4, align 8
  call void @llvm.memset.p0.i64(ptr align 8 %5, i8 0, i64 8, i1 false)
  %6 = call swiftcc %swift.metadata_response @"$sScMMa"(i64 0) #11, !dbg !135
  %7 = extractvalue %swift.metadata_response %6, 0, !dbg !135
  %8 = call swiftcc ptr @"$sScM6sharedScMvgZ"(ptr swiftself %7), !dbg !135
  %9 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %3, i32 0, i32 2, !dbg !136
  store ptr %8, ptr %9, align 8, !dbg !136
  %10 = call ptr @"$sS2cMScAsWl"() #11, !dbg !136
  %11 = call swiftcc { i64, i64 } @"$sScA15unownedExecutorScevgTj"(ptr swiftself %8, ptr %7, ptr %10), !dbg !136
  %12 = extractvalue { i64, i64 } %11, 0, !dbg !136
  %13 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %3, i32 0, i32 3, !dbg !136
  store i64 %12, ptr %13, align 8, !dbg !136
  %14 = extractvalue { i64, i64 } %11, 1, !dbg !136
  %15 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %3, i32 0, i32 4, !dbg !136
  store i64 %14, ptr %15, align 8, !dbg !136
  %16 = load ptr, ptr %4, align 8, !dbg !136
  %17 = load ptr, ptr %4, align 8, !dbg !136
  musttail call swifttailcc void @swift_task_switch(ptr swiftasync %17, ptr @"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaFTY0_", i64 %12, i64 %14) #1, !dbg !136
  ret void, !dbg !136
}

; Function Attrs: noinline
define hidden swifttailcc void @"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaFTY0_"(ptr swiftasync %0) #6 !dbg !137 {
    #dbg_declare(ptr %0, !139, !DIExpression(DW_OP_LLVM_entry_value, 1, DW_OP_plus_uconst, 24), !141)
  %2 = getelementptr inbounds i8, ptr %0, i32 16
  %3 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %2, i32 0, i32 0
  %4 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %2, i32 0, i32 1
  store ptr %0, ptr %3, align 8
  %5 = call swiftcc %swift.metadata_response @"$s16AsyncActorGetter10AAGHandlerCMa"(i64 0) #11, !dbg !142
  %6 = extractvalue %swift.metadata_response %5, 0, !dbg !142
  %7 = call swiftcc %swift.metadata_response @"$s16AsyncActorGetter8AAGUtilsCMa"(i64 0) #11, !dbg !144
  %8 = extractvalue %swift.metadata_response %7, 0, !dbg !144
  %9 = call swiftcc ptr @"$s16AsyncActorGetter8AAGUtilsCACycfC"(ptr swiftself %8), !dbg !144
  %10 = call swiftcc ptr @"$s16AsyncActorGetter10AAGHandlerC5utilsAcA8AAGUtilsC_tcfC"(ptr %9, ptr swiftself %6), !dbg !142
  %11 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %2, i32 0, i32 5, !dbg !145
  store ptr %10, ptr %11, align 8, !dbg !145
  store ptr %10, ptr %4, align 8, !dbg !145
  call void asm sideeffect "", "r"(ptr %4), !dbg !146
  %12 = load i32, ptr getelementptr inbounds nuw (%swift.async_func_pointer, ptr @"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaFTu", i32 0, i32 1), align 8, !dbg !148
  %13 = zext i32 %12 to i64, !dbg !148
  %14 = call swiftcc ptr @swift_task_alloc(i64 %13) #10, !dbg !148
  %15 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %2, i32 0, i32 6, !dbg !148
  store ptr %14, ptr %15, align 8, !dbg !148
  call void @llvm.lifetime.start.p0(i64 -1, ptr %14), !dbg !148
  %16 = load ptr, ptr %3, align 8, !dbg !148
  %17 = getelementptr inbounds nuw <{ ptr, ptr }>, ptr %14, i32 0, i32 0, !dbg !148
  store ptr %16, ptr %17, align 8, !dbg !148
  %18 = getelementptr inbounds nuw <{ ptr, ptr }>, ptr %14, i32 0, i32 1, !dbg !148
  store ptr @"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaFTQ1_", ptr %18, align 8, !dbg !148
  musttail call swifttailcc void @"$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaF"(ptr swiftasync %14, ptr swiftself %10) #1, !dbg !148
  ret void, !dbg !148
}

; Function Attrs: noinline
define hidden swifttailcc void @"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaFTQ1_"(ptr swiftasync %0, i64 %1) #7 !dbg !149 {
    #dbg_declare(ptr %0, !151, !DIExpression(DW_OP_LLVM_entry_value, 1, DW_OP_deref, DW_OP_plus_uconst, 24), !153)
  %3 = load ptr, ptr %0, align 8, !dbg !154
  %4 = call ptr @llvm.swift.async.context.addr(), !dbg !154
  store ptr %3, ptr %4, align 8, !dbg !154
  %5 = getelementptr inbounds i8, ptr %3, i32 16
  %6 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %5, i32 0, i32 0
  %7 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %5, i32 0, i32 1
  %8 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %5, i32 0, i32 6
  %9 = load ptr, ptr %8, align 8
  %10 = load ptr, ptr %0, align 8
  store ptr %10, ptr %6, align 8
  call swiftcc void @swift_task_dealloc(ptr %9) #10, !dbg !154
  call void @llvm.lifetime.end.p0(i64 -1, ptr %9), !dbg !154
  %11 = load ptr, ptr %6, align 8, !dbg !154
  %12 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %5, i32 0, i32 4, !dbg !154
  %13 = load i64, ptr %12, align 8, !dbg !154
  %14 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %5, i32 0, i32 3, !dbg !154
  %15 = load i64, ptr %14, align 8, !dbg !154
  musttail call swifttailcc void @swift_task_switch(ptr swiftasync %11, ptr @"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaFTY2_", i64 %15, i64 %13) #1, !dbg !154
  ret void, !dbg !154
}

; Function Attrs: noinline
define hidden swifttailcc void @"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaFTY2_"(ptr swiftasync %0) #6 !dbg !155 {
    #dbg_declare(ptr %0, !157, !DIExpression(DW_OP_LLVM_entry_value, 1, DW_OP_plus_uconst, 24), !159)
  %2 = getelementptr inbounds i8, ptr %0, i32 16
  %3 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %2, i32 0, i32 0
  %4 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %2, i32 0, i32 1
  %5 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %2, i32 0, i32 5
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr inbounds %"$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF.Frame", ptr %2, i32 0, i32 2
  %8 = load ptr, ptr %7, align 8
  store ptr %0, ptr %3, align 8
  call void @swift_release(ptr %8) #1, !dbg !160
  call void @swift_release(ptr %6) #1, !dbg !160
  call void asm sideeffect "", "r"(ptr %4), !dbg !160
  %9 = load ptr, ptr %3, align 8, !dbg !160
  %10 = getelementptr inbounds nuw <{ ptr, ptr }>, ptr %9, i32 0, i32 1, !dbg !160
  %11 = load ptr, ptr %10, align 8, !dbg !160
  %12 = load ptr, ptr %3, align 8, !dbg !160
  musttail call swifttailcc void %11(ptr swiftasync %12) #1, !dbg !160
  ret void, !dbg !160
}

declare swiftcc %swift.metadata_response @"$sScMMa"(i64) #2

declare swiftcc ptr @"$sScM6sharedScMvgZ"(ptr swiftself) #2

declare swiftcc { i64, i64 } @"$sScA15unownedExecutorScevgTj"(ptr swiftself, ptr, ptr) #2

; Function Attrs: noinline nounwind memory(none)
define weak_odr hidden ptr @"$sS2cMScAsWl"() #8 !dbg !161 {
  %1 = load ptr, ptr @"$sS2cMScAsWL", align 8, !dbg !164
  %2 = icmp eq ptr %1, null, !dbg !164
  br i1 %2, label %3, label %8, !dbg !164

3:                                                ; preds = %0
  %4 = call swiftcc %swift.metadata_response @"$sScMMa"(i64 255) #11, !dbg !164
  %5 = extractvalue %swift.metadata_response %4, 0, !dbg !164
  %6 = extractvalue %swift.metadata_response %4, 1, !dbg !164
  %7 = call ptr @swift_getWitnessTable(ptr @"$sScMScAsMc", ptr %5, ptr undef) #9, !dbg !164
  store atomic ptr %7, ptr @"$sS2cMScAsWL" release, align 8, !dbg !164
  br label %8, !dbg !164

8:                                                ; preds = %3, %0
  %9 = phi ptr [ %1, %0 ], [ %7, %3 ], !dbg !164
  ret ptr %9, !dbg !164
}

; Function Attrs: nounwind memory(read)
declare ptr @swift_getWitnessTable(ptr, ptr, ptr) #9

; Function Attrs: noinline nounwind memory(none)
define hidden swiftcc %swift.metadata_response @"$s16AsyncActorGetter10AAGHandlerCMa"(i64 %0) #8 !dbg !165 {
  %2 = call ptr @objc_opt_self(ptr getelementptr inbounds (<{ ptr, ptr, ptr, i64, ptr, ptr, ptr, i64, i32, i32, i32, i16, i16, i32, i32, ptr, ptr, i64, i64, ptr, ptr, ptr, ptr, ptr }>, ptr @"$s16AsyncActorGetter10AAGHandlerCMf", i32 0, i32 3)) #1, !dbg !166
  %3 = insertvalue %swift.metadata_response undef, ptr %2, 0, !dbg !166
  %4 = insertvalue %swift.metadata_response %3, i64 0, 1, !dbg !166
  ret %swift.metadata_response %4, !dbg !166
}

; Function Attrs: noinline nounwind memory(none)
define hidden swiftcc %swift.metadata_response @"$s16AsyncActorGetter8AAGUtilsCMa"(i64 %0) #8 !dbg !167 {
  %2 = call ptr @objc_opt_self(ptr getelementptr inbounds (<{ ptr, ptr, ptr, i64, ptr, ptr, ptr, i64, i32, i32, i32, i16, i16, i32, i32, ptr, ptr, i64, ptr }>, ptr @"$s16AsyncActorGetter8AAGUtilsCMf", i32 0, i32 3)) #1, !dbg !168
  %3 = insertvalue %swift.metadata_response undef, ptr %2, 0, !dbg !168
  %4 = insertvalue %swift.metadata_response %3, i64 0, 1, !dbg !168
  ret %swift.metadata_response %4, !dbg !168
}

; Function Attrs: nounwind memory(inaccessiblemem: readwrite)
declare swiftcc ptr @swift_task_alloc(i64) #10

; Function Attrs: nounwind memory(inaccessiblemem: readwrite)
declare swiftcc void @swift_task_dealloc(ptr) #10

; Function Attrs: nounwind
declare ptr @objc_opt_self(ptr) #1

attributes #0 = { nocallback nofree nosync nounwind willreturn memory(argmem: readwrite) }
attributes #1 = { nounwind }
attributes #2 = { "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" }
attributes #3 = { nounwind willreturn }
attributes #4 = { nocallback nofree nounwind willreturn memory(argmem: write) }
attributes #5 = { noinline "async_entry" "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" }
attributes #6 = { noinline "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" }
attributes #7 = { noinline "async_ret" "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" }
attributes #8 = { noinline nounwind memory(none) "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" }
attributes #9 = { nounwind memory(read) }
attributes #10 = { nounwind memory(inaccessiblemem: readwrite) }
attributes #11 = { nounwind memory(none) }

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
!62 = distinct !DISubprogram(name: "init", linkageName: "$s16AsyncActorGetter8AAGUtilsCACycfC", scope: !63, file: !19, line: 35, type: !64, scopeLine: 35, spFlags: DISPFlagDefinition, unit: !18, declaration: !67)
!63 = !DICompositeType(tag: DW_TAG_structure_type, name: "AAGUtils", scope: !22, file: !19, size: 64, runtimeLang: DW_LANG_Swift, identifier: "$s16AsyncActorGetter8AAGUtilsCD")
!64 = !DISubroutineType(types: !65)
!65 = !{!63, !66}
!66 = !DICompositeType(tag: DW_TAG_structure_type, name: "$s16AsyncActorGetter8AAGUtilsCXMTD", flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$s16AsyncActorGetter8AAGUtilsCXMTD")
!67 = !DISubprogram(name: "init", linkageName: "$s16AsyncActorGetter8AAGUtilsCACycfC", scope: !63, file: !19, line: 35, type: !64, scopeLine: 35, spFlags: 0)
!68 = !DILocation(line: 0, scope: !62)
!69 = distinct !DISubprogram(name: "init", linkageName: "$s16AsyncActorGetter8AAGUtilsCACycfc", scope: !63, file: !19, line: 35, type: !70, scopeLine: 35, spFlags: DISPFlagDefinition, unit: !18, declaration: !72, retainedNodes: !73)
!70 = !DISubroutineType(types: !71)
!71 = !{!63, !63}
!72 = !DISubprogram(name: "init", linkageName: "$s16AsyncActorGetter8AAGUtilsCACycfc", scope: !63, file: !19, line: 35, type: !70, scopeLine: 35, spFlags: 0)
!73 = !{!74}
!74 = !DILocalVariable(name: "self", arg: 1, scope: !69, file: !19, line: 35, type: !75, flags: DIFlagArtificial)
!75 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !63)
!76 = !DILocation(line: 35, column: 13, scope: !69)
!77 = !DILocation(line: 0, scope: !69)
!78 = !DILocation(line: 36, column: 7, scope: !79)
!79 = distinct !DILexicalBlock(scope: !80, file: !19, line: 36, column: 7)
!80 = distinct !DILexicalBlock(scope: !69, file: !19, line: 35, column: 22)
!81 = !DILocation(line: 36, column: 22, scope: !82)
!82 = distinct !DILexicalBlock(scope: !80, file: !19, line: 36, column: 22)
!83 = !DILocation(line: 0, scope: !82)
!84 = distinct !DISubprogram(name: "init", linkageName: "$s16AsyncActorGetter10AAGHandlerC5utilsAcA8AAGUtilsC_tcfC", scope: !85, file: !19, line: 42, type: !86, scopeLine: 42, spFlags: DISPFlagDefinition, unit: !18, declaration: !89)
!85 = !DICompositeType(tag: DW_TAG_structure_type, name: "AAGHandler", scope: !22, file: !19, size: 64, runtimeLang: DW_LANG_Swift, identifier: "$s16AsyncActorGetter10AAGHandlerCD")
!86 = !DISubroutineType(types: !87)
!87 = !{!85, !63, !88}
!88 = !DICompositeType(tag: DW_TAG_structure_type, name: "$s16AsyncActorGetter10AAGHandlerCXMTD", flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$s16AsyncActorGetter10AAGHandlerCXMTD")
!89 = !DISubprogram(name: "init", linkageName: "$s16AsyncActorGetter10AAGHandlerC5utilsAcA8AAGUtilsC_tcfC", scope: !85, file: !19, line: 42, type: !86, scopeLine: 42, spFlags: 0)
!90 = !DILocation(line: 0, scope: !84)
!91 = distinct !DISubprogram(name: "init", linkageName: "$s16AsyncActorGetter10AAGHandlerC5utilsAcA8AAGUtilsC_tcfc", scope: !85, file: !19, line: 42, type: !92, scopeLine: 42, spFlags: DISPFlagDefinition, unit: !18, declaration: !94, retainedNodes: !95)
!92 = !DISubroutineType(types: !93)
!93 = !{!85, !63, !85}
!94 = !DISubprogram(name: "init", linkageName: "$s16AsyncActorGetter10AAGHandlerC5utilsAcA8AAGUtilsC_tcfc", scope: !85, file: !19, line: 42, type: !92, scopeLine: 42, spFlags: 0)
!95 = !{!96, !97}
!96 = !DILocalVariable(name: "utils", arg: 1, scope: !91, file: !19, line: 42, type: !75)
!97 = !DILocalVariable(name: "self", arg: 2, scope: !91, file: !19, line: 42, type: !98, flags: DIFlagArtificial)
!98 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !85)
!99 = !DILocation(line: 42, column: 8, scope: !91)
!100 = !DILocation(line: 42, column: 3, scope: !91)
!101 = !DILocation(line: 0, scope: !91)
!102 = !DILocation(line: 0, scope: !103)
!103 = !DILexicalBlockFile(scope: !91, discriminator: 0)
!104 = !DILocation(line: 43, column: 18, scope: !91)
!105 = !DILocation(line: 43, column: 16, scope: !91)
!106 = !DILocation(line: 44, column: 3, scope: !91)
!107 = distinct !DISubprogram(name: "read_utils_no_cycle_good_FP", linkageName: "$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaF", scope: !85, file: !19, line: 46, type: !108, scopeLine: 46, spFlags: DISPFlagDefinition, unit: !18, declaration: !111, retainedNodes: !112)
!108 = !DISubroutineType(types: !109)
!109 = !{!110, !85}
!110 = !DICompositeType(tag: DW_TAG_structure_type, name: "Int", scope: !24, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$sSiD")
!111 = !DISubprogram(name: "read_utils_no_cycle_good_FP", linkageName: "$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaF", scope: !85, file: !19, line: 46, type: !108, scopeLine: 46, spFlags: 0)
!112 = !{!113}
!113 = !DILocalVariable(name: "self", arg: 1, scope: !107, file: !19, line: 46, type: !98, flags: DIFlagArtificial)
!114 = !DILocation(line: 46, column: 8, scope: !107)
!115 = !DILocation(line: 0, scope: !107)
!116 = !DILocation(line: 0, scope: !117)
!117 = !DILexicalBlockFile(scope: !107, discriminator: 0)
!118 = distinct !DISubprogram(name: "read_utils_no_cycle_good_FP", linkageName: "$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaFTY0_", scope: !85, file: !19, line: 46, type: !108, scopeLine: 47, spFlags: DISPFlagDefinition, unit: !18, declaration: !119, retainedNodes: !120)
!119 = !DISubprogram(name: "read_utils_no_cycle_good_FP", linkageName: "$s16AsyncActorGetter10AAGHandlerC27read_utils_no_cycle_good_FPSiyYaFTY0_", scope: !85, file: !19, line: 46, type: !108, scopeLine: 46, spFlags: 0)
!120 = !{!121}
!121 = !DILocalVariable(name: "self", arg: 1, scope: !118, file: !19, line: 46, type: !98, flags: DIFlagArtificial)
!122 = !DILocation(line: 46, column: 8, scope: !118)
!123 = !DILocation(line: 47, column: 12, scope: !118)
!124 = !DILocation(line: 47, column: 18, scope: !118)
!125 = !DILocation(line: 0, scope: !118)
!126 = !DILocation(line: 47, column: 5, scope: !118)
!127 = distinct !DISubprogram(name: "test_actor_property_access_no_cycle_good_FP", linkageName: "$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaF", scope: !22, file: !19, line: 52, type: !128, scopeLine: 52, spFlags: DISPFlagDefinition, unit: !18, retainedNodes: !131)
!128 = !DISubroutineType(types: !129)
!129 = !{!130}
!130 = !DICompositeType(tag: DW_TAG_structure_type, name: "$sytD", flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$sytD")
!131 = !{!132}
!132 = !DILocalVariable(name: "h", scope: !133, file: !19, line: 53, type: !98)
!133 = distinct !DILexicalBlock(scope: !127, file: !19, line: 53, column: 7)
!134 = !DILocation(line: 53, column: 7, scope: !133)
!135 = !DILocation(line: 0, scope: !127)
!136 = !DILocation(line: 52, column: 13, scope: !127)
!137 = distinct !DISubprogram(name: "test_actor_property_access_no_cycle_good_FP", linkageName: "$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaFTY0_", scope: !22, file: !19, line: 52, type: !128, scopeLine: 53, spFlags: DISPFlagDefinition, unit: !18, retainedNodes: !138)
!138 = !{!139}
!139 = !DILocalVariable(name: "h", scope: !140, file: !19, line: 53, type: !98)
!140 = distinct !DILexicalBlock(scope: !137, file: !19, line: 53, column: 7)
!141 = !DILocation(line: 53, column: 7, scope: !140)
!142 = !DILocation(line: 53, column: 11, scope: !143)
!143 = distinct !DILexicalBlock(scope: !137, file: !19, line: 53, column: 11)
!144 = !DILocation(line: 53, column: 29, scope: !143)
!145 = !DILocation(line: 0, scope: !140)
!146 = !DILocation(line: 0, scope: !147)
!147 = !DILexicalBlockFile(scope: !140, discriminator: 0)
!148 = !DILocation(line: 54, column: 15, scope: !140)
!149 = distinct !DISubprogram(name: "test_actor_property_access_no_cycle_good_FP", linkageName: "$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaFTQ1_", scope: !22, file: !19, line: 52, type: !128, scopeLine: 54, spFlags: DISPFlagDefinition, unit: !18, retainedNodes: !150)
!150 = !{!151}
!151 = !DILocalVariable(name: "h", scope: !152, file: !19, line: 53, type: !98)
!152 = distinct !DILexicalBlock(scope: !149, file: !19, line: 53, column: 7)
!153 = !DILocation(line: 53, column: 7, scope: !152)
!154 = !DILocation(line: 54, column: 15, scope: !152)
!155 = distinct !DISubprogram(name: "test_actor_property_access_no_cycle_good_FP", linkageName: "$s16AsyncActorGetter43test_actor_property_access_no_cycle_good_FPyyYaFTY2_", scope: !22, file: !19, line: 52, type: !128, scopeLine: 55, spFlags: DISPFlagDefinition, unit: !18, retainedNodes: !156)
!156 = !{!157}
!157 = !DILocalVariable(name: "h", scope: !158, file: !19, line: 53, type: !98)
!158 = distinct !DILexicalBlock(scope: !155, file: !19, line: 53, column: 7)
!159 = !DILocation(line: 53, column: 7, scope: !158)
!160 = !DILocation(line: 55, column: 1, scope: !158)
!161 = distinct !DISubprogram(linkageName: "$sS2cMScAsWl", scope: !22, file: !162, type: !163, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !18)
!162 = !DIFile(filename: "<compiler-generated>", directory: "")
!163 = !DISubroutineType(types: null)
!164 = !DILocation(line: 0, scope: !161)
!165 = distinct !DISubprogram(linkageName: "$s16AsyncActorGetter10AAGHandlerCMa", scope: !22, file: !162, type: !163, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !18)
!166 = !DILocation(line: 0, scope: !165)
!167 = distinct !DISubprogram(linkageName: "$s16AsyncActorGetter8AAGUtilsCMa", scope: !22, file: !162, type: !163, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !18)
!168 = !DILocation(line: 0, scope: !167)
