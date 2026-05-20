; Copyright (c) Facebook, Inc. and its affiliates.
;
; This source code is licensed under the MIT license found in the
; LICENSE file in the root directory of this source tree.
;
; Pulse end-to-end test pinning what the [LlvmSledgeFrontend.get_wvd_global]
; cast-peeling fix unlocks at the analyzer level.
;
; Source-level intent (a lazy-var-style pattern this mirrors at IR level):
;
;     class Outer {
;         let inner: Inner                  // strong stored property
;         init(inner: Inner) {
;             self.inner = inner
;             self.inner.delegate = self    // closes a retain cycle when
;                                           // [delegate] is strong
;         }
;     }
;
; Cycle: [self -> inner -> delegate -> self].
;
; This file uses the swiftc lowering of [self.inner.delegate = self] --
; i.e. load the field offset from a Wvd global as i32, sign-extend to i64,
; then [getelementptr i8, ptr %inner, i64 %off64]. Without the cast-peeling
; fix to [get_wvd_global], the GEP offset operand is a [SExt] of a [Load]
; rather than a bare [Load], so [get_wvd_global] returns [None], the GEP
; falls back to [Llair.Exp.nondet], and the delegate-store target is
; [_fun_llvm_nondet()] -- which makes the cycle invisible to Pulse.
;
; With the fix in place this file's analysis emits a [RETAIN_CYCLE]; without
; it, Pulse reports nothing on this function.

target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Class layouts: 16-byte Swift class header followed by one user field at
; byte offset 16.
%T7RcWvdLd5InnerC = type { ptr, ptr, ptr }
%T7RcWvdLd5OuterC = type { ptr, ptr, ptr }

; Wvd descriptors. The [delegate] descriptor is loaded as i32 and
; sign-extended to i64 in the GEP -- that's the lowering shape the fix
; targets. The [inner] descriptor is loaded directly without a cast (the
; existing [DynamicWvd] path, here just to anchor the back-edge so the
; cycle is closed in one function).
@"$s7RcWvdLd5InnerC8delegateAA0bC8Delegate_pSgvpWvd" = constant i32 16, align 4
@"$s7RcWvdLd5OuterC5innerAA0c2erCvpWvd" = constant i64 16, align 8

%swift.metadata_response = type { ptr, i64 }

declare swiftcc %swift.metadata_response @"$s7RcWvdLd5InnerCMa"(i64) #0
declare ptr @swift_allocObject(ptr, i64, i64) #0

define swiftcc void @"$s7RcWvdLd5OuterC8setupBad5inneryAA0c2erC_tF"(ptr %inner, ptr swiftself %self) !dbg !6 {
entry:
  ; self.inner = inner            (constant-Wvd path; back-edge of cycle)
  %off1 = load i64, ptr @"$s7RcWvdLd5OuterC5innerAA0c2erCvpWvd", align 8, !dbg !12
  %slot1 = getelementptr i8, ptr %self, i64 %off1, !dbg !12
  store ptr %inner, ptr %slot1, align 8, !dbg !12

  ; self.inner.delegate = self    (load-then-SExt-then-GEP path; the
  ;                                shape the fix unlocks)
  %off2_32 = load i32, ptr @"$s7RcWvdLd5InnerC8delegateAA0bC8Delegate_pSgvpWvd", align 4, !dbg !13
  %off2_64 = sext i32 %off2_32 to i64, !dbg !13
  %slot2 = getelementptr i8, ptr %inner, i64 %off2_64, !dbg !13
  store ptr %self, ptr %slot2, align 8, !dbg !13

  ret void, !dbg !14
}

attributes #0 = { nounwind }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2, !3}

!0 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !1, producer: "swift", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
!1 = !DIFile(filename: "infer/tests/codetoanalyze/swift/bitcode-handcrafted-pulse/rc_wvd_loaded_field_offset.swift", directory: ".")
!2 = !{i32 7, !"Dwarf Version", i32 4}
!3 = !{i32 2, !"Debug Info Version", i32 3}
!6 = distinct !DISubprogram(name: "setupBad", scope: !1, file: !1, line: 10, type: !7, scopeLine: 10, spFlags: DISPFlagDefinition, unit: !0)
!7 = !DISubroutineType(types: !8)
!8 = !{null}
!12 = !DILocation(line: 12, column: 5, scope: !6)
!13 = !DILocation(line: 13, column: 5, scope: !6)
!14 = !DILocation(line: 14, column: 5, scope: !6)
