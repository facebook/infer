; Copyright (c) Facebook, Inc. and its affiliates.
;
; This source code is licensed under the MIT license found in the
; LICENSE file in the root directory of this source tree.
;
; Minimal Pulse repro for the `load %self` -> metadata-stored-as-self FP.
;
; Source-level intent:
;
;     // module M
;     class Outer {
;         var c: AnyObject?
;         func setupBad() {
;             let ctx = makeBox()              // swift_allocObject({hdr, ptr})
;             ctx.field_1 = self_metadata      // LLVM `load ptr, ptr %self`
;                                              //   reads first 8 bytes of *self
;                                              //   (the swift_refcounted metadata)
;             self.c = ctx                     // the one real strong edge
;         }
;     }
;
; Runtime: no cycle. ctx.field_1 holds the class metadata pointer; it is
; not a strong reference to a heap object.
;
; Buggy translation: with self's Swift class type recovered via debug
; info, `load ptr, ptr %self` translates as a single Textual
; `n = *&var_self`, which gives the parameter value (the `self` pointer)
; rather than the bytes at `*self` (the metadata). The subsequent store
; records `ctx.field_1 := self` in SIL. Pulse's cycle checker then closes
; a phantom cycle:
;     self -> self.c (= ctx) -> ctx.field_1 (= self) cycle
;
; Until the load-translation / cycle-checker is fixed, Pulse reports
; this as RETAIN_CYCLE on `setupBad`. This file pins that FP.

target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%T1M5OuterC = type { ptr, ptr }
%swift.refcounted = type { ptr, i64 }
%TBox = type <{ %swift.refcounted, ptr }>

@"$s1M5OuterC1cyXlSgvpWvd" = constant i64 16, align 8

declare ptr @swift_allocObject(ptr, i64, i64) #0

define swiftcc void @"$s1M5OuterC8setupBadyyF"(ptr swiftself %0) #0 !dbg !6 {
entry:
  %self.debug = alloca ptr, align 8
    #dbg_declare(ptr %self.debug, !20, !DIExpression(), !21)
  store ptr %0, ptr %self.debug, align 8, !dbg !12

  ; LLVM intent: read the swift_refcounted metadata pointer at *self.
  %meta = load ptr, ptr %0, align 8, !dbg !12

  ; Allocate a small box (a partial-apply-shaped context).
  %ctx = call ptr @swift_allocObject(ptr null, i64 24, i64 7), !dbg !12

  ; ctx.field_1 = metadata  (LLVM intent; buggy SIL records `:= self`).
  %f1 = getelementptr inbounds nuw %TBox, ptr %ctx, i32 0, i32 1, !dbg !12
  store ptr %meta, ptr %f1, align 8, !dbg !12

  ; self.c = ctx  (the one real strong edge).
  %off = load i64, ptr @"$s1M5OuterC1cyXlSgvpWvd", align 8, !dbg !13
  %c_slot = getelementptr i8, ptr %0, i64 %off, !dbg !13
  store ptr %ctx, ptr %c_slot, align 8, !dbg !13

  ret void, !dbg !14
}

attributes #0 = { nounwind }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2, !3}

!0 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !1, producer: "swift", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
!1 = !DIFile(filename: "infer/tests/codetoanalyze/swift/bitcode-handcrafted-pulse/rc_self_metadata_load_fp.swift", directory: ".")
!2 = !{i32 7, !"Dwarf Version", i32 4}
!3 = !{i32 2, !"Debug Info Version", i32 3}
!6 = distinct !DISubprogram(name: "setupBad", scope: !1, file: !1, line: 10, type: !7, scopeLine: 10, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !19)
!7 = !DISubroutineType(types: !8)
!8 = !{null}
!12 = !DILocation(line: 12, column: 5, scope: !6)
!13 = !DILocation(line: 13, column: 5, scope: !6)
!14 = !DILocation(line: 14, column: 5, scope: !6)
!18 = !DICompositeType(tag: DW_TAG_structure_type, name: "Outer", scope: !1, file: !1, size: 64, runtimeLang: DW_LANG_Swift, identifier: "$s1M5OuterCD")
!19 = !{!20}
!20 = !DILocalVariable(name: "self", arg: 1, scope: !6, file: !1, line: 10, type: !22, flags: DIFlagArtificial)
!21 = !DILocation(line: 10, column: 3, scope: !6)
!22 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !18)
