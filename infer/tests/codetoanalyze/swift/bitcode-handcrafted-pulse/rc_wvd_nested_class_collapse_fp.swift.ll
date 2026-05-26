; Copyright (c) Facebook, Inc. and its affiliates.
;
; This source code is licensed under the MIT license found in the
; LICENSE file in the root directory of this source tree.
;
; Pulse end-to-end test demonstrating that the
; `Llair2TextualField.extract_class_and_field_from_wvd`
; nested-class-continuation fix *suppresses* a real false-positive
; retain cycle (not merely relabels its trace).
;
; Source-level intent (the BCN sheet-controller init shape, distilled):
;
;     // module M
;     class Outer {
;         class Inner {
;             var placeholderField: AnyObject
;             var attachField: AnyObject
;             var onLinkTap: AnyObject
;             func setupBad(attachment: Attachment, closure: Closure) {
;                 closure.ctx = attachment                  // captures
;                 let placeholder = AnyObject()
;                 self.placeholderField = placeholder
;                 self.attachField = attachment
;                 // BCN's IR pattern: load back from a *different* Wvd
;                 // slot, then store into a subfield of the loaded value.
;                 let loaded = self.placeholderField
;                 loaded.onLinkTap = closure
;             }
;         }
;     }
;
; No real cycle exists here: attachment has no outgoing edges; the
; closure captures attachment but nothing closes the loop.
;
; Under the buggy parser, all three nested-class Wvd descriptors
; (`...M5OuterC5InnerC<field>...vpWvd`) collapse to a single Textual
; field name `Inner` because the parser stops at the first class
; marker. The three stores on `self` therefore overwrite each other
; on the `self.Inner` slot, and the load reads back the last-stored
; value: `attachment`. The subsequent substore writes `closure` onto
; `attachment.onLinkTap`, fabricating a back-edge that doesn't exist
; in source. Combined with the (real) `closure.ctx -> attachment`
; edge, Pulse closes a false cycle
;     `attachment -> onLinkTap -> closure -> ctx -> attachment`.
;
; With the fix, the three Wvds parse to distinct fields. The load
; reads from `self.placeholderField`, which holds `placeholder` (a
; fresh local with no back-refs). The substore lands on
; `placeholder.onLinkTap`. `attachment` has no outgoing edges; no
; cycle.

target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%T1M5OuterC5InnerC = type { ptr, ptr, ptr, ptr, ptr }
%T1M10AttachmentC = type { ptr, ptr }
%T1M7ClosureC = type { ptr, ptr, ptr }

; Three nested-class Wvds. Distinct byte offsets — so at LLVM level the
; GEPs target three different slots. Under the buggy parser they all
; parse to field-name "Inner"; under the fix they parse to three
; distinct field names.
@"$s1M5OuterC5InnerC15placeholderFieldyXlvpWvd" = constant i64 16, align 8
@"$s1M5OuterC5InnerC11attachFieldyXlvpWvd" = constant i64 24, align 8
@"$s1M5OuterC5InnerC9onLinkTapyXlvpWvd" = constant i64 32, align 8

; Non-nested-class Wvd for Closure.ctx — single class marker, parses
; correctly under both the buggy and fixed parsers.
@"$s1M7ClosureC3ctxyXlvpWvd" = constant i64 8, align 8

declare ptr @swift_allocObject(ptr, i64, i64) #0

define swiftcc void @"$s1M5OuterC5InnerC8setupBad10attachment7closureyAA10AttachmentC_AA7ClosureCtF"(ptr %attachment, ptr %closure, ptr swiftself %self) !dbg !6 {
entry:
  ; closure.ctx = attachment  (closure captures attachment — real edge)
  %off_ctx = load i64, ptr @"$s1M7ClosureC3ctxyXlvpWvd", align 8, !dbg !12
  %slot_ctx = getelementptr i8, ptr %closure, i64 %off_ctx, !dbg !12
  store ptr %attachment, ptr %slot_ctx, align 8, !dbg !12

  ; placeholder = alloc() — a fresh local with no back-refs
  %placeholder = call ptr @swift_allocObject(ptr null, i64 24, i64 8), !dbg !13

  ; self.placeholderField = placeholder      (Wvd_PH)
  %off1 = load i64, ptr @"$s1M5OuterC5InnerC15placeholderFieldyXlvpWvd", align 8, !dbg !14
  %slot1 = getelementptr i8, ptr %self, i64 %off1, !dbg !14
  store ptr %placeholder, ptr %slot1, align 8, !dbg !14

  ; self.attachField = attachment            (Wvd_ATT — under buggy: overwrites Wvd_PH slot)
  %off2 = load i64, ptr @"$s1M5OuterC5InnerC11attachFieldyXlvpWvd", align 8, !dbg !15
  %slot2 = getelementptr i8, ptr %self, i64 %off2, !dbg !15
  store ptr %attachment, ptr %slot2, align 8, !dbg !15

  ; loaded = *self.placeholderField          (Wvd_PH — under buggy reads attachment; under fix reads placeholder)
  %off3 = load i64, ptr @"$s1M5OuterC5InnerC15placeholderFieldyXlvpWvd", align 8, !dbg !16
  %slot3 = getelementptr i8, ptr %self, i64 %off3, !dbg !16
  %loaded = load ptr, ptr %slot3, align 8, !dbg !16

  ; loaded.onLinkTap = closure               (Wvd_OLT — write closure onto loaded.onLinkTap)
  %off4 = load i64, ptr @"$s1M5OuterC5InnerC9onLinkTapyXlvpWvd", align 8, !dbg !17
  %slot4 = getelementptr i8, ptr %loaded, i64 %off4, !dbg !17
  store ptr %closure, ptr %slot4, align 8, !dbg !17

  ret void, !dbg !18
}

attributes #0 = { nounwind }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2, !3}

!0 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !1, producer: "swift", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
!1 = !DIFile(filename: "infer/tests/codetoanalyze/swift/bitcode-handcrafted-pulse/rc_wvd_nested_class_collapse_fp.swift", directory: ".")
!2 = !{i32 7, !"Dwarf Version", i32 4}
!3 = !{i32 2, !"Debug Info Version", i32 3}
!6 = distinct !DISubprogram(name: "setupBad", scope: !1, file: !1, line: 10, type: !7, scopeLine: 10, spFlags: DISPFlagDefinition, unit: !0)
!7 = !DISubroutineType(types: !8)
!8 = !{null}
!12 = !DILocation(line: 12, column: 5, scope: !6)
!13 = !DILocation(line: 13, column: 5, scope: !6)
!14 = !DILocation(line: 14, column: 5, scope: !6)
!15 = !DILocation(line: 15, column: 5, scope: !6)
!16 = !DILocation(line: 16, column: 5, scope: !6)
!17 = !DILocation(line: 17, column: 5, scope: !6)
!18 = !DILocation(line: 18, column: 5, scope: !6)
