; Copyright (c) Facebook, Inc. and its affiliates.
;
; This source code is licensed under the MIT license found in the
; LICENSE file in the root directory of this source tree.
;
; Pulse end-to-end test pinning that a retain cycle through a
; cross-module strong stored property is reported when the receiving
; class's layout is invisible to the consumer's bitcode.
;
; Source-level intent:
;
;     // Module `Definer` (compiled separately, NOT in this bitcode):
;     class Inner {
;         var delegate: AnyObject?   // strong, NOT [weak]
;     }
;
;     // Module `Consumer` (this bitcode):
;     class Outer {
;         let inner: Inner
;         init(inner: Inner) {
;             self.inner = inner
;             self.inner.delegate = self     // strong store, closes cycle
;         }
;     }
;
; Cycle: [self -> inner -> delegate -> self].
;
; The companion fixture `rc_cross_module_weak_field.swift.ll` pins the
; FP scenario where the cross-module property is declared `weak` in the
; defining module and the consumer's view (no `swift::weak<T>` wrapper)
; lets the strong/weak classification slip — the desired behaviour
; (no report) is not yet reachable.
;
; This fixture pins the COMPLEMENTARY happy-path scenario: when the
; cross-module property is strong, the cycle MUST be reported. Together
; the pair brackets the cross-module property-handling logic:
;   - `weak` cross-module field    -> false positive (today)
;   - `strong` cross-module field  -> true positive  (this file)
;
; Trigger machinery:
;   - Consumer sees Inner only as the 16-byte Swift class header
;     (`%T7Definer5InnerC = type { ptr, ptr }`), so
;     `Llair2TextualType.to_textual_field_decls` emits Inner with zero
;     user fields.
;   - The extern `Inner.delegate` Wvd carries the field's identity in
;     its mangled name. `Llair2TextualGlobals.process_wvd_globals`
;     (D105684482, defended for externs) reads that and APPENDS a
;     `delegate` field decl to Inner's struct.
;   - The store at line 14 lowers via the DynamicWvd path so the
;     emitted Field expression references `Inner.delegate` directly
;     (matching the just-added field decl). No wildcard rewrite; the
;     cycle closes cleanly.

target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Cross-module Inner: Consumer sees only the Swift class header. The
; `delegate` field is published by the extern Wvd below.
%T7Definer5InnerC = type { ptr, ptr }

; In-module Outer: header + one `inner` ptr at offset 16.
%T8Consumer5OuterC = type { ptr, ptr, ptr }

; Cross-module Wvd: external constant. Mangled name encodes
; (Definer.Inner, delegate). Used by `process_wvd_globals` to name
; the field on Inner and by the DynamicWvd lowering to point the
; store at the right slot.
@"$s7Definer5InnerC8delegateAA0bC5OuterCSgvpWvd" = external constant i64

; In-module Wvd: constant 16, names (Consumer.Outer, inner).
@"$s8Consumer5OuterC5innerAA7Definer5InnerCvpWvd" = constant i64 16, align 8

%swift.metadata_response = type { ptr, i64 }

declare swiftcc %swift.metadata_response @"$s7Definer5InnerCMa"(i64) #0
declare ptr @swift_allocObject(ptr, i64, i64) #0

define swiftcc void @"$s8Consumer5OuterC8setupBad5inneryAA7Definer5InnerC_tF"(
    ptr %inner, ptr swiftself %self) !dbg !6 {
entry:
  ; self.inner = inner            (back-edge of cycle, in-module Wvd)
  %off_inner = load i64, ptr @"$s8Consumer5OuterC5innerAA7Definer5InnerCvpWvd", align 8, !dbg !12
  %slot_inner = getelementptr i8, ptr %self, i64 %off_inner, !dbg !12
  store ptr %inner, ptr %slot_inner, align 8, !dbg !12

  ; self.inner.delegate = self    (forward edge, strong store, extern Wvd)
  %off_del = load i64, ptr @"$s7Definer5InnerC8delegateAA0bC5OuterCSgvpWvd", align 8, !dbg !13
  %slot_del = getelementptr i8, ptr %inner, i64 %off_del, !dbg !13
  store ptr %self, ptr %slot_del, align 8, !dbg !13

  ret void, !dbg !14
}

attributes #0 = { nounwind }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2, !3}

!0 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !1, producer: "swift", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
!1 = !DIFile(filename: "infer/tests/codetoanalyze/swift/bitcode-handcrafted-pulse/rc_cross_module_strong_field.swift", directory: ".")
!2 = !{i32 7, !"Dwarf Version", i32 4}
!3 = !{i32 2, !"Debug Info Version", i32 3}
!6 = distinct !DISubprogram(name: "setupBad", scope: !1, file: !1, line: 11, type: !7, scopeLine: 11, spFlags: DISPFlagDefinition, unit: !0)
!7 = !DISubroutineType(types: !8)
!8 = !{null}
!12 = !DILocation(line: 13, column: 5, scope: !6)
!13 = !DILocation(line: 14, column: 5, scope: !6)
!14 = !DILocation(line: 15, column: 5, scope: !6)
