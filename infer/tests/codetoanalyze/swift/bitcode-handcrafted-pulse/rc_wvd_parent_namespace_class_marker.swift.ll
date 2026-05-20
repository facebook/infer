; Copyright (c) Facebook, Inc. and its affiliates.
;
; This source code is licensed under the MIT license found in the
; LICENSE file in the root directory of this source tree.
;
; Pulse end-to-end test pinning what the
; `Llair2TextualField.extract_class_and_field_from_wvd`
; parent-namespace-substitution fix unlocks at the analyzer level.
;
; Source-level intent (a cross-module / namespaced cycle pattern this
; mirrors at IR level):
;
;     // module M
;     class Inner {
;         var delegate: AnyObject?
;     }
;     class Outer {
;         let inner: Inner
;         init(inner: Inner) {
;             self.inner = inner
;             self.inner.delegate = self
;         }
;     }
;
; Cycle: [self -> inner -> delegate -> self].
;
; The receiver class `Inner`'s `delegate` field-offset descriptor (Wvd)
; uses the parent-namespace-substitution shape: the 5-char class name
; "Inner" is followed by "AA" (a back-reference to its parent module)
; and then the class marker "C", before the property segment begins
; (i.e. `$s5InnerAAC8delegateAA0bC8Delegate_pSgvpWvd`).
;
; Without the parser fix that recognises `<length-prefix><name>AA<C/V>`
; as a class designator, `extract_class_and_field_from_wvd` returns
; `(None, "unknown_field")` for this Wvd. The DynamicWvd path in
; `Llair2Textual` then falls back to `undef_exp` and the delegate
; store target becomes `_fun_llvm_nondet()` — the cycle is invisible
; to Pulse. With the fix, the recovered Field store closes the cycle
; and Pulse reports a `RETAIN_CYCLE` on the function.

target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%T5InnerAAC = type { ptr, ptr, ptr }
%T5OuterAAC = type { ptr, ptr, ptr }

@"$s5InnerAAC8delegateAA0bC8Delegate_pSgvpWvd" = constant i64 16, align 8
@"$s5OuterAAC5innerAA0c2erCvpWvd" = constant i64 16, align 8

declare ptr @swift_allocObject(ptr, i64, i64) #0

define swiftcc void @"$s5OuterAAC8setupBad5inneryAA0c2erC_tF"(ptr %inner, ptr swiftself %self) !dbg !6 {
entry:
  ; self.inner = inner  (Outer's own field; Wvd shape is the same AAC
  ;                      pattern as the cycle-closing edge)
  %off1 = load i64, ptr @"$s5OuterAAC5innerAA0c2erCvpWvd", align 8, !dbg !12
  %slot1 = getelementptr i8, ptr %self, i64 %off1, !dbg !12
  store ptr %inner, ptr %slot1, align 8, !dbg !12

  ; self.inner.delegate = self  (Inner's delegate Wvd uses the AAC
  ;                               class-marker shape that this fix unblocks)
  %off2 = load i64, ptr @"$s5InnerAAC8delegateAA0bC8Delegate_pSgvpWvd", align 8, !dbg !13
  %slot2 = getelementptr i8, ptr %inner, i64 %off2, !dbg !13
  store ptr %self, ptr %slot2, align 8, !dbg !13

  ret void, !dbg !14
}

attributes #0 = { nounwind }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2, !3}

!0 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !1, producer: "swift", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
!1 = !DIFile(filename: "infer/tests/codetoanalyze/swift/bitcode-handcrafted-pulse/rc_wvd_parent_namespace_class_marker.swift", directory: ".")
!2 = !{i32 7, !"Dwarf Version", i32 4}
!3 = !{i32 2, !"Debug Info Version", i32 3}
!6 = distinct !DISubprogram(name: "setupBad", scope: !1, file: !1, line: 10, type: !7, scopeLine: 10, spFlags: DISPFlagDefinition, unit: !0)
!7 = !DISubroutineType(types: !8)
!8 = !{null}
!12 = !DILocation(line: 12, column: 5, scope: !6)
!13 = !DILocation(line: 13, column: 5, scope: !6)
!14 = !DILocation(line: 14, column: 5, scope: !6)
