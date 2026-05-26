; Copyright (c) Facebook, Inc. and its affiliates.
;
; This source code is licensed under the MIT license found in the
; LICENSE file in the root directory of this source tree.
;
; Pulse end-to-end test pinning what the
; `Llair2TextualField.extract_class_and_field_from_wvd`
; nested-class-continuation fix unlocks at the analyzer level.
;
; Source-level intent (a nested-class cycle pattern this mirrors at IR
; level):
;
;     // module M
;     class Outer {
;         class Inner {
;             var delegate: AnyObject?
;         }
;         let inner: Inner
;         init(inner: Inner) {
;             self.inner = inner
;             self.inner.delegate = self
;         }
;     }
;
; Cycle: [self -> inner -> delegate -> self].
;
; The mangler emits the nested class `Outer.Inner` as
; `<modlen>M5OuterC5InnerC` (two class-marker `C`s, one per nesting
; level). The Wvd descriptor for the nested `Inner.delegate` field
; therefore looks like
;     $s1M5OuterC5InnerC8delegate<type-sig>vpWvd
;
; Before the nested-class-continuation fix, the parser stops at the
; first `C` it sees after the module-level class name, treating the
; remaining `5InnerC8delegate...` as if the inner class name `Inner`
; were the field. The recovered `(class, field)` pair is
; `("T1M5OuterC", "Inner")` — wrong on both axes.
;
; With the fix, the parser detects the second `<digits><name>C/V`
; chunk after the first `C` and extends the class designator through
; it, recovering `("T1M5OuterC5InnerC", "delegate")`. The DynamicWvd
; GEP path in `Llair2Textual` then emits a correctly-typed
; `Field` access whose enclosing class matches the receiver, the
; delegate store target is the right field, and Pulse closes the
; `self -> inner -> delegate -> self` cycle.

target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%T1M5OuterC5InnerC = type { ptr, ptr, ptr }
%T1M5OuterC = type { ptr, ptr, ptr }

@"$s1M5OuterC5InnerC8delegateyXlSgvpWvd" = constant i64 16, align 8
@"$s1M5OuterC5innerAA5InnerCvpWvd" = constant i64 16, align 8

declare ptr @swift_allocObject(ptr, i64, i64) #0

define swiftcc void @"$s1M5OuterC8setupBad5inneryAC5InnerC_tF"(ptr %inner, ptr swiftself %self) !dbg !6 {
entry:
  ; self.inner = inner            (Outer.inner; back-edge of cycle)
  %off1 = load i64, ptr @"$s1M5OuterC5innerAA5InnerCvpWvd", align 8, !dbg !12
  %slot1 = getelementptr i8, ptr %self, i64 %off1, !dbg !12
  store ptr %inner, ptr %slot1, align 8, !dbg !12

  ; self.inner.delegate = self    (Outer.Inner.delegate; this is the
  ;                                nested-class Wvd shape the fix unlocks)
  %off2 = load i64, ptr @"$s1M5OuterC5InnerC8delegateyXlSgvpWvd", align 8, !dbg !13
  %slot2 = getelementptr i8, ptr %inner, i64 %off2, !dbg !13
  store ptr %self, ptr %slot2, align 8, !dbg !13

  ret void, !dbg !14
}

attributes #0 = { nounwind }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2, !3}

!0 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !1, producer: "swift", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
!1 = !DIFile(filename: "infer/tests/codetoanalyze/swift/bitcode-handcrafted-pulse/rc_wvd_nested_class.swift", directory: ".")
!2 = !{i32 7, !"Dwarf Version", i32 4}
!3 = !{i32 2, !"Debug Info Version", i32 3}
!6 = distinct !DISubprogram(name: "setupBad", scope: !1, file: !1, line: 10, type: !7, scopeLine: 10, spFlags: DISPFlagDefinition, unit: !0)
!7 = !DISubroutineType(types: !8)
!8 = !{null}
!12 = !DILocation(line: 12, column: 5, scope: !6)
!13 = !DILocation(line: 13, column: 5, scope: !6)
!14 = !DILocation(line: 14, column: 5, scope: !6)
