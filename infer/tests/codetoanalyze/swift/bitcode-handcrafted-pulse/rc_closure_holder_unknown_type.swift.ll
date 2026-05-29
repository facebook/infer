; Copyright (c) Facebook, Inc. and its affiliates.
;
; This source code is licensed under the MIT license found in the
; LICENSE file in the root directory of this source tree.
;
; Pulse end-to-end test pinning the user-facing message produced when one
; of the retain-cycle participants is an opaque framework-returned holder
; that [PulseModelsSwift.register_closure_holder] stamps with
; [SwiftClassName.swift_alloc_unknown_type] because its layout is not
; known at model time. Reduces at IR level a recurring prod shape on
; Swift report bodies of the form
;
;     1) Swift closure
;     2) self->field_..., assigned on line N
;     3) self->field_...->source
;     4) object of type class __infer_swift_alloc_unknown_type, ...
;     5) &self, assigned on line N.
;
; Source-level intent:
;
;     class Outer {
;         var inner: Inner
;     }
;     class Inner {
;         var holder: AnyObject?
;     }
;     func setupBad(self: Outer, inner: Inner) {
;         self.inner = inner
;         // The model expands this to a swift_alloc_unknown_type-typed
;         // holder with a [captured_env] strong field pointing at [self].
;         let h = external_register_handler(callback=nil, capturedEnv=self)
;         inner.holder = h
;     }
;
; Cycle: [self -> inner -> holder -> captured_env -> self]. The holder
; hop's dynamic type is [swift_alloc_unknown_type] because
; [register_closure_holder] in [PulseModelsSwift] assigns that placeholder
; to its returned value (no Swift-class context recovers a real type at
; model time). With the cycle traversal going through a separately-named
; [inner] field, the holder participant has no clean access path the
; decompiler can render, forcing [get_expr_with_fallback] into the
; type-recovery fallback that surfaces the placeholder name.
;
; Without a filter, that fallback passes the recovered typename verbatim
; to the qualifier, producing
; [object of type class __infer_swift_alloc_unknown_type] in the
; user-facing message — a Pulse-internal symbol leaking into output.

target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%T2CH5OuterC = type { ptr, ptr }
%T2CH5InnerC = type { ptr, ptr }

@"$s2CH5OuterC5innerAA5InnerCvpWvd" = constant i64 16, align 8
@"$s2CH5InnerC6holderyXlSgvpWvd" = constant i64 16, align 8

declare ptr @external_register_handler(ptr, ptr) #0

define swiftcc void @"$s2CH5OuterC8setupBad5inneryAC5InnerC_tF"(ptr %inner, ptr swiftself %self) !dbg !6 {
entry:
  ; self.inner = inner
  %off1 = load i64, ptr @"$s2CH5OuterC5innerAA5InnerCvpWvd", align 8, !dbg !10
  %slot1 = getelementptr i8, ptr %self, i64 %off1, !dbg !10
  store ptr %inner, ptr %slot1, align 8, !dbg !10

  ; %h = external_register_handler(callback=null, captured_env=self)
  ; Model sets: %h has dynamic type swift_alloc_unknown_type;
  ;             %h->captured_env = %self  (strong edge, closes the cycle).
  %h = call ptr @external_register_handler(ptr null, ptr %self), !dbg !11

  ; inner.holder = %h
  %off2 = load i64, ptr @"$s2CH5InnerC6holderyXlSgvpWvd", align 8, !dbg !12
  %slot2 = getelementptr i8, ptr %inner, i64 %off2, !dbg !12
  store ptr %h, ptr %slot2, align 8, !dbg !12

  ret void, !dbg !13
}

attributes #0 = { nounwind }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2, !3}

!0 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !1, producer: "swift", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
!1 = !DIFile(filename: "infer/tests/codetoanalyze/swift/bitcode-handcrafted-pulse/rc_closure_holder_unknown_type.swift", directory: ".")
!2 = !{i32 7, !"Dwarf Version", i32 4}
!3 = !{i32 2, !"Debug Info Version", i32 3}
!6 = distinct !DISubprogram(name: "setupBad", scope: !1, file: !1, line: 7, type: !7, scopeLine: 7, spFlags: DISPFlagDefinition, unit: !0)
!7 = !DISubroutineType(types: !8)
!8 = !{null}
!10 = !DILocation(line: 8, column: 5, scope: !6)
!11 = !DILocation(line: 9, column: 5, scope: !6)
!12 = !DILocation(line: 10, column: 5, scope: !6)
!13 = !DILocation(line: 11, column: 5, scope: !6)
