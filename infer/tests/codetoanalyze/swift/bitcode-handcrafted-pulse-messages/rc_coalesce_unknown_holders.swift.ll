; Copyright (c) Facebook, Inc. and its affiliates.
;
; This source code is licensed under the MIT license found in the
; LICENSE file in the root directory of this source tree.
;
; Message-text fixture for the adjacent-node coalescing in
; [PulseDiagnostic.pp_retain_cycle]. A chain of closure-holder allocations,
; each modelled by [PulseModelsSwift.register_closure_holder] (via the
; [external_register_handler] test stub), stamped with
; [SwiftClassName.swift_alloc_unknown_type]. The two interior holders have
; no clean decompiler access path and no recovered concrete type, so both
; render as the same opaque "dynamically allocated object" label and sit
; adjacent in the detected cycle. The renderer should collapse them to a
; single row with an "(xN)" count and a merged "assigned on lines ..."
; list, rather than emitting one identical line each.
;
; Source-level intent:
;
;     class Outer { var holder: AnyObject? }
;     func setupBad(self: Outer) {
;         let h1 = external_register_handler(callback: nil, capturedEnv: self)
;         let h2 = external_register_handler(callback: nil, capturedEnv: h1)
;         let h3 = external_register_handler(callback: nil, capturedEnv: h2)
;         self.holder = h3
;     }
;
; Cycle: [self -> holder -> h3 -> captured_env -> h2 -> captured_env -> h1
;         -> captured_env -> self]. [h3] is reached via the clean
; [self->holder] path; [h2] and [h1] fall to the type-recovery fallback and
; both render "dynamically allocated object", adjacent — exercising the
; coalesce. (Two chained holders aren't enough: the cycle reducer drops one
; of a 2-holder pair, leaving a single interior node.)

target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%T2CM5OuterC = type { ptr, ptr }

@"$s2CM5OuterC6holderyXlSgvpWvd" = constant i64 16, align 8

declare ptr @external_register_handler(ptr, ptr) #0

define swiftcc void @"$s2CM5OuterC8setupBadyyF"(ptr swiftself %self) !dbg !6 {
entry:
  ; h1->captured_env = self ; h2->captured_env = h1 ; h3->captured_env = h2
  %h1 = call ptr @external_register_handler(ptr null, ptr %self), !dbg !10
  %h2 = call ptr @external_register_handler(ptr null, ptr %h1), !dbg !11
  %h3 = call ptr @external_register_handler(ptr null, ptr %h2), !dbg !12

  ; self.holder = h3   (closes the cycle)
  %off = load i64, ptr @"$s2CM5OuterC6holderyXlSgvpWvd", align 8, !dbg !13
  %slot = getelementptr i8, ptr %self, i64 %off, !dbg !13
  store ptr %h3, ptr %slot, align 8, !dbg !13

  ret void, !dbg !14
}

attributes #0 = { nounwind }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2, !3}

!0 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !1, producer: "swift", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
!1 = !DIFile(filename: "infer/tests/codetoanalyze/swift/bitcode-handcrafted-pulse-messages/rc_coalesce_unknown_holders.swift", directory: ".")
!2 = !{i32 7, !"Dwarf Version", i32 4}
!3 = !{i32 2, !"Debug Info Version", i32 3}
!6 = distinct !DISubprogram(name: "setupBad", scope: !1, file: !1, line: 7, type: !7, scopeLine: 7, spFlags: DISPFlagDefinition, unit: !0)
!7 = !DISubroutineType(types: !8)
!8 = !{null}
!10 = !DILocation(line: 8, column: 5, scope: !6)
!11 = !DILocation(line: 9, column: 5, scope: !6)
!12 = !DILocation(line: 10, column: 5, scope: !6)
!13 = !DILocation(line: 11, column: 5, scope: !6)
!14 = !DILocation(line: 12, column: 5, scope: !6)
