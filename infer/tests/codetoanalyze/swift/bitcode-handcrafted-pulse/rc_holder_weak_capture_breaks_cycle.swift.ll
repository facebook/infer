; Diagnostic fixture: does Pulse's [register_closure_holder] model correctly
; propagate the weak attribution from a closure-env struct's [%swift.weak]
; slot to the cycle-detection walker?
;
; Setup:
;   class Outer { var workItem: AnyObject? }
;   func setupBad() {
;     // Closure-env with [weak self] (mirroring swiftc's lowering of
;     //   { [weak self] in ... } for DispatchWorkItem / Task.detached)
;     let env = swift_allocObject(layout=<{ refcounted, swift.weak }>)
;     env.field_1 = self                            // [weak self] store
;
;     // external_register_handler is matched by PulseModelsSwift's
;     // register_closure_holder, which mints:
;     //   holder = SwiftAlloc, dynamic type swift_alloc_unknown_type
;     //   holder.captured_env = env  (strong, via Fieldname.make ~is_weak:false)
;     let holder = external_register_handler(callback, captured_env=env)
;
;     self.workItem = holder
;   }
;
; Walked cycle path:
;   self -> self.workItem (= holder)
;        -> holder.captured_env (= env)   [strong, per model]
;        -> env.field_1 (= self)          [SHOULD be weak via %swift.weak struct]
;
; Outcomes:
; (1) Pulse reports RETAIN_CYCLE: weak attribution is lost across the
;     model's synthetic captured_env edge -> the IGDBC FP hypothesis is
;     confirmed; fix lives in [register_closure_holder] or its downstream.
; (2) No issue reported: the closure-env weak slot reaches the cycle walker
;     correctly, the cycle breaks at env.field_1, and the IGDBC FP comes
;     from a different mechanism (need release diagnostics to pin).

target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%T2CH5OuterC = type { ptr, ptr }
%swift.refcounted = type { ptr, i64 }
%swift.weak = type { ptr }
%TBlock = type <{ %swift.refcounted, %swift.weak }>

@"$s2CH5OuterC8workItemyXlSgvpWvd" = constant i64 16, align 8

declare ptr @swift_allocObject(ptr, i64, i64) #0
declare ptr @external_register_handler(ptr, ptr) #0
declare void @callback_stub(ptr) #0

define swiftcc void @"$s2CH5OuterC8setupBadyyF"(ptr swiftself %self) !dbg !6 {
entry:
  ; Allocate the closure-env block; field_1 is the %swift.weak slot.
  %block = call ptr @swift_allocObject(ptr null, i64 24, i64 7), !dbg !10
  %weak_slot = getelementptr inbounds nuw %TBlock, ptr %block, i32 0, i32 1, !dbg !10
  store ptr %self, ptr %weak_slot, align 8, !dbg !10

  ; holder = external_register_handler(callback, captured_env=block).
  ; The model wires: holder.captured_env = block (strong).
  %holder = call ptr @external_register_handler(ptr @callback_stub, ptr %block), !dbg !11

  ; self.workItem = holder
  %off = load i64, ptr @"$s2CH5OuterC8workItemyXlSgvpWvd", align 8, !dbg !12
  %self_slot = getelementptr i8, ptr %self, i64 %off, !dbg !12
  store ptr %holder, ptr %self_slot, align 8, !dbg !12

  ret void, !dbg !13
}

attributes #0 = { nounwind }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2, !3}

!0 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !1, producer: "swift", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
!1 = !DIFile(filename: "infer/tests/codetoanalyze/swift/bitcode-handcrafted-pulse/rc_holder_weak_capture_breaks_cycle.swift", directory: ".")
!2 = !{i32 7, !"Dwarf Version", i32 4}
!3 = !{i32 2, !"Debug Info Version", i32 3}
!6 = distinct !DISubprogram(name: "setupBad", scope: !1, file: !1, line: 7, type: !7, scopeLine: 7, spFlags: DISPFlagDefinition, unit: !0)
!7 = !DISubroutineType(types: !8)
!8 = !{null}
!10 = !DILocation(line: 8, column: 5, scope: !6)
!11 = !DILocation(line: 9, column: 5, scope: !6)
!12 = !DILocation(line: 10, column: 5, scope: !6)
!13 = !DILocation(line: 11, column: 5, scope: !6)
