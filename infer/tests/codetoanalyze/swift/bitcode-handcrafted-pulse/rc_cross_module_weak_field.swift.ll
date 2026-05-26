; Copyright (c) Facebook, Inc. and its affiliates.
;
; This source code is licensed under the MIT license found in the
; LICENSE file in the root directory of this source tree.
;
; Pulse end-to-end test pinning the cross-module weak-field-loss false
; positive (T271610450, class-property shape).
;
; Source-level intent:
;
;     // Module `Definer` (compiled separately, NOT in this bitcode):
;     class Button {
;         weak var tapHandler: AnyObject?
;     }
;
;     // Module `Consumer` (this bitcode):
;     class Owner {
;         let button: Button
;         init(button: Button) {
;             self.button = button
;             self.button.tapHandler = self    // weak assignment, NO cycle
;         }
;     }
;
; Because `tapHandler` is declared `weak` in `Definer`, the source-level
; semantics are clear: no cycle. But the Consumer module's bitcode sees
; only the positional layout of `Button` (no [swift::weak] wrapper, no
; [swift5_fieldmd] descriptor table — the field-attribute information
; lives entirely in the Definer module's bitcode/binary).
;
; Today's behaviour: the Consumer's [Llair2TextualType.to_textual_field_decls]
; walks the positional layout and emits [tapHandler] with [attributes=[]],
; i.e. NOT weak. [process_wvd_globals] (D105684482) then names the field
; from the extern Wvd descriptor but does NOT add the weak attribute either
; (it cannot, because [WvD] symbols carry only byte offsets, not attribute
; flags). Pulse therefore treats the [tapHandler] store as STRONG and
; reports a false [RETAIN_CYCLE].
;
; The cross-module store IS lowered as a [swift_weakAssign] call (the Swift
; runtime call for weak assignment is preserved even under [-O] because it
; maintains the weak-ref runtime table). However, Pulse's existing model
; rewrites [swift_weakAssign(target, value)] as [Store target := value]
; without any "weak" annotation on the Store itself. The weak/strong
; determination at cycle-detection time is per-field via [Fieldname.is_weak]
; — and that comes back as [Some false] for cross-module fields.
;
; A future fix must recover the weak attribute from outside the Consumer's
; bitcode (defining-module sidecar, aggregated bitcode at capture time, or
; an out-of-band field-attribute table).
;
; This fixture pins the CURRENT broken behaviour (a [RETAIN_CYCLE] report)
; so a future fix flips it to no-report and the change is reviewable.

target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Cross-module class. Consumer sees only positional layout: header + one
; pointer-sized slot at offset 16 (the [tapHandler] weak ref). The Wvd
; below is declared `external constant` — we don't know its offset at
; analysis time, but its mangled name names the (class, field) pair.
%T7Definer6ButtonC = type { ptr, i64 }

; In-module Owner: header + button slot at offset 16. The Wvd is defined
; here because the Consumer module declares the property.
%T8Consumer5OwnerC = type { ptr, ptr }

; Cross-module Wvd: external constant. Consumer doesn't know its value;
; the runtime resolves it at link/load time.
@"$s7Definer6ButtonC10tapHandleryyXlSgvpWvd" = external constant i64

; In-module Wvd: constant 16 (the `button` field's offset in Owner).
@"$s8Consumer5OwnerC6buttonAA7Definer6ButtonCvpWvd" = constant i64 16, align 8

%swift.metadata_response = type { ptr, i64 }

declare swiftcc void @swift_weakAssign(ptr, ptr) #0
declare swiftcc %swift.metadata_response @"$s7Definer6ButtonCMa"(i64) #0
declare ptr @swift_allocObject(ptr, i64, i64) #0

; setupBad: closes the apparent cycle [self -> button -> tapHandler -> self].
; The actual store is a [swift_weakAssign] runtime call (the source-level
; [weak]-attributed property's setter emits this and survives -O inlining),
; so the cycle should NOT be reported. Today it is, because the
; Consumer's view of [tapHandler]'s field decl has no weak attribute.
define swiftcc void @"$s8Consumer5OwnerC8setupBad6buttonyAA7Definer6ButtonC_tF"(
    ptr %button, ptr swiftself %self) !dbg !6 {
entry:
  ; self.button = button         (in-module constant-Wvd store, back-edge)
  %off_button = load i64, ptr @"$s8Consumer5OwnerC6buttonAA7Definer6ButtonCvpWvd", align 8, !dbg !12
  %slot_button = getelementptr i8, ptr %self, i64 %off_button, !dbg !12
  store ptr %button, ptr %slot_button, align 8, !dbg !12

  ; self.button.tapHandler = self
  ;
  ; Lowered as a swift_weakAssign runtime call because the defining-module
  ; setter is `weak`. The target address is computed via the extern Wvd.
  %off_tap = load i64, ptr @"$s7Definer6ButtonC10tapHandleryyXlSgvpWvd", align 8, !dbg !13
  %slot_tap = getelementptr i8, ptr %button, i64 %off_tap, !dbg !13
  call swiftcc void @swift_weakAssign(ptr %slot_tap, ptr %self), !dbg !13

  ret void, !dbg !14
}

attributes #0 = { nounwind }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2, !3}

!0 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !1, producer: "swift", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
!1 = !DIFile(filename: "infer/tests/codetoanalyze/swift/bitcode-handcrafted-pulse/rc_cross_module_weak_field.swift", directory: ".")
!2 = !{i32 7, !"Dwarf Version", i32 4}
!3 = !{i32 2, !"Debug Info Version", i32 3}
!6 = distinct !DISubprogram(name: "setupBad", scope: !1, file: !1, line: 19, type: !7, scopeLine: 19, spFlags: DISPFlagDefinition, unit: !0)
!7 = !DISubroutineType(types: !8)
!8 = !{null}
!12 = !DILocation(line: 20, column: 5, scope: !6)
!13 = !DILocation(line: 21, column: 5, scope: !6)
!14 = !DILocation(line: 22, column: 5, scope: !6)
