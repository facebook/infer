; Copyright (c) Facebook, Inc. and its affiliates.
;
; This source code is licensed under the MIT license found in the
; LICENSE file in the root directory of this source tree.
;
; Hand-crafted LLVM IR for the cross-module pattern where the Wvd field-offset
; global is *only declared externally* in the consumer module (no initializer),
; because the defining module is elsewhere. This is the LLVM shape produced
; when one Swift module references a property defined in another module — the
; Wvd symbol arrives as `external constant`, not as a constant definition.
;
; Sibling of cross_module_field_decl.swift.ll: same struct layout and consumer
; function, but the Wvd global is `external` rather than locally defined. The
; current golden pins the broken behavior — the Wvd-named field (`tapHandler`)
; is missing from the struct decl, because the extern is skipped by
; [process_wvd_globals]. A follow-up fix updates the golden once externs also
; contribute their (class, field) pair.

target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%T7Definer6ButtonC = type { ptr, i64 }

@"$s7Definer6ButtonC10tapHandleryycSgvpWvd" = external constant i64, align 8

define void @"$s8Consumer10setHandleryy7Definer6ButtonC_yyctF"(ptr %0, i64 %1) {
entry:
  %field = getelementptr inbounds %T7Definer6ButtonC, ptr %0, i32 0, i32 1
  store i64 %1, ptr %field, align 8
  ret void
}
