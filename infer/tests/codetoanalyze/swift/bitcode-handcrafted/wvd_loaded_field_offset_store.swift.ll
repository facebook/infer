; Copyright (c) Facebook, Inc. and its affiliates.
;
; This source code is licensed under the MIT license found in the
; LICENSE file in the root directory of this source tree.
;
; Hand-crafted LLVM IR pinning the runtime-Wvd-loaded GEP shape. The
; field offset is *loaded* from a Wvd descriptor (as a 32-bit integer)
; and *sign-extended* to i64 before being consumed by
; [getelementptr i8, ptr %nav, i64 %off64].
;
; This is a swiftc lowering for inlined existential / cross-module /
; resilient property writes when the optimiser cannot fold the Wvd
; descriptor lookup to a literal i64. The constant-folded variant is
; pinned by [static_byte_offset_delegate_store]; the plain
; runtime-loaded variant (without an i32->i64 cast) is pinned by
; [wvd_substitution_compressed]; this fixture pins the
; cast-and-dynamic-offset variant.
;
; What this file pins. With the [LlvmSledgeFrontend.get_wvd_global]
; cast-peeling fix applied, the SIL store target is the typed Field
; [n8.delegate] (receiver chain preserved via [DynamicWvd]). Without
; the fix, [get_wvd_global] returns [None] for the cast-wrapped load,
; the GEP collapses to [Llair.Exp.nondet], and the store target is
; [$builtins.llvm_nondet()] -- losing the [self -> nav -> delegate ->
; self] receiver chain that downstream Pulse needs to detect retain
; cycles.

target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Receiver class layout: 16-byte Swift class header (metadata + refcount)
; followed by a single [ptr] user field at byte offset 16.
%T4Test10NavbarViewC = type { ptr, ptr, ptr }

; Wvd field-offset descriptor for the [delegate] field at byte 16.
; Stored as i32: swiftc lowers field offsets as i32 in the table, then
; sign-extends at each use site.
@"$s4Test10NavbarViewC8delegateAA0bC8Delegate_pSgvpWvd" = constant i32 16, align 4

%swift.metadata_response = type { ptr, i64 }

declare swiftcc %swift.metadata_response @"$s4Test10NavbarViewCMa"(i64) #0

declare ptr @swift_allocObject(ptr, i64, i64) #0

define ptr @"$s4Test14ViewControllerC10navbarViewAA0c6NavbarF0CvgAGyXEfU_"(ptr %self) {
entry:
  ; nav = NavbarView()
  %m_resp = call swiftcc %swift.metadata_response @"$s4Test10NavbarViewCMa"(i64 0)
  %m = extractvalue %swift.metadata_response %m_resp, 0
  %nav = call ptr @swift_allocObject(ptr %m, i64 24, i64 7)

  ; nav.delegate = self
  ; Load Wvd as i32, sign-extend to i64, then GEP. This is the lowering
  ; pattern that motivates the [get_wvd_global] cast-peeling fix.
  %off32 = load i32, ptr @"$s4Test10NavbarViewC8delegateAA0bC8Delegate_pSgvpWvd", align 4
  %off64 = sext i32 %off32 to i64
  %slot = getelementptr i8, ptr %nav, i64 %off64
  store ptr %self, ptr %slot, align 8

  ret ptr %nav
}

attributes #0 = { nounwind }
