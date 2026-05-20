; Copyright (c) Facebook, Inc. and its affiliates.
;
; This source code is licensed under the MIT license found in the
; LICENSE file in the root directory of this source tree.
;
; Hand-crafted LLVM IR exercising a third Swift mangled-name substitution
; pattern the Wvd parser must walk past in order to find the C/V class
; boundary marker.
;
; Pattern: parent-namespace substitution back-reference immediately
; preceding the class marker.
;   "$s14OuterContainerAAC8delegateAA0bC8Delegate_pSgvpWvd"
;   After consuming the 14-character class identifier the parser sits at
;   "AAC": "AA" is a back-reference to the parent namespace (typically the
;   module name) and "C" is the class marker for the *inner* class. The
;   parser must skip the "AA" back-reference to recognise the immediately
;   following "C" as the class boundary, then continue with the
;   length-prefixed property segment.
;
; Distinct from the sibling [wvd_substitution_nested] pattern (which has a
; nested *type* expansion of shape "0A<length><name>C" between the parent
; and the class marker) and from [wvd_substitution_compressed] (which
; carries the substitution inside the *property* segment rather than
; between class and marker).
;
; The current .sil baseline captures the broken pre-fix behaviour: the
; parser walks past "AA" via the generic non-digit skip but then also
; walks past the "C" marker (because the recursive parse step sees "C" as
; a non-digit and skips it), eventually failing on the property's
; substitution-compressed "8delegateAA0bC8Delegate_pSg" segment and
; returning (None, "unknown_field"). No class is recognised, no
; `type __infer_swift_type<...>` declaration is injected. The follow-up
; diff in this stack adds the parent-namespace skip to the parser and
; updates this baseline to show the recovered class type and field name.

target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%T14OuterContainerAAC = type { ptr, ptr }

@"$s14OuterContainerAAC8delegateAA0bC8Delegate_pSgvpWvd" = constant i64 16, align 8
