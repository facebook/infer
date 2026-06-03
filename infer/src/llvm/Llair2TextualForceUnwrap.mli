(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Recognise the Swift Optional force-unwrap (postfix [!]) lowering shapes (branch-form diamond and
    constant-folded init_none-then-trap) and replace the trap-block [__sil_assert_fail] call with
    [__swift_optional_force_unwrap_trap(<opt>)] so the companion Pulse model can fire [SWIFT_NPE]
    instead of the generic [PULSE_ASSERTION_ERROR]. *)

val rewrite_module : Textual.Module.t -> Textual.Module.t
(** Apply the rewrite to every procedure in [module_]. No-op on non-Swift modules. *)
