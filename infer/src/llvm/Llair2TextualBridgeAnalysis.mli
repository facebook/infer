(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

val find_msgsends_with_recoverable_nullability : Llair.func -> (int, unit) Base.Hashtbl.t
(** For a Swift LLAIR proc, return the set (keyed by [Llair.Reg.id]) of [objc_msgSend] /
    [performSelector] [areturn] registers whose downstream CFG carries a structural signal that lets
    the Llair->Textual translator recover a [Nullable] annotation on the call. Two shapes qualify:

    - [as?]-cast re-bridge: some path from the seed reaches a [_bridgeToObjectiveC] call through
      [_unconditionallyBridgeFromObjectiveC] + ARC retain/release helpers. Source-level signature of
      [if let s = api.f() as? T { ... }].

    - Optional-passthrough getter: every path from the seed reaches a value-returning [Return]
      through the same transparent chain, AND at least one path traverses
      [_unconditionallyBridgeFromObjectiveC]. Source-level signature of
      [func g() -> T? { return api.f() }].

    Both shapes are sound to suppress: the [as?] caller has already typed the result as Optional,
    and the passthrough caller's [-> T?] return forces every chained caller to handle nil.

    Optional-chain ([?.]), [guard let X = e else { return nil }], and [e ?? default] are recognised
    at the SIL level by [CallReturnNullChecked] (it sets [cf_return_null_checked]). Both consumers
    ([SwiftObjCNullabilityChecker] and [PulseModelsSwift]) gate MNA on either [cf_caller_ret_annots]
    or [cf_return_null_checked], so the two preanalyses compose. *)
