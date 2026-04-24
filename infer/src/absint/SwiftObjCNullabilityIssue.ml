(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let message procname =
  F.asprintf
    "Method `%a` returns a pointer without `_Nullable` or `_Nonnull` annotations. When called from \
     Swift the return type is imported as an Implicitly Unwrapped Optional (`T!`), which traps at \
     runtime if the method ever returns `nil`. Annotate the declaration with `_Nullable` or \
     `_Nonnull` so the contract is explicit at the Swift call site."
    Procname.pp procname


(* The Swift frontend occasionally hands us a bogus call-site location (line 0, no source position)
   for synthesized calls like implicit `init`s. Reports anchored to such locations are not
   actionable for the user, so suppress them in both the static checker and the Pulse model. *)
let should_report_at (loc : Location.t) = loc.line > 0
