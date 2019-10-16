(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Used-globals abstract domain *)

include Domain_sig.Dom with type summary = Reg.Set.t

type r =
  | Per_function of Reg.Set.t Reg.Map.t
      (** per-function used-globals map *)
  | Declared of Reg.Set.t  (** program-wide set *)

val by_function : r -> Reg.t -> summary
