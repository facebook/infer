(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Used-globals abstract domain *)

include Domain_intf.Dom with type summary = Llair.Reg.Set.t

type r =
  | Per_function of Llair.Reg.Set.t Llair.Reg.Map.t
      (** per-function used-globals map *)
  | Declared of Llair.Reg.Set.t  (** program-wide set *)

val by_function : r -> Llair.Reg.t -> summary
