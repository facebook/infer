(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Used-globals abstract domain *)

include Domain_intf.Dom with type summary = Llair.Global.Set.t

type r =
  | Per_function of Llair.Global.Set.t Llair.Function.Map.t
      (** per-function used-globals map *)
  | Declared of Llair.Global.Set.t  (** program-wide set *)

val by_function : r -> Llair.Function.t -> summary
