(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Used-globals abstract domain *)

open Domain_intf
include Domain with type summary = Llair.Global.Set.t

type used_globals =
  | Per_function of Llair.Global.Set.t Llair.FuncName.Map.t
      (** map of functions to globals they or their callees use *)
  | Declared of Llair.Global.Set.t
      (** set of declared globals used in the program *)

val by_function : used_globals -> Llair.FuncName.t -> summary
