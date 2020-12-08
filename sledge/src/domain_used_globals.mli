(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Used-globals abstract domain *)

include Domain_intf.Dom with type summary = Llair.Global.Set.t

val by_function : Domain_intf.used_globals -> Llair.Function.t -> summary
