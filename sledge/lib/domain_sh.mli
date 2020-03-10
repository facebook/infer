(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Abstract domain *)

include Domain_intf.Dom

(* formals should include all the parameters of the summary. That is both
   formals and globals. *)
val create_summary :
     locals:Reg.Set.t
  -> formals:Reg.Set.t
  -> entry:t
  -> current:t
  -> summary * t

val simplify_states : bool ref
