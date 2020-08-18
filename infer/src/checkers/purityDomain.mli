(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module ModifiedParamIndices : sig
  type t

  val pp : Format.formatter -> t -> unit

  val empty : t

  val of_list : int list -> t

  val is_empty : t -> bool

  val mem : int -> t -> bool

  val add : int -> t -> t
end

include AbstractDomain.S with type t = ModifiedParamIndices.t AbstractDomain.Types.top_lifted

val pure : t
(** Pure abstract state: no side-effect on parameters and global values. The return value depends
    only on the value of parameters. *)

val impure_global : t
(** Impure abstract state: there may be side-effect on the parameters or global values. Or the
    return value may depend on the machine state, e.g. `Math.random`. *)

val impure_params : ModifiedParamIndices.t -> t
(** Impure abstract state: there may be side-effect on the parameters, but not on global values. *)

val is_pure : t -> bool

val all_params_modified : (Exp.t * Typ.t) list -> ModifiedParamIndices.t

type summary = t

val pp_summary : Format.formatter -> t -> unit
