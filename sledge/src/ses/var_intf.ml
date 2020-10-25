(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type REPR = sig
  type t [@@deriving compare, equal, sexp]

  val make : id:int -> name:string -> t
  val id : t -> int
  val name : t -> string
end

module type VAR = sig
  type t [@@deriving compare, equal, sexp]
  type 'a strength = 'a -> [`Universal | `Existential | `Anonymous] option

  val ppx : t strength -> t pp
  val pp : t pp

  module Map : sig
    include NS.Map.S with type key := t

    val t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a t
  end

  module Set : sig
    type var := t

    include NS.Set.S with type elt := t

    val sexp_of_t : t -> Sexp.t
    val t_of_sexp : Sexp.t -> t
    val ppx : var strength -> t pp
    val pp : t pp
    val pp_xs : t pp
  end

  val id : t -> int
  val name : t -> string
  val program : name:string -> global:bool -> t
  val fresh : string -> wrt:Set.t -> t * Set.t

  val identified : name:string -> id:int -> t
  (** Variable with the given [id]. Variables are compared by [id] alone,
      [name] is used only for printing. The only way to ensure [identified]
      variables do not clash with [fresh] variables is to pass the
      [identified] variables to [fresh] in [wrt]:
      [Var.fresh name ~wrt:(Var.Set.of_ (Var.identified ~name ~id))]. *)

  (** Variable renaming substitutions *)
  module Subst : sig
    type var := t
    type t [@@deriving compare, equal, sexp]
    type x = {sub: t; dom: Set.t; rng: Set.t}

    val pp : t pp
    val empty : t
    val freshen : Set.t -> wrt:Set.t -> x * Set.t
    val invert : t -> t
    val restrict : t -> Set.t -> x
    val is_empty : t -> bool
    val domain : t -> Set.t
    val range : t -> Set.t
    val fold : t -> 's -> f:(var -> var -> 's -> 's) -> 's
    val apply : t -> var -> var
  end
end
