(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Variables *)
module type S = sig
  type t [@@deriving compare, equal, sexp]
  type strength = t -> [`Universal | `Existential | `Anonymous] option

  val ppx : strength -> t pp
  val pp : t pp

  module Map : sig
    include NS.Map.S with type key := t

    val t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a t
  end

  module Set : sig
    include NS.Set.S with type elt := t

    val t_of_sexp : Sexp.t -> t
    val ppx : strength -> t pp
    val pp : t pp
    val pp_xs : t pp
    val pp_diff : (t * t) pp
  end

  val id : t -> int
  val name : t -> string
  val fresh : string -> wrt:Set.t -> t * Set.t

  val identified : name:string -> id:int -> t
  (** Create a variable identified by [id]. The [id] uniquely identifies the
      variable, and must be positive. *)

  (** Renaming Substitutions: injective maps from variables to variables *)
  module Subst : sig
    type var := t
    type t [@@deriving compare, equal, sexp]
    type x = {sub: t; dom: Set.t; rng: Set.t}

    val pp : t pp
    val empty : t
    val freshen : Set.t -> wrt:Set.t -> x * Set.t
    val invert : t -> t

    val restrict_dom : t -> Set.t -> x
    (** restrict the domain of a substitution to a set, and yield the range
        of the unrestricted substitution *)

    val is_empty : t -> bool
    val domain : t -> Set.t
    val range : t -> Set.t
    val fold : t -> 's -> f:(var -> var -> 's -> 's) -> 's
    val apply : t -> var -> var
  end

  (** Variable Context: A vocabulary that can serve as a source of fresh
      variable names and an (existential) quantifier prefix *)
  module Context : sig
    type t [@@deriving compare, equal, sexp]

    val voc : t -> Set.t
    (** [voc vx] is the vocabulary of [vx]. *)

    val xs : t -> Set.t
    (** [xs vx] is the quantifier prefix of [vx]. *)

    val empty : t
    (** The empty context, which does not constrain the choice of freshly
        generated variables and quantifies no variables. *)

    val of_vars : Set.t -> t
    (** The context with vocabulary containing the given variables and
        quantifies no variables. *)

    val with_xs : Set.t -> t -> t
    (** [with_xs xs vx] is [vx] but with quantifier prefix [xs]. *)

    val merge : t -> t -> t
    (** Combine two variable contexts, assuming no clashes. *)

    val contains : t -> Set.t -> bool
    (** [contains vx vars] holds if [vars] are included in the vocabulary of
        [vx]. *)

    val diff : Set.t -> t -> Set.t
    (** [diff vars vx] is the subset of [vars] not in the vocabulary of
        [vx]. *)

    val inter : Set.t -> t -> Set.t
    (** [inter vars vx] is the subset of [vars] in the vocabulary of [vx]. *)

    val diff_inter : Set.t -> t -> Set.t * Set.t
    (** [diff] and [inter] computed simultaneously. *)

    val pp_voc : t pp
    val pp_diff : (t * t) pp
  end

  (** Fresh Variables: A value of type ['a m] is a value of type ['a] which
      may contain not-yet-named variables. Unnamed variables are created
      using [var], and can be generated fresh with respect to the vocabulary
      of a [Context.t] using [gen]. *)
  module Fresh : sig
    (** An ['a m] value is an ['a] value that may contain not-yet-named
        variables. *)
    type 'a m = Context.t ref -> 'a
    (** This is essentially a state monad where, to avoid corner-case
        performance footguns, the representation is exposed and rather than
        exposing [return] and [bind], clients must write the only/obvious
        code of the right type:

        {[
          let return : 'a -> 'a m = fun x _ -> x

          let ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m =
           fun m f vx -> f (m vx) vx
        ]} *)

    val gen : Context.t -> 'a m -> 'a * Context.t
    (** [gen x m] generates names fresh with respect to [x] for unnamed
        variables in [m], and produces the value in terms of the fresh
        variables together with an updated context. *)

    val gen_ : Context.t -> 'a m -> 'a
    (** Like [gen] but discards the updated context. *)

    val var : string -> t m
    (** Fresh variable whose name is based on the given string. *)

    val rename : ?existential:bool -> t -> Subst.t -> Subst.t m
    (** Extend a substitution with a mapping freshening the given variable.
        The generated variable is added to the current quantifier prefix
        unless [existential:false] is passed. *)

    val subst : Set.t -> Subst.t m
    (** [subst vs] generates a substitution from [vs] to fresh variables. *)

    val reset_xs : Context.t m
    (** Read the context and then reset the existentials to empty. *)

    val extract_xs : Set.t m
    (** Read the existentials and then reset them to empty. *)

    val inter_xs : Set.t -> unit m
    (** Remove the complement of a set from the existentials. *)

    val fold_iter :
      ('a * Context.t) iter m -> 'z -> f:('a -> 'z -> 'z) -> 'z m
    (** Fold over a sequence of value-context pairs, accumulating the values
        with [f] and merging the contexts into the current context. *)

    module Import : sig
      module Dbg : sig
        include module type of Dbg

        val dbgs :
             ?call:(pf -> unit)
          -> ?retn:(pf -> 'a * (Context.t * Context.t) -> unit)
          -> ?rais:(pf -> Context.t -> exn -> Printexc.raw_backtrace -> unit)
          -> string
          -> 'a m
          -> 'a m
      end
    end
  end
end
