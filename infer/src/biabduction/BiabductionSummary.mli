(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module for joined props: the result of joining together propositions repeatedly *)
module Jprop : sig
  (** Remember when a prop is obtained as the join of two other props; the first parameter is an id *)
  type 'a t = Prop of int * 'a Prop.t | Joined of int * 'a Prop.t * 'a t * 'a t

  val compare : 'a t -> 'a t -> int
  (** Comparison for joined_prop *)

  val equal : 'a t -> 'a t -> bool
  (** Return true if the two join_prop's are equal *)

  val d_shallow : Prop.normal t -> unit
  (** Dump the toplevel prop *)

  val d_list : shallow:bool -> Prop.normal t list -> unit
  (** dump a joined prop list, the boolean indicates whether to print toplevel props only *)

  val free_vars : Prop.normal t -> Ident.t Sequence.t

  val filter : ('a t -> 'b option) -> 'a t list -> 'b list
  (** [jprop_filter filter joinedprops] applies [filter] to the elements
      of [joindeprops] and applies it to the subparts if the result is
      [None]. Returns the most absract results which pass [filter]. *)

  val jprop_sub : Sil.subst -> Prop.normal t -> Prop.exposed t
  (** apply a substitution to a jprop *)

  val map : ('a Prop.t -> 'b Prop.t) -> 'a t -> 'b t
  (** map the function to each prop in the jprop, pointwise *)

  val to_prop : 'a t -> 'a Prop.t
  (** Extract the toplevel jprop of a prop *)
end

(** set of visited nodes: node id and list of lines of all the instructions *)
module Visitedset : Caml.Set.S with type elt = Procdesc.Node.id * int list

(** A spec consists of:
    pre: a joined prop
    posts: a list of props with path
    visited: a list of pairs (node_id, line) for the visited nodes *)
type 'a spec = {pre: 'a Jprop.t; posts: ('a Prop.t * Paths.Path.t) list; visited: Visitedset.t}

(** encapsulate type for normalized specs *)
module NormSpec : sig
  type t

  val compact : Sil.sharing_env -> t -> t
  (** Return a compact representation of the spec *)

  val erase_join_info_pre : Tenv.t -> t -> t
  (** Erase join info from pre of spec *)
end

val normalized_specs_to_specs : NormSpec.t list -> Prop.normal spec list
(** Cast a list of normalized specs to a list of specs *)

val pp_spec : Format.formatter -> _ spec -> unit

val spec_normalize : Tenv.t -> Prop.normal spec -> NormSpec.t
(** Convert spec into normal form w.r.t. variable renaming *)

type phase = FOOTPRINT | RE_EXECUTION

val equal_phase : phase -> phase -> bool

val string_of_phase_short : phase -> string

val get_specs_from_preposts : NormSpec.t list option -> Prop.normal spec list

type t = {preposts: NormSpec.t list; phase: phase}

val opt_get_phase : t option -> phase

val pp : Pp.env -> Format.formatter -> t -> unit
