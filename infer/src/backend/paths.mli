(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Execution Paths *)

module Path : sig
  (** type for paths *)
  type t

  type session = int

  val add_call : bool -> t -> Typ.Procname.t -> t -> t
  (** add a call with its sub-path, the boolean indicates whether the subtrace for the procedure should be included *)

  val add_skipped_call : t -> Typ.Procname.t -> string -> t
  (** add a call to a procname that's had to be skipped, along with the reason *)

  val contains : t -> t -> bool
  (** check whether a path contains another path *)

  val contains_position : t -> PredSymb.path_pos -> bool
  (** check wether the path contains the given position *)

  val create_loc_trace : t -> PredSymb.path_pos option -> Errlog.loc_trace
  (** Create the location trace of the path, up to the path position if specified *)

  val curr_node : t -> Procdesc.Node.t option
  (** return the current node of the path *)

  val d : t -> unit
  (** dump a path *)

  val d_stats : t -> unit
  (** dump statistics of the path *)

  val extend : Procdesc.Node.t -> Typ.Name.t option -> session -> t -> t
  (** extend a path with a new node reached from the given session, with an optional string for exceptions *)

  val add_description : t -> string -> t

  val iter_all_nodes_nocalls : (Procdesc.Node.t -> unit) -> t -> unit
  (** iterate over each node in the path, excluding calls, once *)

  val iter_shortest_sequence :
    (int -> t -> int -> Typ.Name.t option -> unit) -> PredSymb.path_pos option -> t -> unit
  (** iterate over the shortest sequence belonging to the path,
      restricting to those containing the given position if given.
      Do not iterate past the last occurrence of the given position.
      [f level path session exn_opt] is passed the current nesting [level] and [path]
      and previous [session] and possible exception [exn_opt] *)

  val join : t -> t -> t
  (** join two paths *)

  val pp : Format.formatter -> t -> unit
  (** pretty print a path *)

  val pp_stats : Format.formatter -> t -> unit
  (** pretty print statistics of the path *)

  val start : Procdesc.Node.t -> t
  (** create a new path with given start node *)
end

(** Set of (prop,path) pairs, where the identity is given by prop *)
module PathSet : sig
  type t

  val add_renamed_prop : Prop.normal Prop.t -> Path.t -> t -> t
  (** It's the caller's resposibility to ensure that Prop.prop_rename_primed_footprint_vars was called on the prop *)

  val d : t -> unit
  (** dump the pathset *)

  val diff : t -> t -> t
  (** difference between two pathsets *)

  val empty : t
  (** empty pathset *)

  val elements : t -> (Prop.normal Prop.t * Path.t) list
  (** list of elements in a pathset *)

  val equal : t -> t -> bool
  (** equality for pathsets *)

  val filter : (Prop.normal Prop.t -> bool) -> t -> t
  (** filter a pathset on the prop component *)

  val filter_path : Path.t -> t -> Prop.normal Prop.t list
  (** find the list of props whose associated path contains the given path *)

  val fold : (Prop.normal Prop.t -> Path.t -> 'a -> 'a) -> t -> 'a -> 'a
  (** fold over a pathset *)

  val from_renamed_list : (Prop.normal Prop.t * Path.t) list -> t
  (** It's the caller's resposibility to ensure that Prop.prop_rename_primed_footprint_vars was called on the list *)

  val is_empty : t -> bool
  (** check whether the pathset is empty *)

  val iter : (Prop.normal Prop.t -> Path.t -> unit) -> t -> unit
  (** iterate over a pathset *)

  val map : (Prop.normal Prop.t -> Prop.normal Prop.t) -> t -> t
  (** map over the prop component of a pathset. *)

  val map_option : (Prop.normal Prop.t -> Prop.normal Prop.t option) -> t -> t
  (** map over the prop component of a pathset using a partial function; elements mapped to None are discarded *)

  val partition : (Prop.normal Prop.t -> bool) -> t -> t * t
  (** partition a pathset on the prop component *)

  val pp : Pp.env -> Format.formatter -> t -> unit
  (** pretty print the pathset *)

  val size : t -> int
  (** number of elements in the pathset *)

  val to_proplist : t -> Prop.normal Prop.t list
  (** convert to a list of props *)

  val to_propset : Tenv.t -> t -> Propset.t
  (** convert to a set of props *)

  val union : t -> t -> t
  (** union of two pathsets *)
end
