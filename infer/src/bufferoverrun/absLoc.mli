(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module Allocsite : sig
  type t = private
    | Unknown
    | Symbol of Symb.SymbolPath.partial
    | Known of
        { proc_name: string  (** the name of the procedure (builtin) which created the allocsite *)
        ; caller_pname: Procname.t option
              (** the name of the procedure for that the allocsite was created. That is, the
                  procedure that called proc_name *)
        ; node_hash: int  (** hash of the node being allocated *)
        ; inst_num: int  (** order of the instruction in the node, i.e. n-th instruction *)
        ; dimension: int  (** depth of nested array *)
        ; represents_multiple_values: bool
        ; path: Symb.SymbolPath.partial option }
    | LiteralString of string

  include PrettyPrintable.PrintableOrderedType with type t := t

  val unknown : t

  val is_unknown : t -> bool

  val make :
       Procname.t
    -> caller_pname:Procname.t option
    -> node_hash:int
    -> inst_num:int
    -> dimension:int
    -> path:Symb.SymbolPath.partial option
    -> represents_multiple_values:bool
    -> t

  val make_symbol : Symb.SymbolPath.partial -> t

  val literal_string : string -> t

  val get_param_path : t -> Symb.SymbolPath.partial option

  val eq : t -> t -> Boolean.t
end

module Loc : sig
  type prim = private Var of Var.t | Allocsite of Allocsite.t [@@deriving compare]

  type t = prim BufferOverrunField.t [@@deriving compare, equal]

  include PrettyPrintable.PrintableOrderedType with type t := t

  val of_allocsite : Allocsite.t -> t

  val of_c_strlen : t -> t
  (** It appends the [strlen] field. *)

  val of_id : Ident.t -> t

  val of_path : Symb.SymbolPath.partial -> t

  val of_pvar : Pvar.t -> t

  val of_var : Var.t -> t

  val unknown : t

  val exists_pvar : f:(Pvar.t -> bool) -> t -> bool
  (** It checks if a pvar in location satisfies [f]. *)

  val exists_str : f:(string -> bool) -> t -> bool
  (** It checks if a variable or a field name in the location path satisfies [f]. *)

  val get_literal_string : t -> string option

  val get_literal_string_strlen : t -> string option

  val get_path : t -> Symb.SymbolPath.partial option

  val get_param_path : t -> Symb.SymbolPath.partial option
  (** As get_path, but returns None if the path doesn't correspond to parameter passed by reference. *)

  val is_trans_field_of : loc:t -> field_loc:t -> bool
  (** Checks if field_loc is a direct or indirect field of loc. *)

  val is_frontend_tmp : t -> bool

  val is_global : t -> bool

  val get_global_array_initializer : t -> Procname.t option
  (** Return the name of global initializer when given abstract location represents a global
      constant array value *)

  val is_pretty : t -> bool
  (** It checks if it is representable with pretty form, e.g., with a path or with a variable name. *)

  val is_return : t -> bool

  val is_unknown : t -> bool

  val represents_multiple_values : t -> bool

  val is_objc_collection_internal_array : t -> bool

  val append_field : ?typ:Typ.t -> t -> Fieldname.t -> t
  (** It appends field. [typ] is the type of [fn]. *)
end

module LocSet : PrettyPrintable.PPSet with type elt = Loc.t

module PowLoc : sig
  include AbstractDomain.S

  val compare : t -> t -> int

  val get_parent_field : t -> t

  val append_field : ?typ:Typ.t -> t -> fn:Fieldname.t -> t

  val append_star_field : t -> fn:Fieldname.t -> t

  val bot : t

  val add : Loc.t -> t -> t

  val of_list : Loc.t list -> t

  val exists : (Loc.t -> bool) -> t -> bool

  val mem : Loc.t -> t -> bool

  val is_singleton_or_more : t -> Loc.t IContainer.singleton_or_more

  val min_elt_opt : t -> Loc.t option

  val singleton : Loc.t -> t

  val fold : (Loc.t -> 'a -> 'a) -> t -> 'a -> 'a

  val cast : Typ.t -> t -> t

  val of_c_strlen : t -> t
  (** It appends the [strlen] field. *)

  val unknown : t

  val exists_str : f:(string -> bool) -> t -> bool
  (** It checks if a variable or a field name in the location path satisfies [f]. *)

  val is_bot : t -> bool

  (** Type for evaluating a path to an abstract location. *)
  type eval_locpath = Symb.SymbolPath.partial -> t

  val subst : t -> eval_locpath -> t
  (** It substitutes paths in the abstract location using [eval_locpath]. *)

  val subst_loc : Loc.t -> eval_locpath -> t
  (** It substitutes paths in the abstract location using [eval_locpath]. *)

  val lift_cmp : Boolean.EqualOrder.t -> t -> t -> Boolean.t
  (** It lifts a comparison of [Loc.t] to [t]. The comparison can be [Boolean.EqualOrder.eq],
      [Boolean.EqualOrder.ne], etc. *)

  val to_set : t -> LocSet.t

  val get_linked_list_next : lhs:t -> rhs:t -> Loc.t option
  (** It checks whether [rhs] is of [lhs.any_field], which is a heuristic for detecting a linked
      list, e.g. [x = x.next()]. It returns [Some lhs] if the condition is satisfied, [None]
      otherwise. *)

  val is_unknown : t -> bool

  val is_single_known_loc : t -> bool
  (** Returns true if the set consists of a single known location. *)
end

val can_strong_update : PowLoc.t -> bool
(** It checks if the abstract location can be updated strongly. *)
