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
        { proc_name: string
        ; node_hash: int  (** hash of the node being allocated *)
        ; inst_num: int  (** order of the instruction in the node, i.e. n-th instruction *)
        ; dimension: int  (** depth of nested array *)
        ; represents_multiple_values: bool
        ; path: Symb.SymbolPath.partial option }
    | LiteralString of string

  include PrettyPrintable.PrintableOrderedType with type t := t

  val to_string : t -> string

  val unknown : t

  val make :
       Typ.Procname.t
    -> node_hash:int
    -> inst_num:int
    -> dimension:int
    -> path:Symb.SymbolPath.partial option
    -> represents_multiple_values:bool
    -> t

  val make_symbol : Symb.SymbolPath.partial -> t

  val literal_string : string -> t

  val get_literal_string : t -> string option

  val get_param_path : t -> Symb.SymbolPath.partial option

  val eq : t -> t -> Boolean.t
end

module Loc : sig
  type field_typ

  type t = private
    | Var of Var.t  (** abstract location of variable *)
    | Allocsite of Allocsite.t  (** abstract location of allocsites *)
    | Field of {prefix: t; fn: Typ.Fieldname.t; typ: field_typ}
        (** field appended abstract locations, i.e., [prefix.fn] *)
    | StarField of {prefix: t; last_field: Typ.Fieldname.t}
        (** field appended abstract locations, but some of intermediate fields are abstracted, i.e.,
            [prefix.*.fn] *)
  [@@deriving equal]

  include PrettyPrintable.PrintableOrderedType with type t := t

  val to_string : t -> string

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

  val is_field_of : loc:t -> field_loc:t -> bool
  (** It checks if [loc] is prefix of [field_loc]. *)

  val is_frontend_tmp : t -> bool

  val is_global : t -> bool

  val is_pretty : t -> bool
  (** It checks if it is representable with pretty form, e.g., with a path or with a variable
     name. *)

  val is_return : t -> bool

  val is_unknown : t -> bool

  val represents_multiple_values : t -> bool

  val append_field : ?typ:Typ.typ -> t -> fn:Typ.Fieldname.t -> t
  (** It appends field. [typ] is the type of [fn]. *)
end

module PowLoc : sig
  include AbstractDomain.FiniteSetS with type elt = Loc.t

  val append_field : t -> fn:Typ.Fieldname.t -> t

  val append_star_field : t -> fn:Typ.Fieldname.t -> t

  val bot : t

  val cast : Typ.typ -> t -> t

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
  (** It lifts a comparison of [Loc.t] to [t].  The comparison can be [Boolean.EqualOrder.eq],
     [Boolean.EqualOrder.ne], etc. *)
end

val can_strong_update : PowLoc.t -> bool
(** It checks if the abstract location can be updated strongly. *)
