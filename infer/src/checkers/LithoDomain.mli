(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val is_component_or_section_builder : Typ.Name.t -> Tenv.t -> bool

(** Access path + its parent procedure *)
module LocalAccessPath : sig
  type t = private {access_path: AccessPath.t; parent: Procname.t} [@@deriving compare]

  val make : AccessPath.t -> Procname.t -> t

  val make_from_pvar : Pvar.t -> Typ.t -> Procname.t -> t

  val make_from_access_expression : HilExp.AccessExpression.t -> Procname.t -> t
end

val suffixes : String.Set.t

(** Called procedure & location *)
module MethodCallPrefix : sig
  type t = private {prefix: string; procname: Procname.t; location: Location.t}

  val make_with_prefixes : Procname.t -> Location.t -> t list
end

module Mem : sig
  type t

  val contains_build : t -> bool
end

include AbstractDomain.S

val subst :
     callsite:Location.t
  -> formals:(Pvar.t * Typ.t) list
  -> actuals:HilExp.t list
  -> ret_id_typ:AccessPath.base
  -> caller_pname:Procname.t
  -> callee_pname:Procname.t
  -> caller:t
  -> callee:Mem.t
  -> t

(** type for saving in summary payload *)
type summary = Mem.t

val init : Tenv.t -> Procname.t -> (Pvar.t * Typ.t) list -> LocalAccessPath.t -> t

val assign : lhs:LocalAccessPath.t -> rhs:LocalAccessPath.t -> t -> t

val assume_null : LocalAccessPath.t -> t -> t
(** Semantics of null assume statement, i.e., [assume(x==null)] *)

val call_create : LocalAccessPath.t -> Typ.name -> Location.t -> t -> t
(** Semantics of builder creation method *)

val call_builder :
  ret:LocalAccessPath.t -> receiver:LocalAccessPath.t -> MethodCallPrefix.t -> t -> t
(** Semantics of builder's methods, e.g. [prop] *)

val call_build_method : ret:LocalAccessPath.t -> receiver:LocalAccessPath.t -> t -> t
(** Semantics of builder's final build method *)

val call_return : t -> t
(** Semantics of return method *)

val pp_summary : Format.formatter -> summary -> unit

val get_summary : is_void_func:bool -> t -> summary

val check_required_props :
     check_on_string_set:(Typ.name -> Location.t -> MethodCallPrefix.t list -> String.Set.t -> unit)
  -> summary
  -> summary
