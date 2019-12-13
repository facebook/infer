(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Access path + its parent procedure *)
module LocalAccessPath : sig
  type t = private {access_path: AccessPath.t; parent: Typ.Procname.t} [@@deriving compare]

  val make : AccessPath.t -> Typ.Procname.t -> t

  val make_from_access_expression : HilExp.AccessExpression.t -> Typ.Procname.t -> t
end

val suffixes : String.Set.t

(** Called procedure & location + its receiver *)
module MethodCallPrefix : sig
  type t = private
    {receiver: LocalAccessPath.t; prefix: string; procname: Typ.Procname.t; location: Location.t}

  val make : LocalAccessPath.t -> Typ.Procname.t -> Location.t -> t
end

module Mem : sig
  type t

  val contains_build : t -> bool
end

include AbstractDomain.S

val subst :
     formals:(Pvar.t * Typ.t) list
  -> actuals:HilExp.t list
  -> ret_id_typ:AccessPath.base
  -> caller_pname:Typ.Procname.t
  -> callee_pname:Typ.Procname.t
  -> caller:t
  -> callee:Mem.t
  -> t

(** type for saving in summary payload *)
type summary = Mem.t

val init : Tenv.t -> Typ.Procname.t -> (Pvar.t * Typ.t) list -> t

val assign : lhs:LocalAccessPath.t -> rhs:LocalAccessPath.t -> t -> t

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
     check_on_string_set:(Typ.name -> MethodCallPrefix.t list -> String.Set.t -> unit)
  -> summary
  -> summary
