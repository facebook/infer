(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Ast = ErlangAst

type module_name = string [@@deriving sexp_of]

type absent = Absent

type 'a present = Present of 'a

module UnqualifiedFunction : sig
  module T : sig
    type t = {name: string; arity: int} [@@deriving sexp, compare]
  end

  include module type of Comparable.Make (T)

  val of_ast : Ast.function_ -> T.t
end

type record_field_info = {index: int; initializer_: Ast.expression option} [@@deriving sexp_of]

type record_info = {field_names: string list; field_info: record_field_info String.Map.t}
[@@deriving sexp_of]

type ('procdesc, 'result) t =
  { current_module: module_name  (** used to qualify function names *)
  ; exports: UnqualifiedFunction.Set.t  (** used to determine public/private access *)
  ; imports: module_name UnqualifiedFunction.Map.t  (** used to resolve function names *)
  ; records: record_info String.Map.t  (** used to get fields, indexes and initializers *)
  ; location: Location.t  (** used to tag nodes and instructions being created *)
  ; procdesc: ('procdesc[@sexp.opaque])
  ; result: ('result[@sexp.opaque]) }
[@@deriving sexp_of]

val get_environment : Ast.form list -> (absent, absent) t

val typ_of_name : ErlangTypeName.t -> Typ.t

val ptr_typ_of_name : ErlangTypeName.t -> Typ.t
