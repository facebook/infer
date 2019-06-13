(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type breadcrumb =
  | VariableDeclaration of Location.t
  | CppTemporaryCreated of Location.t
  | Assignment of {lhs: HilExp.AccessExpression.t; location: Location.t}
  | Capture of
      { captured_as: AccessPath.base
      ; captured: HilExp.AccessExpression.t
      ; location: Location.t }
  | Call of
      { f: [`HilCall of HilInstr.call | `Model of string]
      ; actuals: HilExp.t list
      ; location: Location.t }

type breadcrumbs = breadcrumb list [@@deriving compare]

val pp_breadcrumbs : F.formatter -> breadcrumbs -> unit

val add_errlog_of_breadcrumbs :
  nesting:int -> breadcrumbs -> Errlog.loc_trace_elem list -> Errlog.loc_trace_elem list

type 'a action =
  | Immediate of {imm: 'a; location: Location.t}
  | ViaCall of {action: 'a action; proc_name: Typ.Procname.t; location: Location.t}
[@@deriving compare]

val pp_action : (F.formatter -> 'a -> unit) -> F.formatter -> 'a action -> unit

val immediate_of_action : 'a action -> 'a

val outer_location_of_action : 'a action -> Location.t

type 'a t = {action: 'a action; breadcrumbs: breadcrumbs} [@@deriving compare]

val pp : (F.formatter -> 'a -> unit) -> F.formatter -> 'a t -> unit

val add_to_errlog :
     header:string
  -> (F.formatter -> 'a -> unit)
  -> 'a t
  -> Errlog.loc_trace_elem list
  -> Errlog.loc_trace_elem list
