(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type breadcrumb =
  | VariableDeclaration of Location.t
  | Assignment of {lhs: HilExp.AccessExpression.t; location: Location.t}
  | Capture of
      { captured_as: AccessPath.base
      ; captured: HilExp.AccessExpression.t
      ; location: Location.t }
  | Call of
      { f: [`HilCall of HilInstr.call | `Model of string]
      ; actuals: HilExp.t list
      ; location: Location.t }

type t = breadcrumb list [@@deriving compare]

val pp : F.formatter -> t -> unit

val make_errlog_trace : depth:int -> t -> Errlog.loc_trace

val pp_interesting_events : F.formatter -> t -> unit

type 'a action =
  | Immediate of {imm: 'a; location: Location.t}
  | ViaCall of {action: 'a action; proc_name: Typ.Procname.t; location: Location.t}
[@@deriving compare]

val pp_action : (F.formatter -> 'a -> unit) -> F.formatter -> 'a action -> unit

val immediate_of_action : 'a action -> 'a

val outer_location_of_action : 'a action -> Location.t

val trace_of_action :
  action_name:string -> (F.formatter -> 'a -> unit) -> 'a action -> Errlog.loc_trace_elem sexp_list
