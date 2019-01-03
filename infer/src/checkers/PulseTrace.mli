(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

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

val pp : Format.formatter -> t -> unit

val make_errlog_trace : depth:int -> t -> Errlog.loc_trace

val pp_interesting_events : Format.formatter -> t -> unit
