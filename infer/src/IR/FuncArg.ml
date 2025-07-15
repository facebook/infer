(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Little abstraction over arguments + polymorphic payload *)

open! IStd
module L = Logging

type 'arg_payload t = {exp: Exp.t; typ: Typ.t; arg_payload: 'arg_payload}

let typ {typ} = typ

let exp {exp} = exp

let arg_payload {arg_payload} = arg_payload

let is_var {exp} = match exp with Var _ -> true | _ -> false

let map_payload ~f ({arg_payload} as func_arg) = {func_arg with arg_payload= f arg_payload}

let get_var_exn {exp; typ} =
  match exp with
  | Exp.Var v ->
      v
  | e ->
      L.(die InternalError) "Expected Lvar, got %a:%a" Exp.pp e (Typ.pp Pp.text) typ
