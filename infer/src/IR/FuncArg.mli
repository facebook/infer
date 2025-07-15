(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Little abstraction over arguments + polymorphic payload *)
open! IStd

type 'arg_payload t = {exp: Exp.t; typ: Typ.t; arg_payload: 'arg_payload}

val typ : 'arg_payload t -> Typ.t

val exp : 'arg_payload t -> Exp.t

val arg_payload : 'arg_payload t -> 'arg_payload

val is_var : 'arg_payload t -> bool

val map_payload : f:('a -> 'b) -> 'a t -> 'b t

val get_var_exn : 'arg_payload t -> Ident.t
