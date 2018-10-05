(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

exception UnmatchedParameters

val with_formals_types :
  ?has_clang_model:bool -> Procdesc.t -> Typ.Procname.t -> (Exp.t * Typ.t) list -> Procdesc.t
(** Creates a copy of a procedure description and a list of type substitutions of the form
    (name, typ) where name is a parameter. The resulting procdesc is isomorphic but
    all the type of the parameters are replaced in the instructions according to the list.
    The virtual calls are also replaced to match the parameter types *)

val with_block_args : Procdesc.t -> Typ.Procname.t -> Exp.closure option list -> Procdesc.t
(** Creates a copy of a procedure description given a list of possible closures
  that are passed as arguments to the method. The resulting procdesc is isomorphic but
  a) the block parameters are replaces with the closures
  b) the parameters of the method are extended with parameters for the captured variables
  in the closures *)
