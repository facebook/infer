(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

exception UnmatchedParameters

val with_formals_types : Procdesc.t -> Procname.t -> (Exp.t * Typ.t) list -> Procdesc.t
(** Creates a copy of a procedure description and a list of type substitutions of the form (name,
    typ) where name is a parameter. The resulting procdesc is isomorphic but all the type of the
    parameters are replaced in the instructions according to the list. The virtual calls are also
    replaced to match the parameter types *)
