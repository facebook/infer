(*
 * Copyright (c) 2014-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module type for the type checking functions. *)

type check_return_type =
  Typ.Procname.t -> Procdesc.t -> Typ.t -> Typ.t option -> Location.t -> unit

type find_canonical_duplicate = Procdesc.Node.t -> Procdesc.Node.t

type checks = {eradicate: bool; check_ret_type: check_return_type list}

val typecheck_node :
     Tenv.t
  -> bool ref
  -> checks
  -> Idenv.t
  -> Typ.Procname.t
  -> Procdesc.t
  -> find_canonical_duplicate
  -> AnnotatedSignature.t
  -> TypeState.t
  -> Procdesc.Node.t
  -> Printer.LineReader.t
  -> TypeState.t list * TypeState.t list
