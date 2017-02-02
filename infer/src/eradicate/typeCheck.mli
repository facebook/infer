(*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd


(** Module type for the type checking functions. *)

type check_return_type =
  Procname.t -> Procdesc.t -> Typ.t -> Typ.t option -> Location.t -> unit

type find_canonical_duplicate = Procdesc.Node.t -> Procdesc.Node.t

type get_proc_desc = TypeState.get_proc_desc

type checks =
  {
    eradicate : bool;
    check_extension : bool;
    check_ret_type : check_return_type list;
  }

val typecheck_node :
  Tenv.t -> 'a TypeState.ext ->
  bool ref -> checks -> Idenv.t ->
  get_proc_desc -> Procname.t -> Procdesc.t ->
  find_canonical_duplicate -> AnnotatedSignature.t -> 'a TypeState.t ->
  Procdesc.Node.t -> Printer.LineReader.t -> 'a TypeState.t list * 'a TypeState.t list
