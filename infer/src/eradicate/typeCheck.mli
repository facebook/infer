(*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)


(** Module type for the type checking functions. *)

type check_return_type =
  Procname.t -> Cfg.Procdesc.t -> Sil.typ -> Sil.typ option -> Location.t -> unit

type find_canonical_duplicate = Cfg.Node.t -> Cfg.Node.t

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
  get_proc_desc -> Procname.t -> Cfg.Procdesc.t ->
  find_canonical_duplicate -> Annotations.annotated_signature -> 'a TypeState.t ->
  Cfg.Node.t -> Printer.LineReader.t -> 'a TypeState.t list * 'a TypeState.t list
