(*
* Copyright (c) 2014 - Facebook. All rights reserved.
*)


(** Module type for the type checking functions. *)

type check_return_type =
  Procname.t -> Cfg.Procdesc.t -> Sil.typ -> Sil.typ option -> Sil.location -> unit

type find_canonical_duplicate = Cfg.Node.t -> Cfg.Node.t

type get_proc_desc = TypeState.get_proc_desc

type checks =
  {
    eradicate : bool;
    check_extension : bool;
    check_ret_type : check_return_type list;
  }

val typecheck_node :
'a TypeState.ext ->
bool ref -> checks -> Idenv.t ->
get_proc_desc -> Procname.t -> Cfg.Procdesc.t ->
find_canonical_duplicate -> Annotations.annotated_signature -> 'a TypeState.t ->
Cfg.Node.t -> Printer.LineReader.t -> 'a TypeState.t list * 'a TypeState.t list
