(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t =
  | Reachable of {proc_name: Procname.t}
  | Extends of {typ: Typ.Name.t; typ_super: Typ.Name.t}
  | Cast of {proc_name: Procname.t; dest: Ident.t; src: Ident.t; dest_typ: Typ.t}
  | Alloc of {proc_name: Procname.t; return: Ident.t; allocation_site: string; typ: Typ.t}
  | VirtualCall of {proc_name: Procname.t; call_site: string; return: Ident.t; call_proc: Procname.t}
  | StaticCall of {proc_name: Procname.t; call_site: string; call_proc: Procname.t}

let fact_types = ["Reachable"; "Extends"; "Cast"; "Alloc"; "VirtualCall"; "StaticCall"]

let pp fmt = function
  | Reachable {proc_name} ->
      F.fprintf fmt "Reachable %s" (Procname.to_unique_id proc_name)
  | Extends {typ; typ_super} ->
      F.fprintf fmt "Extends %s %s" (Typ.Name.name typ) (Typ.Name.name typ_super)
  | Cast {proc_name; dest; src; dest_typ} ->
      F.fprintf fmt "Cast %s %s %s %s" (Procname.to_unique_id proc_name) (Ident.to_string dest)
        (Ident.to_string src) (Typ.to_string dest_typ)
  | Alloc {proc_name; return; allocation_site; typ} ->
      F.fprintf fmt "Alloc %s %s %s %s" (Procname.to_unique_id proc_name) (Ident.to_string return)
        allocation_site (Typ.to_string typ)
  | VirtualCall {proc_name; call_site; return; call_proc} ->
      F.fprintf fmt "VirtualCall %s %s %s %s" (Procname.to_unique_id proc_name) call_site
        (Ident.to_string return) (Procname.to_unique_id call_proc)
  | StaticCall {proc_name; call_site; call_proc} ->
      F.fprintf fmt "StaticCall %s %s %s" (Procname.to_unique_id proc_name) call_site
        (Procname.to_unique_id call_proc)


(** Generate a hash to uniquely identify an allocation or call site. The id of the retunred var is
    included because Java doesn't have information about columns. *)
let make_site proc_name loc id =
  match Procname.get_class_name proc_name with
  | Some class_name ->
      F.asprintf "%s:%s:%s" class_name (Location.to_string loc) (Ident.to_string id)
  | None ->
      F.asprintf "%a:%s" Location.pp_file_pos loc (Ident.to_string id)


let to_string fact = F.asprintf "%a" pp fact

let iter_fact_types f = List.iter fact_types ~f

let reachable proc_name = Reachable {proc_name}

let extends typ typ_super = Extends {typ; typ_super}

let cast proc_name dest src dest_typ = Cast {proc_name; dest; src; dest_typ}

let alloc proc_name return loc typ =
  Alloc {proc_name; return; allocation_site= make_site proc_name loc return; typ}


let virtual_call proc_name loc return call_proc =
  VirtualCall {proc_name; return; call_site= make_site proc_name loc return; call_proc}


let static_call proc_name loc return call_proc =
  StaticCall {proc_name; call_site= make_site proc_name loc return; call_proc}
