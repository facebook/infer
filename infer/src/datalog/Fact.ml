(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t =
  | EntryPoint of {proc_name: Procname.t}
  | Extends of {typ: Typ.Name.t; typ_super: Typ.Name.t}
  | Cast of {proc_name: Procname.t; dest: Ident.t; src: Ident.t; dest_typ: Typ.t}
  | Alloc of {proc_name: Procname.t; return: Ident.t; allocation_site: string; typ: Typ.t}
  | VirtualCall of
      {proc_name: Procname.t; call_site: string; receiver: Ident.t; proc_signature: string}
  | StaticCall of {proc_name: Procname.t; call_site: string; call_proc: Procname.t}
  | ActualArg of {proc_name: Procname.t; call_site: string; n_arg: int; arg: Ident.t}
  | FormalArg of {proc_name: Procname.t; n_arg: int; arg: Ident.t}
  | ActualReturn of {proc_name: Procname.t; call_site: string; return: Ident.t}
  | FormalReturn of {proc_name: Procname.t; return: Ident.t}
  | Implem of {typ: Typ.Name.t; proc_signature: string}
  | LoadField of {proc_name: Procname.t; dest: Ident.t; src: Ident.t; src_field: Fieldname.t}
  | StoreField of {proc_name: Procname.t; dest: Ident.t; dest_field: Fieldname.t; src: Ident.t}

let fact_types =
  [ "EntryPoint"
  ; "Extends"
  ; "Cast"
  ; "Alloc"
  ; "VirtualCall"
  ; "StaticCall"
  ; "ActualArg"
  ; "FormalArg"
  ; "ActualReturn"
  ; "FormalReturn"
  ; "Implem"
  ; "LoadField"
  ; "StoreField" ]


let unique_proc_id ?(withclass = true) proc_name =
  let j_proc_name = Procname.as_java_exn proc_name ~explanation:"Only Java procdesc supported" in
  let pp_typ = Typ.pp_java ~verbose:true in
  let pp_param_list = Pp.seq ~sep:"," pp_typ in
  F.asprintf "%s%s(%a):%a"
    (if withclass then Procname.Java.get_class_name j_proc_name ^ "." else "")
    (Procname.Java.get_method j_proc_name)
    pp_param_list
    (Procname.Java.get_parameters j_proc_name)
    pp_typ
    (Procname.Java.get_return_typ j_proc_name)


let pp fmt = function
  | EntryPoint {proc_name} ->
      F.fprintf fmt "EntryPoint %s" (unique_proc_id proc_name)
  | Extends {typ; typ_super} ->
      F.fprintf fmt "Extends %s %s" (Typ.Name.name typ) (Typ.Name.name typ_super)
  | Cast {proc_name; dest; src; dest_typ} ->
      F.fprintf fmt "Cast %s %s %s %s" (unique_proc_id proc_name) (Ident.to_string dest)
        (Ident.to_string src) (Typ.to_string dest_typ)
  | Alloc {proc_name; return; allocation_site; typ} ->
      F.fprintf fmt "Alloc %s %s %s %s" (unique_proc_id proc_name) (Ident.to_string return)
        allocation_site (Typ.to_string typ)
  | VirtualCall {proc_name; call_site; receiver; proc_signature} ->
      F.fprintf fmt "VirtualCall %s %s %s %s" (unique_proc_id proc_name) call_site
        (Ident.to_string receiver) proc_signature
  | StaticCall {proc_name; call_site; call_proc} ->
      F.fprintf fmt "StaticCall %s %s %s" (unique_proc_id proc_name) call_site
        (unique_proc_id call_proc)
  | ActualArg {proc_name; call_site; n_arg; arg} ->
      F.fprintf fmt "ActualArg %s %s %d %s" (unique_proc_id proc_name) call_site n_arg
        (Ident.to_string arg)
  | FormalArg {proc_name; n_arg; arg} ->
      F.fprintf fmt "FormalArg %s %d %s" (unique_proc_id proc_name) n_arg (Ident.to_string arg)
  | ActualReturn {proc_name; call_site; return} ->
      F.fprintf fmt "ActualReturn %s %s %s" (unique_proc_id proc_name) call_site
        (Ident.to_string return)
  | FormalReturn {proc_name; return} ->
      F.fprintf fmt "FormalReturn %s %s" (unique_proc_id proc_name) (Ident.to_string return)
  | Implem {typ; proc_signature} ->
      F.fprintf fmt "Implem %s %s" (Typ.Name.name typ) proc_signature
  | LoadField {proc_name; dest; src; src_field} ->
      F.fprintf fmt "LoadField %s %s %s %s" (unique_proc_id proc_name) (Ident.to_string dest)
        (Ident.to_string src) (Fieldname.to_string src_field)
  | StoreField {proc_name; dest; dest_field; src} ->
      F.fprintf fmt "StoreField %s %s %s %s" (unique_proc_id proc_name) (Ident.to_string dest)
        (Fieldname.to_string dest_field) (Ident.to_string src)


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

let entrypoint proc_name = EntryPoint {proc_name}

let extends typ typ_super = Extends {typ; typ_super}

let cast proc_name dest src dest_typ = Cast {proc_name; dest; src; dest_typ}

let alloc proc_name return loc typ =
  Alloc {proc_name; return; allocation_site= make_site proc_name loc return; typ}


let virtual_call proc_name loc return call_proc receiver =
  VirtualCall
    { proc_name
    ; receiver
    ; call_site= make_site proc_name loc return
    ; proc_signature= unique_proc_id ~withclass:false call_proc }


let static_call proc_name loc return call_proc =
  StaticCall {proc_name; call_site= make_site proc_name loc return; call_proc}


let actual_arg proc_name loc return n_arg arg =
  ActualArg {proc_name; call_site= make_site proc_name loc return; n_arg; arg}


let formal_arg proc_name n_arg arg = FormalArg {proc_name; n_arg; arg}

let actual_return proc_name loc return =
  ActualReturn {proc_name; call_site= make_site proc_name loc return; return}


let formal_return proc_name return = FormalReturn {proc_name; return}

let implem typ proc_name = Implem {typ; proc_signature= unique_proc_id ~withclass:false proc_name}

let load_field proc_name dest src src_field = LoadField {proc_name; dest; src; src_field}

let store_field proc_name dest dest_field src = StoreField {proc_name; dest; dest_field; src}
