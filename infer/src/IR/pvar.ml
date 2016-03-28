(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** The Smallfoot Intermediate Language *)

module L = Logging
module F = Format

(** Kind of global variables *)
type pvar_kind =
  | Local_var of Procname.t (** local variable belonging to a function *)
  | Callee_var of Procname.t (** local variable belonging to a callee *)
  | Abducted_retvar of Procname.t * Location.t (** synthetic variable to represent return value *)
  | Abducted_ref_param of Procname.t * t * Location.t
  (** synthetic variable to represent param passed by reference *)
  | Global_var (** gloval variable *)
  | Seed_var (** variable used to store the initial value of formal parameters *)

(** Names for program variables. *)
and t =
  { pv_name: Mangled.t;
    pv_kind: pvar_kind }

let rec pvar_kind_compare k1 k2 = match k1, k2 with
  | Local_var n1, Local_var n2 -> Procname.compare n1 n2
  | Local_var _, _ -> - 1
  | _, Local_var _ -> 1
  | Callee_var n1, Callee_var n2 -> Procname.compare n1 n2
  | Callee_var _, _ -> - 1
  | _, Callee_var _ -> 1
  | Abducted_retvar (p1, l1), Abducted_retvar (p2, l2) ->
      let n = Procname.compare p1 p2 in
      if n <> 0 then n else Location.compare l1 l2
  | Abducted_retvar _, _ -> - 1
  | _, Abducted_retvar _ -> 1
  | Abducted_ref_param (p1, pv1, l1), Abducted_ref_param (p2, pv2, l2) ->
      let n = Procname.compare p1 p2 in
      if n <> 0 then n else
        let n = compare pv1 pv2 in
        if n <> 0 then n else Location.compare l1 l2
  | Abducted_ref_param _, _ -> - 1
  | _, Abducted_ref_param _ -> 1
  | Global_var, Global_var -> 0
  | Global_var, _ -> - 1
  | _, Global_var -> 1
  | Seed_var, Seed_var -> 0

and compare pv1 pv2 =
  let n = Mangled.compare pv1.pv_name pv2.pv_name in
  if n <> 0 then n else pvar_kind_compare pv1.pv_kind pv2.pv_kind

let equal pvar1 pvar2 =
  compare pvar1 pvar2 = 0


let rec _pp f pv =
  let name = pv.pv_name in
  match pv.pv_kind with
  | Local_var n ->
      if !Config.pp_simple then F.fprintf f "%a" Mangled.pp name
      else F.fprintf f "%a$%a" Procname.pp n Mangled.pp name
  | Callee_var n ->
      if !Config.pp_simple then F.fprintf f "%a|callee" Mangled.pp name
      else F.fprintf f "%a$%a|callee" Procname.pp n Mangled.pp name
  | Abducted_retvar (n, l) ->
      if !Config.pp_simple then F.fprintf f "%a|abductedRetvar" Mangled.pp name
      else F.fprintf f "%a$%a%a|abductedRetvar" Procname.pp n Location.pp l Mangled.pp name
  | Abducted_ref_param (n, pv, l) ->
      if !Config.pp_simple then F.fprintf f "%a|%a|abductedRefParam" _pp pv Mangled.pp name
      else F.fprintf f "%a$%a%a|abductedRefParam" Procname.pp n Location.pp l Mangled.pp name
  | Global_var -> F.fprintf f "#GB$%a" Mangled.pp name
  | Seed_var -> F.fprintf f "old_%a" Mangled.pp name

(** Pretty print a program variable in latex. *)
let pp_latex f pv =
  let name = pv.pv_name in
  match pv.pv_kind with
  | Local_var _ ->
      Latex.pp_string Latex.Roman f (Mangled.to_string name)
  | Callee_var _ ->
      F.fprintf f "%a_{%a}" (Latex.pp_string Latex.Roman) (Mangled.to_string name)
        (Latex.pp_string Latex.Roman) "callee"
  | Abducted_retvar _ ->
      F.fprintf f "%a_{%a}" (Latex.pp_string Latex.Roman) (Mangled.to_string name)
        (Latex.pp_string Latex.Roman) "abductedRetvar"
  | Abducted_ref_param _ ->
      F.fprintf f "%a_{%a}" (Latex.pp_string Latex.Roman) (Mangled.to_string name)
        (Latex.pp_string Latex.Roman) "abductedRefParam"
  | Global_var ->
      Latex.pp_string Latex.Boldface f (Mangled.to_string name)
  | Seed_var ->
      F.fprintf f "%a^{%a}" (Latex.pp_string Latex.Roman) (Mangled.to_string name)
        (Latex.pp_string Latex.Roman) "old"

(** Pretty print a pvar which denotes a value, not an address *)
let pp_value pe f pv =
  match pe.pe_kind with
  | PP_TEXT -> _pp f pv
  | PP_HTML -> _pp f pv
  | PP_LATEX -> pp_latex f pv

(** Pretty print a program variable. *)
let pp pe f pv =
  let ampersand = match pe.pe_kind with
    | PP_TEXT -> "&"
    | PP_HTML -> "&amp;"
    | PP_LATEX -> "\\&" in
  F.fprintf f "%s%a" ampersand (pp_value pe) pv

(** Dump a program variable. *)
let d (pvar: t) = L.add_print_action (L.PTpvar, Obj.repr pvar)

(** Pretty print a list of program variables. *)
let pp_list pe f pvl =
  F.fprintf f "%a" (pp_seq (fun f e -> F.fprintf f "%a" (pp pe) e)) pvl

(** Dump a list of program variables. *)
let d_list pvl =
  IList.iter (fun pv -> d pv; L.d_str " ") pvl

let get_name pv = pv.pv_name

let to_string pv = Mangled.to_string pv.pv_name

let get_simplified_name pv =
  let s = Mangled.to_string pv.pv_name in
  match string_split_character s '.' with
  | Some s1, s2 ->
      (match string_split_character s1 '.' with
       | Some _, s4 -> s4 ^ "." ^ s2
       | _ -> s)
  | _ -> s

(** Check if the pvar is an abucted return var or param passed by ref *)
let is_abducted pv =
  match pv.pv_kind with
  | Abducted_retvar _ | Abducted_ref_param _ -> true
  | _ -> false

(** Turn a pvar into a seed pvar (which stored the initial value) *)
let to_seed pv = { pv with pv_kind = Seed_var }

(** Check if the pvar is a local var *)
let is_local pv =
  match pv.pv_kind with
  | Local_var _ -> true
  | _ -> false

(** Check if the pvar is a callee var *)
let is_callee pv =
  match pv.pv_kind with
  | Callee_var _ -> true
  | _ -> false

(** Check if the pvar is a seed var *)
let is_seed pv =
  match pv.pv_kind with
  | Seed_var -> true
  | _ -> false

(** Check if the pvar is a global var *)
let is_global pv =
  pv.pv_kind = Global_var

(** Check if a pvar is the special "this" var *)
let is_this pvar =
  Mangled.equal (get_name pvar) (Mangled.from_string "this")

(** Check if the pvar is a return var *)
let is_return pv =
  get_name pv = Ident.name_return

(** Turn an ordinary program variable into a callee program variable *)
let to_callee pname pvar = match pvar.pv_kind with
  | Local_var _ ->
      { pvar with pv_kind = Callee_var pname }
  | Global_var ->
      pvar
  | Callee_var _ | Abducted_retvar _ | Abducted_ref_param _ | Seed_var ->
      L.d_str "Cannot convert pvar to callee: ";
      d pvar; L.d_ln ();
      assert false
(** [mk name proc_name] creates a program var with the given function name *)
let mk (name: Mangled.t) (proc_name: Procname.t) : t =
  { pv_name = name; pv_kind = Local_var proc_name }

let get_ret_pvar pname =
  mk Ident.name_return pname

(** [mk_callee name proc_name] creates a program var
    for a callee function with the given function name *)
let mk_callee (name: Mangled.t) (proc_name: Procname.t) : t =
  { pv_name = name; pv_kind = Callee_var proc_name }

(** create a global variable with the given name *)
let mk_global (name: Mangled.t) : t =
  { pv_name = name; pv_kind = Global_var }

(** create an abducted return variable for a call to [proc_name] at [loc] *)
let mk_abducted_ret (proc_name : Procname.t) (loc : Location.t) : t =
  let name = Mangled.from_string ("$RET_" ^ (Procname.to_unique_id proc_name)) in
  { pv_name = name; pv_kind = Abducted_retvar (proc_name, loc) }

let mk_abducted_ref_param (proc_name : Procname.t) (pv : t) (loc : Location.t) : t =
  let name = Mangled.from_string ("$REF_PARAM_" ^ (Procname.to_unique_id proc_name)) in
  { pv_name = name; pv_kind = Abducted_ref_param (proc_name, pv, loc) }
