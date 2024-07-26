(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The Smallfoot Intermediate Language *)

open! IStd
module L = Logging
module F = Format

type translation_unit = SourceFile.t option [@@deriving compare, equal, sexp, hash, normalize]

(** Kind of global variables *)
type pvar_kind =
  | Local_var of Procname.t  (** local variable belonging to a function *)
  | Callee_var of Procname.t  (** local variable belonging to a callee *)
  | Abduced_retvar of Procname.t * Location.t  (** synthetic variable to represent return value *)
  | Abduced_ref_param of Procname.t * int * Location.t
      (** synthetic variable to represent param passed by reference *)
  | Global_var of
      { translation_unit: translation_unit
      ; template_args: Typ.template_spec_info
      ; is_constexpr: bool  (** is it compile constant? *)
      ; is_ice: bool  (** is it integral constant expression? *)
      ; is_pod: bool
      ; is_static_local: bool
      ; is_static_global: bool
      ; is_constant_array: bool
      ; is_const: bool }  (** global variable *)
  | Seed_var  (** variable used to store the initial value of formal parameters *)
[@@deriving compare, equal, sexp, hash, normalize]

(** Names for program variables. *)
type t = {pv_hash: int; pv_name: Mangled.t; pv_kind: pvar_kind; pv_tmp_id: Ident.t option [@ignore]}
[@@deriving compare, equal, sexp, hash, normalize]

let yojson_of_t {pv_name} = [%yojson_of: Mangled.t] pv_name

let compare_modulo_this x y =
  if phys_equal x y then 0
  else
    let cmp = Int.compare x.pv_hash y.pv_hash in
    if not (Int.equal 0 cmp) then cmp
    else
      let cmp = Mangled.compare x.pv_name y.pv_name in
      if not (Int.equal 0 cmp) then cmp
      else if Mangled.is_this x.pv_name || Mangled.is_self x.pv_name then 0
      else compare_pvar_kind x.pv_kind y.pv_kind


let get_declaring_function pv =
  match pv.pv_kind with
  | Local_var n | Callee_var n | Abduced_retvar (n, _) | Abduced_ref_param (n, _, _) ->
      Some n
  | Global_var _ | Seed_var ->
      None


let pp_translation_unit fmt = function None -> () | Some fname -> SourceFile.pp fmt fname

let get_name pv = pv.pv_name

let to_string pv = Mangled.to_string pv.pv_name

let get_simplified_name pv =
  let s = Mangled.to_string pv.pv_name in
  let s =
    match String.rsplit2 s ~on:'.' with
    | Some (s1, s2) -> (
      match pv with
      | {pv_kind= Global_var _} when Language.curr_language_is Language.Clang ->
          s2
      | _ -> (
        match String.rsplit2 s1 ~on:'.' with Some (_, s4) -> Printf.sprintf "%s.%s" s4 s2 | _ -> s )
      )
    | _ ->
        s
  in
  match pv with
  | {pv_kind= Global_var {template_args}} ->
      F.asprintf "%s%a" s (Typ.pp_template_spec_info Pp.text) template_args
  | _ ->
      s


(** Check if the pvar is an abducted return var or param passed by ref *)
let is_abduced pv =
  match pv.pv_kind with Abduced_retvar _ | Abduced_ref_param _ -> true | _ -> false


(** Turn a pvar into a seed pvar (which stored the initial value) *)
let to_seed pv = {pv with pv_kind= Seed_var}

(** Check if the pvar is a local var *)
let is_local pv = match pv.pv_kind with Local_var _ -> true | _ -> false

(** Check if the pvar is a callee var *)
let is_callee pv = match pv.pv_kind with Callee_var _ -> true | _ -> false

(** Check if the pvar is a seed var *)
let is_seed pv = match pv.pv_kind with Seed_var -> true | _ -> false

(** Check if the pvar is a global var *)
let is_global pv = match pv.pv_kind with Global_var _ -> true | _ -> false

let is_static_companion {pv_name; pv_kind} =
  match pv_kind with
  | Global_var _ ->
      String.is_suffix (Mangled.to_string pv_name) ~suffix:HackClassName.static_suffix
  | _ ->
      false


let is_static_local pv =
  match pv.pv_kind with Global_var {is_static_local} -> is_static_local | _ -> false


let is_constant_array pv =
  match pv.pv_kind with Global_var {is_constant_array} -> is_constant_array | _ -> false


let is_const pv = match pv.pv_kind with Global_var {is_const} -> is_const | _ -> false

(** Check if a pvar is the special "this" var *)
let is_this pvar = Mangled.is_this (get_name pvar)

(** Check if a pvar is the special "self" var *)
let is_self pvar = Mangled.is_self (get_name pvar)

let is_artificial pvar = Mangled.is_artificial (get_name pvar)

(** Check if the pvar is a return var *)
let is_return pv = Mangled.equal (get_name pv) Ident.name_return

(** something that can't be part of a legal identifier in any conceivable language *)
let tmp_prefix = "0$?%__sil_tmp"

(** In case of a temporary variable, returns the id used to create it, or None otherwise. *)
let get_tmp_id pvar = pvar.pv_tmp_id

(** name given to variables representing C++ temporary objects *)
let materialized_cpp_temporary = "SIL_materialize_temp__"

let unnamed_param_prefix = "__param_"

(** return true if [pvar] is a temporary variable generated by the frontend *)
let is_frontend_tmp pvar =
  (* Check whether the program variable is a temporary one generated by Sawja, javac, or some other
     bytecode/name generation pass. valid java identifiers cannot contain `$` *)
  let is_bytecode_tmp name = String.contains name '$' || String.is_prefix ~prefix:"CatchVar" name in
  (* Check whether the program variable is generated by [mk_tmp] *)
  let is_sil_tmp name = String.is_prefix ~prefix:tmp_prefix name in
  let name = to_string pvar in
  is_sil_tmp name
  ||
  match pvar.pv_kind with
  | Local_var pname ->
      (Procname.is_java pname || Procname.is_csharp pname) && is_bytecode_tmp name
  | _ ->
      false


let tmp_clang_regex = Str.regexp "^__\\(range\\|begin\\|end\\)[0-9]*$"

let is_clang_tmp pvar = Str.string_match tmp_clang_regex (to_string pvar) 0

(* in Sawja, variables like $T0_18 are temporaries, but not SSA vars. *)
let is_ssa_frontend_tmp pvar =
  is_frontend_tmp pvar
  &&
  let name = to_string pvar in
  not (String.contains name '_' && String.contains name '$')


let is_cpp_temporary pvar =
  let name = to_string pvar in
  String.is_substring ~substring:materialized_cpp_temporary name


let is_cpp_unnamed_param pvar =
  let name = to_string pvar in
  String.is_substring ~substring:unnamed_param_prefix name


let is_gmock_param pvar =
  let name = to_string pvar in
  String.is_prefix ~prefix:"gmock" name


let pp_ ~verbose f pv =
  let name = pv.pv_name in
  match pv.pv_kind with
  | _ when (not verbose) && is_cpp_temporary pv ->
      F.pp_print_string f "C++ temporary"
  | _ when (not verbose) && is_clang_tmp pv ->
      F.pp_print_string f "compiler-generated variable"
  | _ when (not verbose) && is_frontend_tmp pv ->
      F.pp_print_string f "infer intermediate variable"
  | Local_var _ ->
      Mangled.pp f name
  | Callee_var _ ->
      Mangled.pp f name ;
      if verbose then F.pp_print_string f "|callee"
  | Abduced_retvar _ ->
      Mangled.pp f name ;
      if verbose then F.pp_print_string f "|abducedRetvar"
  | Abduced_ref_param (_, index, _) ->
      Mangled.pp f name ;
      if verbose then F.fprintf f "|abducedRefParam%d" index
  | Global_var {translation_unit; template_args; is_constexpr; is_ice; is_pod} ->
      if verbose then
        F.fprintf f "#GB<%a%s%s%s>$%a" pp_translation_unit translation_unit
          (if is_constexpr then "|const" else "")
          (if is_ice then "|ice" else "")
          (if not is_pod then "|!pod" else "")
          Mangled.pp name
      else F.fprintf f "%s" (get_simplified_name pv) ;
      (Typ.pp_template_spec_info Pp.text) f template_args
  | Seed_var ->
      F.fprintf f "old_%a" Mangled.pp name


(** Pretty print a pvar which denotes a value, not an address *)
let pp_value f pv = pp_ ~verbose:true f pv

(** Non-verbose version of pp_value *)
let pp_value_non_verbose f pv = pp_ ~verbose:false f pv

(** Pretty print a program variable. *)
let pp pe f pv =
  let ampersand = match pe.Pp.kind with TEXT -> "&" | HTML -> "&amp;" in
  F.fprintf f "%s%a" ampersand pp_value pv


let equal = [%compare.equal: t]

(** Dump a program variable. *)
let d (pvar : t) = L.d_pp_with_pe pp pvar

(** Turn an ordinary program variable into a callee program variable *)
let to_callee pname pvar =
  match pvar.pv_kind with
  | Local_var _ ->
      {pvar with pv_kind= Callee_var pname}
  | Global_var _ ->
      pvar
  | Callee_var _ ->
      pvar
  | Abduced_retvar _ | Abduced_ref_param _ | Seed_var ->
      L.d_str "Cannot convert pvar to callee: " ;
      d pvar ;
      L.d_ln () ;
      assert false


let name_hash (name : Mangled.t) = Mangled.hash name

(** [mk name proc_name] creates a program var with the given function name *)
let mk_with_tmp_id ~tmp_id (name : Mangled.t) (proc_name : Procname.t) : t =
  {pv_hash= name_hash name; pv_name= name; pv_kind= Local_var proc_name; pv_tmp_id= tmp_id}


let mk (name : Mangled.t) (proc_name : Procname.t) : t = mk_with_tmp_id ~tmp_id:None name proc_name

let get_ret_pvar pname = mk Ident.name_return pname

let get_ret_param_pvar pname = mk Mangled.return_param pname

(** [mk_callee name proc_name] creates a program var for a callee function with the given function
    name *)
let mk_callee (name : Mangled.t) (proc_name : Procname.t) : t =
  {pv_hash= name_hash name; pv_name= name; pv_kind= Callee_var proc_name; pv_tmp_id= None}


(** create a global variable with the given name *)
let mk_global ?(is_constexpr = false) ?(is_ice = false) ?(is_pod = true) ?(is_static_local = false)
    ?(is_static_global = false) ?(is_constant_array = false) ?(is_const = false) ?translation_unit
    ?(template_args = Typ.NoTemplate) (name : Mangled.t) : t =
  { pv_hash= name_hash name
  ; pv_name= name
  ; pv_tmp_id= None
  ; pv_kind=
      Global_var
        { translation_unit
        ; template_args
        ; is_constexpr
        ; is_ice
        ; is_pod
        ; is_static_local
        ; is_static_global
        ; is_constant_array
        ; is_const } }


(** create a fresh temporary variable local to procedure [pname]. for use in the frontends only! *)
let mk_tmp name pname =
  let tmp_id = Ident.create_fresh Ident.knormal in
  let pvar_mangled = Mangled.from_string (tmp_prefix ^ name ^ Ident.to_string tmp_id) in
  mk_with_tmp_id ~tmp_id:(Some tmp_id) pvar_mangled pname


(** create an abduced return variable for a call to [proc_name] at [loc] *)
let mk_abduced_ret (proc_name : Procname.t) (loc : Location.t) : t =
  let name = Mangled.from_string (F.asprintf "$RET_%a" Procname.pp_unique_id proc_name) in
  {pv_hash= name_hash name; pv_name= name; pv_kind= Abduced_retvar (proc_name, loc); pv_tmp_id= None}


let mk_abduced_ref_param (proc_name : Procname.t) (index : int) (loc : Location.t) : t =
  let name = Mangled.from_string (F.asprintf "$REF_PARAM_VAL_%a" Procname.pp_unique_id proc_name) in
  { pv_hash= name_hash name
  ; pv_name= name
  ; pv_kind= Abduced_ref_param (proc_name, index, loc)
  ; pv_tmp_id= None }


let get_translation_unit pvar =
  match pvar.pv_kind with
  | Global_var {translation_unit} ->
      translation_unit
  | _ ->
      L.(die InternalError) "Expected a global variable"


let is_compile_constant pvar =
  match pvar.pv_kind with Global_var {is_constexpr} -> is_constexpr | _ -> false


let is_ice pvar = match pvar.pv_kind with Global_var {is_ice} -> is_ice | _ -> false

let is_pod pvar = match pvar.pv_kind with Global_var {is_pod} -> is_pod | _ -> true

let get_initializer_pname {pv_name; pv_kind} =
  match pv_kind with
  | Global_var {translation_unit; template_args; is_static_global} ->
      let open IOption.Let_syntax in
      let qual_name =
        let plain_qual_name =
          Config.clang_initializer_prefix ^ Mangled.to_string_full pv_name
          |> QualifiedCppName.of_qual_string
        in
        if Typ.is_template_spec_info_empty template_args then plain_qual_name
        else
          let template_suffix = F.asprintf "%a" (Typ.pp_template_spec_info Pp.text) template_args in
          QualifiedCppName.append_template_args_to_last plain_qual_name ~args:template_suffix
      in
      let+ mangled =
        if is_static_global then
          let+ file = translation_unit in
          SourceFile.to_string file |> Utils.string_crc_hex32 |> Option.return
        else Some None
      in
      Procname.C (Procname.C.c qual_name ?mangled template_args)
  | _ ->
      None


let get_template_args pvar =
  match pvar.pv_kind with Global_var {template_args} -> template_args | _ -> Typ.NoTemplate


let is_objc_static_local_of_proc_name pname pvar =
  (* local static name is of the form procname_varname *)
  String.is_prefix (Mangled.to_string (get_name pvar)) ~prefix:pname


let is_block_pvar pvar =
  String.is_prefix (Mangled.to_string (get_name pvar)) ~prefix:Config.anonymous_block_prefix


module Set = PrettyPrintable.MakePPSet (struct
  type nonrec t = t [@@deriving compare]

  let pp = pp Pp.text
end)

module Map = PrettyPrintable.MakePPMap (struct
  type nonrec t = t [@@deriving compare]

  let pp = pp Pp.text
end)
