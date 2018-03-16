(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** The Smallfoot Intermediate Language: Decompiled Expressions *)

open! IStd
module L = Logging
module F = Format

(** expression representing the result of decompilation *)
type t =
  | Darray of t * t
  | Dbinop of Binop.t * t * t
  | Dconst of Const.t
  | Dsizeof of Typ.t * t option * Subtype.t
  | Dderef of t
  | Dfcall of t * t list * Location.t * CallFlags.t
  | Darrow of t * Typ.Fieldname.t
  | Ddot of t * Typ.Fieldname.t
  | Dpvar of Pvar.t
  | Dpvaraddr of Pvar.t
  | Dunop of Unop.t * t
  | Dunknown
  | Dretcall of t * t list * Location.t * CallFlags.t

(** Value paths: identify an occurrence of a value in a symbolic heap
    each expression represents a path, with Dpvar being the simplest one *)
type vpath = t option

let eradicate_java () = Config.eradicate && Language.curr_language_is Java

(** convert a dexp to a string *)
let rec to_string = function
  | Darray (de1, de2) ->
      to_string de1 ^ "[" ^ to_string de2 ^ "]"
  | Dbinop (op, de1, de2) ->
      "(" ^ to_string de1 ^ Binop.str Pp.text op ^ to_string de2 ^ ")"
  | Dconst Cfun pn
    -> (
      let procname_str = Typ.Procname.to_simplified_string pn in
      match pn with
      | Typ.Procname.ObjC_Cpp {kind= ObjCInstanceMethod}
      | Typ.Procname.ObjC_Cpp {kind= ObjCClassMethod} -> (
        match String.lsplit2 ~on:':' procname_str with
        | Some (base_name, _) ->
            base_name
        | None ->
            procname_str )
      | _ ->
          procname_str )
  | Dconst c ->
      Const.to_string c
  | Dderef de ->
      "*" ^ to_string de
  | Dfcall (fun_dexp, args, _, {cf_virtual= isvirtual}) ->
      let pp_arg fmt de = F.fprintf fmt "%s" (to_string de) in
      let pp_args fmt des =
        if eradicate_java () then ( if des <> [] then F.fprintf fmt "..." )
        else Pp.comma_seq pp_arg fmt des
      in
      let pp_fun fmt = function
        | Dconst Cfun pname ->
            let s =
              match pname with
              | Typ.Procname.Java pname_java ->
                  Typ.Procname.Java.get_method pname_java
              | _ ->
                  Typ.Procname.to_string pname
            in
            F.fprintf fmt "%s" s
        | de ->
            F.fprintf fmt "%s" (to_string de)
      in
      let receiver, args' =
        match args with
        | (Dpvar pv) :: args' when isvirtual && Pvar.is_this pv ->
            (None, args')
        | a :: args' when isvirtual ->
            (Some a, args')
        | _ ->
            (None, args)
      in
      let pp fmt =
        let pp_receiver fmt = function None -> () | Some arg -> F.fprintf fmt "%a." pp_arg arg in
        F.fprintf fmt "%a%a(%a)" pp_receiver receiver pp_fun fun_dexp pp_args args'
      in
      F.asprintf "%t" pp
  | Darrow (Dpvar pv, f) when Pvar.is_this pv ->
      (* this->fieldname *)
      Typ.Fieldname.to_simplified_string f
  | Darrow (de, f) ->
      if Language.curr_language_is Java then to_string de ^ "." ^ Typ.Fieldname.to_flat_string f
      else to_string de ^ "->" ^ Typ.Fieldname.to_string f
  | Ddot (Dpvar _, fe) when eradicate_java () ->
      (* static field access *)
      Typ.Fieldname.to_simplified_string fe
  | Ddot (de, f) ->
      if Language.curr_language_is Java then to_string de ^ "." ^ Typ.Fieldname.to_flat_string f
      else to_string de ^ "." ^ Typ.Fieldname.to_string f
  | Dpvar pv ->
      Mangled.to_string (Pvar.get_name pv)
  | Dpvaraddr pv ->
      let s =
        if eradicate_java () then Pvar.get_simplified_name pv
        else Mangled.to_string (Pvar.get_name pv)
      in
      let ampersand = if eradicate_java () then "" else "&" in
      ampersand ^ s
  | Dunop (op, de) ->
      Unop.str op ^ to_string de
  | Dsizeof (typ, _, _) ->
      F.asprintf "%a" (Typ.pp_full Pp.text) typ
  | Dunknown ->
      "unknown"
  | Dretcall (de, _, _, _) ->
      "returned by " ^ to_string de


(** Pretty print a dexp. *)
let pp fmt de = F.fprintf fmt "%s" (to_string de)

(** Pretty print a value path *)
let pp_vpath pe fmt vpath =
  let pp fmt = function Some de -> pp fmt de | None -> () in
  if Pp.equal_print_kind pe.Pp.kind Pp.HTML then
    F.fprintf fmt " %a{vpath: %a}%a" Io_infer.Html.pp_start_color Pp.Orange pp vpath
      Io_infer.Html.pp_end_color ()
  else F.fprintf fmt "%a" pp vpath


let rec has_tmp_var = function
  | Dpvar pvar | Dpvaraddr pvar ->
      Pvar.is_frontend_tmp pvar
  | Dderef dexp | Ddot (dexp, _) | Darrow (dexp, _) | Dunop (_, dexp) | Dsizeof (_, Some dexp, _) ->
      has_tmp_var dexp
  | Darray (dexp1, dexp2) | Dbinop (_, dexp1, dexp2) ->
      has_tmp_var dexp1 || has_tmp_var dexp2
  | Dretcall (dexp, dexp_list, _, _) | Dfcall (dexp, dexp_list, _, _) ->
      has_tmp_var dexp || List.exists ~f:has_tmp_var dexp_list
  | Dconst _ | Dunknown | Dsizeof (_, None, _) ->
      false
