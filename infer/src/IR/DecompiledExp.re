/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 *
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! Utils;


/** The Smallfoot Intermediate Language: Decompiled Expressions */
let module L = Logging;

let module F = Format;


/** expression representing the result of decompilation */
type t =
  | Darray t t
  | Dbinop Binop.t t t
  | Dconst Const.t
  | Dsizeof Typ.t (option t) Subtype.t
  | Dderef t
  | Dfcall t (list t) Location.t CallFlags.t
  | Darrow t Ident.fieldname
  | Ddot t Ident.fieldname
  | Dpvar Pvar.t
  | Dpvaraddr Pvar.t
  | Dunop Unop.t t
  | Dunknown
  | Dretcall t (list t) Location.t CallFlags.t;


/** Value paths: identify an occurrence of a value in a symbolic heap
    each expression represents a path, with Dpvar being the simplest one */
type vpath = option t;

let java () => !Config.curr_language == Config.Java;

let eradicate_java () => Config.eradicate && java ();


/** convert a dexp to a string */
let rec to_string =
  fun
  | Darray de1 de2 => to_string de1 ^ "[" ^ to_string de2 ^ "]"
  | Dbinop op de1 de2 => "(" ^ to_string de1 ^ Binop.str pe_text op ^ to_string de2 ^ ")"
  | Dconst (Cfun pn) => Procname.to_simplified_string pn
  | Dconst c => Const.to_string c
  | Dderef de => "*" ^ to_string de
  | Dfcall fun_dexp args _ {cf_virtual: isvirtual} => {
      let pp_arg fmt de => F.fprintf fmt "%s" (to_string de);
      let pp_args fmt des =>
        if (eradicate_java ()) {
          if (des != []) {
            F.fprintf fmt "..."
          }
        } else {
          pp_comma_seq pp_arg fmt des
        };
      let pp_fun fmt => (
        fun
        | Dconst (Cfun pname) => {
            let s =
              switch pname {
              | Procname.Java pname_java => Procname.java_get_method pname_java
              | _ => Procname.to_string pname
              };
            F.fprintf fmt "%s" s
          }
        | de => F.fprintf fmt "%s" (to_string de)
      );
      let (receiver, args') =
        switch args {
        | [Dpvar pv, ...args'] when isvirtual && Pvar.is_this pv => (None, args')
        | [a, ...args'] when isvirtual => (Some a, args')
        | _ => (None, args)
        };
      let pp fmt () => {
        let pp_receiver fmt => (
          fun
          | None => ()
          | Some arg => F.fprintf fmt "%a." pp_arg arg
        );
        F.fprintf fmt "%a%a(%a)" pp_receiver receiver pp_fun fun_dexp pp_args args'
      };
      pp_to_string pp ()
    }
  | Darrow (Dpvar pv) f when Pvar.is_this pv =>
    /* this->fieldname */
    Ident.fieldname_to_simplified_string f
  | Darrow de f =>
    if (Ident.fieldname_is_hidden f) {
      to_string de
    } else if (java ()) {
      to_string de ^ "." ^ Ident.fieldname_to_flat_string f
    } else {
      to_string de ^ "->" ^ Ident.fieldname_to_string f
    }
  | Ddot (Dpvar _) fe when eradicate_java () =>
    /* static field access */
    Ident.fieldname_to_simplified_string fe
  | Ddot de f =>
    if (Ident.fieldname_is_hidden f) {
      "&" ^ to_string de
    } else if (java ()) {
      to_string de ^ "." ^ Ident.fieldname_to_flat_string f
    } else {
      to_string de ^ "." ^ Ident.fieldname_to_string f
    }
  | Dpvar pv => Mangled.to_string (Pvar.get_name pv)
  | Dpvaraddr pv => {
      let s =
        if (eradicate_java ()) {
          Pvar.get_simplified_name pv
        } else {
          Mangled.to_string (Pvar.get_name pv)
        };
      let ampersand =
        if (eradicate_java ()) {
          ""
        } else {
          "&"
        };
      ampersand ^ s
    }
  | Dunop op de => Unop.str op ^ to_string de
  | Dsizeof typ _ _ => pp_to_string (Typ.pp_full pe_text) typ
  | Dunknown => "unknown"
  | Dretcall de _ _ _ => "returned by " ^ to_string de;


/** Pretty print a dexp. */
let pp fmt de => F.fprintf fmt "%s" (to_string de);


/** Pretty print a value path */
let pp_vpath pe fmt vpath => {
  let pp fmt =>
    fun
    | Some de => pp fmt de
    | None => ();
  if (pe.pe_kind === PP_HTML) {
    F.fprintf
      fmt
      " %a{vpath: %a}%a"
      Io_infer.Html.pp_start_color
      Orange
      pp
      vpath
      Io_infer.Html.pp_end_color
      ()
  } else {
    F.fprintf fmt "%a" pp vpath
  }
};

let rec has_tmp_var =
  fun
  | Dpvar pvar
  | Dpvaraddr pvar => Pvar.is_frontend_tmp pvar
  | Dderef dexp
  | Ddot dexp _
  | Darrow dexp _
  | Dunop _ dexp
  | Dsizeof _ (Some dexp) _ => has_tmp_var dexp
  | Darray dexp1 dexp2
  | Dbinop _ dexp1 dexp2 => has_tmp_var dexp1 || has_tmp_var dexp2
  | Dretcall dexp dexp_list _ _
  | Dfcall dexp dexp_list _ _ => has_tmp_var dexp || IList.exists has_tmp_var dexp_list
  | Dconst _
  | Dunknown
  | Dsizeof _ None _ => false;
