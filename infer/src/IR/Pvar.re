/*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! IStd;


/** The Smallfoot Intermediate Language */
module L = Logging;

module F = Format;

type translation_unit =
  | TUFile SourceFile.t
  | TUExtern
[@@deriving compare];


/** Kind of global variables */
type pvar_kind =
  | Local_var Typ.Procname.t /** local variable belonging to a function */
  | Callee_var Typ.Procname.t /** local variable belonging to a callee */
  | Abduced_retvar Typ.Procname.t Location.t /** synthetic variable to represent return value */
  | Abduced_ref_param Typ.Procname.t t Location.t
  | Abduced_ref_param_val Typ.Procname.t Ident.t Location.t
  /** synthetic variable to represent param passed by reference */
  | Global_var (translation_unit, bool, bool, bool)
  /** global variable: translation unit + is it compile constant? + is it POD? + is it a static
      local? */
  | Seed_var /** variable used to store the initial value of formal parameters */
[@@deriving compare]
/** Names for program variables. */
and t = {pv_hash: int, pv_name: Mangled.t, pv_kind: pvar_kind} [@@deriving compare];

let equal = [%compare.equal : t];

let pp_translation_unit fmt =>
  fun
  | TUFile fname => SourceFile.pp fmt fname
  | TUExtern => Format.fprintf fmt "EXTERN";

let rec _pp f pv => {
  let name = pv.pv_name;
  switch pv.pv_kind {
  | Local_var n =>
    if !Config.pp_simple {
      F.fprintf f "%a" Mangled.pp name
    } else {
      F.fprintf f "%a$%a" Typ.Procname.pp n Mangled.pp name
    }
  | Callee_var n =>
    if !Config.pp_simple {
      F.fprintf f "%a|callee" Mangled.pp name
    } else {
      F.fprintf f "%a$%a|callee" Typ.Procname.pp n Mangled.pp name
    }
  | Abduced_retvar n l =>
    if !Config.pp_simple {
      F.fprintf f "%a|abducedRetvar" Mangled.pp name
    } else {
      F.fprintf f "%a$%a%a|abducedRetvar" Typ.Procname.pp n Location.pp l Mangled.pp name
    }
  | Abduced_ref_param n pv l =>
    if !Config.pp_simple {
      F.fprintf f "%a|%a|abducedRefParam" _pp pv Mangled.pp name
    } else {
      F.fprintf f "%a$%a%a|abducedRefParam" Typ.Procname.pp n Location.pp l Mangled.pp name
    }
  | Abduced_ref_param_val n id l =>
    if !Config.pp_simple {
      F.fprintf f "%a|%a|abducedRefParamVal" (Ident.pp Pp.text) id Mangled.pp name
    } else {
      F.fprintf f "%a$%a%a|abducedRefParamVal" Typ.Procname.pp n Location.pp l Mangled.pp name
    }
  | Global_var (translation_unit, is_const, is_pod, _) =>
    F.fprintf
      f
      "#GB<%a%s%s>$%a"
      pp_translation_unit
      translation_unit
      (if is_const {"|const"} else {""})
      (
        if (not is_pod) {
          "|!pod"
        } else {
          ""
        }
      )
      Mangled.pp
      name
  | Seed_var => F.fprintf f "old_%a" Mangled.pp name
  }
};


/** Pretty print a program variable in latex. */
let pp_latex f pv => {
  let name = pv.pv_name;
  switch pv.pv_kind {
  | Local_var _ => Latex.pp_string Latex.Roman f (Mangled.to_string name)
  | Callee_var _ =>
    F.fprintf
      f
      "%a_{%a}"
      (Latex.pp_string Latex.Roman)
      (Mangled.to_string name)
      (Latex.pp_string Latex.Roman)
      "callee"
  | Abduced_retvar _ =>
    F.fprintf
      f
      "%a_{%a}"
      (Latex.pp_string Latex.Roman)
      (Mangled.to_string name)
      (Latex.pp_string Latex.Roman)
      "abducedRetvar"
  | Abduced_ref_param _ =>
    F.fprintf
      f
      "%a_{%a}"
      (Latex.pp_string Latex.Roman)
      (Mangled.to_string name)
      (Latex.pp_string Latex.Roman)
      "abducedRefParam"
  | Abduced_ref_param_val _ =>
    F.fprintf
      f
      "%a_{%a}"
      (Latex.pp_string Latex.Roman)
      (Mangled.to_string name)
      (Latex.pp_string Latex.Roman)
      "abducedRefParamVal"
  | Global_var _ => Latex.pp_string Latex.Boldface f (Mangled.to_string name)
  | Seed_var =>
    F.fprintf
      f
      "%a^{%a}"
      (Latex.pp_string Latex.Roman)
      (Mangled.to_string name)
      (Latex.pp_string Latex.Roman)
      "old"
  }
};


/** Pretty print a pvar which denotes a value, not an address */
let pp_value pe f pv =>
  switch pe.Pp.kind {
  | TEXT => _pp f pv
  | HTML => _pp f pv
  | LATEX => pp_latex f pv
  };


/** Pretty print a program variable. */
let pp pe f pv => {
  let ampersand =
    switch pe.Pp.kind {
    | TEXT => "&"
    | HTML => "&amp;"
    | LATEX => "\\&"
    };
  F.fprintf f "%s%a" ampersand (pp_value pe) pv
};


/** Dump a program variable. */
let d (pvar: t) => L.add_print_action (L.PTpvar, Obj.repr pvar);


/** Pretty print a list of program variables. */
let pp_list pe f pvl => F.fprintf f "%a" (Pp.seq (fun f e => F.fprintf f "%a" (pp pe) e)) pvl;


/** Dump a list of program variables. */
let d_list pvl =>
  List.iter
    f::(
      fun pv => {
        d pv;
        L.d_str " "
      }
    )
    pvl;

let get_name pv => pv.pv_name;

let to_string pv => Mangled.to_string pv.pv_name;

let get_simplified_name pv => {
  let s = Mangled.to_string pv.pv_name;
  switch (String.rsplit2 s on::'.') {
  | Some (s1, s2) =>
    switch (String.rsplit2 s1 on::'.') {
    | Some (_, s4) => s4 ^ "." ^ s2
    | _ => s
    }
  | _ => s
  }
};


/** Check if the pvar is an abucted return var or param passed by ref */
let is_abduced pv =>
  switch pv.pv_kind {
  | Abduced_retvar _
  | Abduced_ref_param _
  | Abduced_ref_param_val _ => true
  | _ => false
  };


/** Turn a pvar into a seed pvar (which stored the initial value) */
let to_seed pv => {...pv, pv_kind: Seed_var};


/** Check if the pvar is a local var */
let is_local pv =>
  switch pv.pv_kind {
  | Local_var _ => true
  | _ => false
  };


/** Check if the pvar is a callee var */
let is_callee pv =>
  switch pv.pv_kind {
  | Callee_var _ => true
  | _ => false
  };


/** Check if the pvar is a seed var */
let is_seed pv =>
  switch pv.pv_kind {
  | Seed_var => true
  | _ => false
  };


/** Check if the pvar is a global var */
let is_global pv =>
  switch pv.pv_kind {
  | Global_var _ => true
  | _ => false
  };

let is_static_local pv =>
  switch pv.pv_kind {
  | Global_var (_, _, _, true) => true
  | _ => false
  };


/** Check if a pvar is the special "this" var */
let is_this pvar => Mangled.equal (get_name pvar) (Mangled.from_string "this");


/** Check if a pvar is the special "self" var */
let is_self pvar => Mangled.equal (get_name pvar) (Mangled.from_string "self");


/** Check if the pvar is a return var */
let is_return pv => Mangled.equal (get_name pv) Ident.name_return;


/** something that can't be part of a legal identifier in any conceivable language */
let tmp_prefix = "0$?%__sil_tmp";


/** return true if [pvar] is a temporary variable generated by the frontend */
let is_frontend_tmp pvar => {
  /* Check whether the program variable is a temporary one generated by Sawja, javac, or some other
     bytecode/name generation pass. valid java identifiers cannot contain `$` */
  let is_bytecode_tmp name =>
    String.contains name '$' && not (String.contains name '_') ||
    String.is_prefix prefix::"CatchVar" name;
  /* Check whether the program variable is generated by [mk_tmp] */
  let is_sil_tmp name => String.is_prefix prefix::tmp_prefix name;
  let name = to_string pvar;
  is_sil_tmp name || (
    switch pvar.pv_kind {
    | Local_var pname => Typ.Procname.is_java pname && is_bytecode_tmp name
    | _ => false
    }
  )
};

/* in Sawja, variables like $T0_18 are temporaries, but not SSA vars. */
let is_ssa_frontend_tmp pvar =>
  is_frontend_tmp pvar && {
    let name = to_string pvar;
    not (String.contains name '_' && String.contains name '$')
  };


/** Turn an ordinary program variable into a callee program variable */
let to_callee pname pvar =>
  switch pvar.pv_kind {
  | Local_var _ => {...pvar, pv_kind: Callee_var pname}
  | Global_var _ => pvar
  | Callee_var _
  | Abduced_retvar _
  | Abduced_ref_param _
  | Abduced_ref_param_val _
  | Seed_var =>
    L.d_str "Cannot convert pvar to callee: ";
    d pvar;
    L.d_ln ();
    assert false
  };

let name_hash (name: Mangled.t) => Hashtbl.hash name;


/** [mk name proc_name] creates a program var with the given function name */
let mk (name: Mangled.t) (proc_name: Typ.Procname.t) :t => {
  pv_hash: name_hash name,
  pv_name: name,
  pv_kind: Local_var proc_name
};

let get_ret_pvar pname => mk Ident.name_return pname;


/** [mk_callee name proc_name] creates a program var
    for a callee function with the given function name */
let mk_callee (name: Mangled.t) (proc_name: Typ.Procname.t) :t => {
  pv_hash: name_hash name,
  pv_name: name,
  pv_kind: Callee_var proc_name
};


/** create a global variable with the given name */
let mk_global
    ::is_constexpr=false
    ::is_pod=true
    ::is_static_local=false
    (name: Mangled.t)
    translation_unit
    :t => {
  pv_hash: name_hash name,
  pv_name: name,
  pv_kind: Global_var (translation_unit, is_constexpr, is_pod, is_static_local)
};


/** create a fresh temporary variable local to procedure [pname]. for use in the frontends only! */
let mk_tmp name pname => {
  let id = Ident.create_fresh Ident.knormal;
  let pvar_mangled = Mangled.from_string (tmp_prefix ^ name ^ Ident.to_string id);
  mk pvar_mangled pname
};


/** create an abduced return variable for a call to [proc_name] at [loc] */
let mk_abduced_ret (proc_name: Typ.Procname.t) (loc: Location.t) :t => {
  let name = Mangled.from_string ("$RET_" ^ Typ.Procname.to_unique_id proc_name);
  {pv_hash: name_hash name, pv_name: name, pv_kind: Abduced_retvar proc_name loc}
};

let mk_abduced_ref_param (proc_name: Typ.Procname.t) (pv: t) (loc: Location.t) :t => {
  let name = Mangled.from_string ("$REF_PARAM_" ^ Typ.Procname.to_unique_id proc_name);
  {pv_hash: name_hash name, pv_name: name, pv_kind: Abduced_ref_param proc_name pv loc}
};

let mk_abduced_ref_param_val (proc_name: Typ.Procname.t) (id: Ident.t) (loc: Location.t) :t => {
  let name = Mangled.from_string ("$REF_PARAM_VAL_" ^ Typ.Procname.to_unique_id proc_name);
  {pv_hash: name_hash name, pv_name: name, pv_kind: Abduced_ref_param_val proc_name id loc}
};

let get_translation_unit pvar =>
  switch pvar.pv_kind {
  | Global_var (tu, _, _, _) => tu
  | _ => invalid_argf "Expected a global variable"
  };

let is_compile_constant pvar =>
  switch pvar.pv_kind {
  | Global_var (_, b, _, _) => b
  | _ => false
  };

let is_pod pvar =>
  switch pvar.pv_kind {
  | Global_var (_, _, b, _) => b
  | _ => true
  };

let get_initializer_pname {pv_name, pv_kind} =>
  switch pv_kind {
  | Global_var _ =>
    Some (
      Typ.Procname.from_string_c_fun (
        Config.clang_initializer_prefix ^ Mangled.to_string_full pv_name
      )
    )
  | _ => None
  };
