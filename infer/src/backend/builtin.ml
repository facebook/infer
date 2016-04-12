(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Module for builtin functions with their symbolic execution handler *)

type args = {
  pdesc : Cfg.Procdesc.t;
  instr : Sil.instr;
  tenv : Tenv.t;
  prop_ : Prop.normal Prop.t;
  path : Paths.Path.t;
  ret_ids : Ident.t list;
  args : (Sil.exp * Sil.typ) list;
  proc_name : Procname.t;
  loc : Location.t;
}

type ret_typ = (Prop.normal Prop.t * Paths.Path.t) list

type t = args -> ret_typ

(* builtin function names for which we do symbolic execution *)
let builtin_functions = Procname.Hash.create 4

(* Check if the function is a builtin *)
let is_registered name =
  Procname.Hash.mem builtin_functions name

(* get the symbolic execution handler associated to the builtin function name *)
let get name : t =
  try Procname.Hash.find builtin_functions name
  with Not_found -> assert false

(* register a builtin function name and symbolic execution handler *)
let register proc_name_str (sym_exe_fun: t) =
  let proc_name = Procname.from_string_c_fun proc_name_str in
  Procname.Hash.replace builtin_functions proc_name sym_exe_fun;
  proc_name

(* register a builtin [Procname.t] and symbolic execution handler *)
let register_procname proc_name (sym_exe_fun: t) =
  Procname.Hash.replace builtin_functions proc_name sym_exe_fun

(** print the functions registered *)
let pp_registered fmt () =
  let builtin_names = ref [] in
  Procname.Hash.iter (fun name _ -> builtin_names := name :: !builtin_names) builtin_functions;
  builtin_names := IList.sort Procname.compare !builtin_names;
  let pp pname = Format.fprintf fmt "%a@\n" Procname.pp pname in
  Format.fprintf fmt "Registered builtins:@\n  @[";
  IList.iter pp !builtin_names;
  Format.fprintf fmt "@]@."

(** print the builtin functions and exit *)
let print_and_exit () =
  pp_registered Format.std_formatter ();
  exit 0
