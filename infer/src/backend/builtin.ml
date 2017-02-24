(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Module for builtin functions with their symbolic execution handler *)

type args = {
  pdesc : Procdesc.t;
  instr : Sil.instr;
  tenv : Tenv.t;
  prop_ : Prop.normal Prop.t;
  path : Paths.Path.t;
  ret_id : (Ident.t * Typ.t) option;
  args : (Exp.t * Typ.t) list;
  proc_name : Procname.t;
  loc : Location.t;
}

type ret_typ = (Prop.normal Prop.t * Paths.Path.t) list

type t = args -> ret_typ

type registered = t

(** builtin function names for which we do symbolic execution *)
let builtin_functions = Procname.Hash.create 4

let check_register_populated () =
  (* check if BuiltinDefn were loaded before accessing register *)
  if Int.equal (Procname.Hash.length builtin_functions) 0 then
    failwith "Builtins were not initialized"

(** check if the function is a builtin *)
let is_registered name =
  Procname.Hash.mem builtin_functions name || (check_register_populated (); false)

(** get the symbolic execution handler associated to the builtin function name *)
let get name : t option =
  try Some (Procname.Hash.find builtin_functions name)
  with Not_found -> (check_register_populated (); None)

(** register a builtin [Procname.t] and symbolic execution handler *)
let register proc_name sym_exe_fun : registered =
  Procname.Hash.replace builtin_functions proc_name sym_exe_fun;
  sym_exe_fun

(** print the functions registered *)
let pp_registered fmt () =
  let builtin_names = ref [] in
  Procname.Hash.iter (fun name _ -> builtin_names := name :: !builtin_names) builtin_functions;
  builtin_names := List.sort ~cmp:Procname.compare !builtin_names;
  let pp pname = Format.fprintf fmt "%a@\n" Procname.pp pname in
  Format.fprintf fmt "Registered builtins:@\n  @[";
  List.iter ~f:pp !builtin_names;
  Format.fprintf fmt "@]@."

(** print the builtin functions and exit *)
let print_and_exit () =
  pp_registered Format.std_formatter ();
  exit 0
