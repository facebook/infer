(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let any_type : Typ.t =
  let classname = Typ.mk (Tstruct Typ.Name.Java.java_lang_object) in
  Typ.mk (Tptr (classname, Pk_pointer))


let topl_class_name : Typ.Name.t = Typ.Name.Java.from_string ToplName.topl_property

let topl_class_typ = Typ.mk (Tstruct topl_class_name)

let topl_call ret_id (ret_typ : Typ.desc) loc name arg_ts : Sil.instr =
  let e_fun =
    let ret_typ = Some Typ.Name.Java.Split.void in
    let args_typ = List.map arg_ts ~f:(fun _ -> Typ.Name.Java.Split.java_lang_object) in
    Exp.Const
      (Const.Cfun
         (Typ.Procname.Java
            (Typ.Procname.Java.make topl_class_name ret_typ name args_typ Typ.Procname.Java.Static)))
  in
  Sil.Call ((ret_id, Typ.mk ret_typ), e_fun, arg_ts, loc, CallFlags.default)


let topl_class_exp =
  let class_name = Mangled.from_string ToplName.topl_property in
  let var_name = Pvar.mk_global class_name in
  Exp.Lvar var_name


let static_var x : Exp.t =
  Exp.Lfield (topl_class_exp, Typ.Fieldname.Java.from_string x, topl_class_typ)


let local_var proc_name x : Exp.t = Exp.Lvar (Pvar.mk (Mangled.from_string x) proc_name)

let constant_int (x : int) : Exp.t = Exp.int (IntLit.of_int x)

let is_synthesized = function
  | Typ.Procname.Java j ->
      String.equal ToplName.topl_property (Typ.Procname.Java.get_class_name j)
  | _ ->
      false


let debug fmt =
  Logging.debug Analysis Verbose "ToplTrace: " ;
  Logging.debug Analysis Verbose fmt
