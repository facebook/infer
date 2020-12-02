(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let any_type : Typ.t =
  let classname = Typ.mk (Tstruct StdTyp.Name.Java.java_lang_object) in
  Typ.mk (Tptr (classname, Pk_pointer))


let topl_class_name : Typ.Name.t = Typ.Name.Java.from_string ToplName.topl_property

let topl_class_typ = Typ.mk (Tstruct topl_class_name)

let topl_call ret_id (ret_typ : Typ.desc) loc method_name arg_ts : Sil.instr =
  let e_fun =
    let return_type = Some StdTyp.void in
    let parameters = List.map arg_ts ~f:(fun _ -> StdTyp.Java.pointer_to_java_lang_object) in
    Exp.Const
      (Const.Cfun
         (Procname.make_java ~class_name:topl_class_name ~return_type ~method_name ~parameters
            ~kind:Procname.Java.Static ()))
  in
  Sil.Call ((ret_id, Typ.mk ret_typ), e_fun, arg_ts, loc, CallFlags.default)


let topl_class_pvar =
  let class_name = Mangled.from_string ToplName.topl_property in
  Pvar.mk_global class_name


let topl_class_exp = Exp.Lvar topl_class_pvar

let make_field field_name =
  Fieldname.make (Typ.Name.Java.from_string ToplName.topl_property) field_name


let static_var x : Exp.t = Exp.Lfield (topl_class_exp, make_field x, topl_class_typ)

let local_var proc_name x : Exp.t = Exp.Lvar (Pvar.mk (Mangled.from_string x) proc_name)

let constant_int (x : int) : Exp.t = Exp.int (IntLit.of_int x)

let is_synthesized = function
  | Procname.Java j ->
      String.equal ToplName.topl_property (Procname.Java.get_class_name j)
  | _ ->
      false


let binop_to =
  let open ToplAst in
  let open Binop in
  function OpEq -> Eq | OpNe -> Ne | OpGe -> Ge | OpGt -> Gt | OpLe -> Le | OpLt -> Lt


let debug fmt =
  let mode = if Config.trace_topl then Logging.Quiet else Logging.Verbose in
  Logging.debug Analysis mode "ToplTrace: " ;
  Logging.debug Analysis mode fmt
