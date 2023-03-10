(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let is_view_controller tenv cls =
  let uiviewcontroller = Typ.Name.Objc.from_string "UIViewController" in
  let cls_name = Typ.Name.Objc.from_string cls in
  PatternMatch.is_subtype tenv cls_name uiviewcontroller
  && not (Typ.Name.equal cls_name uiviewcontroller)


let is_init_view_controller procname =
  Procname.is_objc_init procname
  || String.is_prefix (Procname.get_method procname) ~prefix:"_initWithFuncTable:"
  || String.is_suffix (Procname.get_method procname) ~suffix:"ViewControllerCreate"


let lifecycle_methods = [("viewDidLoad", `NoArg)]

let build_objc_instance_method cls method_name =
  Procname.ObjC_Cpp (Procname.ObjC_Cpp.make cls method_name ObjCInstanceMethod Typ.NoTemplate [])


let build_view_controller_methods cls loc controller_arg =
  let call_flags = {CallFlags.default with cf_virtual= true} in
  let to_args param =
    match param with
    | `NoArg ->
        []
    | `AnimatedArg ->
        [(Exp.one, {Typ.desc= Typ.Tint IBool; Typ.quals= Typ.mk_type_quals ()})]
  in
  let build_view_controller_method (method_name, params) instrs =
    let procname = build_objc_instance_method cls method_name in
    let ret_id = CTrans_utils.mk_fresh_void_id_typ () in
    let args = controller_arg :: to_args params in
    let instr = Sil.Call (ret_id, Const (Cfun procname), args, loc, call_flags) in
    instr :: instrs
  in
  List.fold_right ~f:build_view_controller_method ~init:[] lifecycle_methods


let replace_calls tenv _ proc_desc =
  let procname = Procdesc.get_proc_name proc_desc in
  if is_init_view_controller procname then ()
  else
    let add_calls node _ instr =
      let instrs = ProcCfg.Exceptional.instrs node in
      Ident.update_name_generator (Instrs.instrs_get_normal_vars instrs) ;
      let instrs =
        match (instr : Sil.instr) with
        | Call ((ret_id, ret_typ), Const (Cfun callee), _, loc, _)
          when is_init_view_controller callee -> (
          match ret_typ.Typ.desc with
          | Typ.Tptr ({desc= Tstruct cls}, _) ->
              let cls_name = Typ.Name.name cls in
              if is_view_controller tenv cls_name then
                instr :: build_view_controller_methods cls loc (Var ret_id, ret_typ)
              else [instr]
          | _ ->
              [instr] )
        | _ ->
            [instr]
      in
      Array.of_list instrs
    in
    let update_context () _ = () in
    let context_at_node _ = () in
    ignore
      (Procdesc.replace_instrs_by_using_context proc_desc ~f:add_calls ~update_context
         ~context_at_node )


let process cfg tenv = Procname.Hash.iter (replace_calls tenv) cfg
