(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

module Payload = SummaryPayload.Make (struct
  type t = ClassLoadsDomain.summary

  let update_payloads post (payloads : Payloads.t) = {payloads with class_loads= Some post}

  let of_payloads (payloads : Payloads.t) = payloads.class_loads
end)

let do_call pdesc callee loc init =
  Payload.read pdesc callee |> Option.fold ~init ~f:(ClassLoadsDomain.integrate_summary callee loc)


(** fully load a class given the typename *)
let rec load_class proc_desc tenv loc astate class_name =
  (* don't bother if class is already loaded *)
  if ClassLoadsDomain.mem_typename class_name astate then astate
  else
    (* load the class itself *)
    let astate1 = ClassLoadsDomain.add_typename loc astate class_name in
    (* load classes referenced by the class initializer *)
    let astate2 =
      let class_initializer = Typ.Procname.(Java (Java.get_class_initializer class_name)) in
      (* NB may recurse if we are in class init but the shortcircuiting above makes it a no-op *)
      do_call proc_desc class_initializer loc astate1
    in
    (* finally, recursively load all superclasses *)
    Tenv.lookup tenv class_name
    |> Option.value_map ~default:[] ~f:(fun tstruct -> tstruct.Typ.Struct.supers)
    |> List.fold ~init:astate2 ~f:(load_class proc_desc tenv loc)


let rec exp_fold_over_fields ~f ~init (exp : Exp.t) =
  match exp with
  (* TODO Cast? Const literals for class objects? Arrays? *)
  | Var _ | Const _ | Lvar _ | Sizeof _ | Closure _ ->
      init
  | Cast (_, e) | UnOp (_, e, _) | Exn e | Lindex (e, _) ->
      exp_fold_over_fields ~f ~init e
  | BinOp (_, e1, e2) ->
      let init = exp_fold_over_fields ~f ~init e1 in
      exp_fold_over_fields ~f ~init e2
  | Lfield (e, field, typ) ->
      let init = f init field typ in
      exp_fold_over_fields ~f ~init e


let class_of_type (typ : Typ.t) =
  match typ with
  | {desc= Tstruct name} | {desc= Tptr ({desc= Tstruct name}, _)} ->
      Some name
  | _ ->
      None


let add_field_loads_of_exp proc_desc tenv exp loc init =
  let f init _field typ =
    class_of_type typ |> Option.fold ~init ~f:(load_class proc_desc tenv loc)
  in
  exp_fold_over_fields ~f ~init exp


let exec_instr pdesc tenv astate _ (instr : Sil.instr) =
  match instr with
  | Call (_, Const (Cfun callee), _, loc, _) ->
      do_call pdesc callee loc astate
  | Load (_, exp, _, loc) | Prune (exp, loc, _, _) ->
      add_field_loads_of_exp pdesc tenv exp loc astate
  | Store (lexp, _, rexp, loc) ->
      add_field_loads_of_exp pdesc tenv lexp loc astate
      |> add_field_loads_of_exp pdesc tenv rexp loc
  | _ ->
      astate


let report_loads proc_desc summary astate =
  let report_load ({ClassLoadsDomain.Event.loc; elem} as event) =
    if String.is_prefix ~prefix:"java." elem then ()
    else
      let ltr = ClassLoadsDomain.Event.make_loc_trace event in
      let msg = Format.asprintf "Class %s loaded" elem in
      Reporting.log_warning summary ~loc ~ltr IssueType.class_load msg
  in
  let pname = Procdesc.get_proc_name proc_desc in
  Typ.Procname.get_class_name pname
  |> Option.iter ~f:(fun clazz ->
         let method_strname = Typ.Procname.get_method pname in
         let fullname = clazz ^ "." ^ method_strname in
         if String.Set.mem Config.class_loads_roots fullname then
           ClassLoadsDomain.iter report_load astate )


let analyze_procedure {Callbacks.proc_desc; tenv; summary} =
  let proc_name = Procdesc.get_proc_name proc_desc in
  L.debug Analysis Verbose "CL: ANALYZING %a@." Typ.Procname.pp proc_name ;
  let loc = Procdesc.get_loc proc_desc in
  (* load the method's class *)
  let init =
    Typ.Procname.get_class_type_name proc_name
    |> Option.fold ~init:ClassLoadsDomain.empty ~f:(load_class proc_desc tenv loc)
  in
  let post = Procdesc.fold_instrs proc_desc ~init ~f:(exec_instr proc_desc tenv) in
  report_loads proc_desc summary post ;
  let result = Payload.update_summary post summary in
  L.debug Analysis Verbose "CL: FINISHED ANALYZING %a@." Typ.Procname.pp proc_name ;
  result
