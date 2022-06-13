(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module IRAttributes = Attributes

type actual = CapturedVar.t ProcAttributes.passed_block option

(* name for the specialized method instantiated with closure arguments and captured vars *)
let pname_with_closure_actuals callee_pname formals_to_blocks =
  let rec get_pnames pnames = function
    | ProcAttributes.Block (pname, _) when Procname.is_objc_block pname || Procname.is_c pname ->
        Procname.to_funptr_parameter pname :: pnames
    | ProcAttributes.Fields passed_blocks ->
        Fieldname.Map.fold
          (fun _ passed_block pnames -> get_pnames pnames passed_block)
          passed_blocks pnames
    | _ ->
        pnames
  in
  let pnames =
    Pvar.Map.fold (fun _ passed_block pnames -> get_pnames pnames passed_block) formals_to_blocks []
  in
  Procname.with_block_parameters callee_pname pnames


let make_formals_to_blocks ~extra_formals_to_blocks ~args:(arg_formals, arg_actuals)
    ~captured_vars:(captured_formals, captured_actuals) pname =
  match
    List.fold2 arg_formals arg_actuals ~init:extra_formals_to_blocks
      ~f:(fun map (mangled, _, _) actual ->
        let pvar = Pvar.mk mangled pname in
        Option.value_map actual ~default:map ~f:(fun actual -> Pvar.Map.add pvar actual map) )
  with
  | Unequal_lengths ->
      None
  | Ok map -> (
    match
      List.fold2 captured_formals captured_actuals ~init:map
        ~f:(fun map {CapturedVar.pvar} actual ->
          Option.value_map actual ~default:map ~f:(fun actual -> Pvar.Map.add pvar actual map) )
    with
    | Unequal_lengths ->
        None
    | Ok map ->
        Some map )


let rec get_captured_in_passed_block captured_vars_acc = function
  | ProcAttributes.Block (_, captured_vars) ->
      captured_vars :: captured_vars_acc
  | ProcAttributes.Fields passed_blocks ->
      Fieldname.Map.fold
        (fun _ actual captured_vars_acc -> get_captured_in_passed_block captured_vars_acc actual)
        passed_blocks captured_vars_acc


let get_captured actuals =
  List.fold_right ~init:[] actuals ~f:(fun actual captured_vars ->
      Option.value_map actual ~default:captured_vars ~f:(fun actual ->
          get_captured_in_passed_block captured_vars actual ) )
  |> List.concat


let get_extra_captured extra_formals_to_blocks =
  Pvar.Map.fold
    (fun _ actual captured_vars -> get_captured_in_passed_block captured_vars actual)
    extra_formals_to_blocks []
  |> List.concat


let should_specialize actuals = List.exists actuals ~f:Option.is_some

let is_objc_setter proc_desc =
  let attributes = Procdesc.get_attributes proc_desc in
  match attributes.ProcAttributes.objc_accessor with Some (Objc_setter _) -> true | _ -> false


let is_initializer proc_desc =
  let proc_name = Procdesc.get_proc_name proc_desc in
  Procname.is_constructor proc_name


let is_dispatch_model proc_desc =
  let proc_name = Procdesc.get_proc_name proc_desc in
  ObjCDispatchModels.is_model proc_name


let create_specialized_procdesc callee_pname ~extra_formals_to_blocks ~captured_actuals ~arg_actuals
    =
  (* captured vars always come first *)
  let actuals = captured_actuals @ arg_actuals in
  if should_specialize actuals then
    match Procdesc.load callee_pname with
    | Some proc_desc
      when (not (is_objc_setter proc_desc))
           && (not (is_initializer proc_desc))
           && not (is_dispatch_model proc_desc) -> (
        (* The specialization is always a full specialization:
             given a function and its known block arguments and captured vars:
             1. if the function is unspecialized then specialize it with the given blocks
             2. if the function is already specialized then get its unspecialized version
                (available as [orig_proc] in the attributes) and apply case 1 with it
           Rather than adding the mechanism to do partial specialization:
             2'. if the function is already specialized then filter-out the arguments and
                 captured vars that have already been used for its specialization and
                 specialize it with the remaining arguments and captured vars

           It is easier (and less error-prone) to always do a full specialization (1 and 2)

           Note:
             Always applying the full specialization is only valid as long as the new
             specialization work is given at least as much information as the previous one.
             Anything triggering a re-specialization has access to the previous
             specialization information and, therefore, should be able to ensure the above
             property holds if there were any doubt
        *)
        let orig_pdesc =
          let callee_attributes = Procdesc.get_attributes proc_desc in
          let orig_pdesc =
            let open IOption.Let_syntax in
            let* {ProcAttributes.orig_proc} = callee_attributes.specialized_with_blocks_info in
            Procdesc.load orig_proc
          in
          Option.value ~default:proc_desc orig_pdesc
        in
        let orig_attributes = Procdesc.get_attributes orig_pdesc in
        let callee_pname = orig_attributes.proc_name in
        match
          make_formals_to_blocks ~extra_formals_to_blocks
            ~args:(orig_attributes.formals, arg_actuals)
            ~captured_vars:(orig_attributes.captured, captured_actuals)
            callee_pname
        with
        | Some formals_to_blocks -> (
            let specialized_pname = pname_with_closure_actuals callee_pname formals_to_blocks in
            (* To avoid duplicated additions on a specialized procname, it does a
               membership check. This may happen when there are multiple function calls
               with the same callees and the same closure parameters.  For the following
               additions, we can simply ignore them, because the function bodies of the
               same procname must be the same.

               Here, it creates a copy of orig_pdesc. The function body will be
               specialized later by [CCallSpecializedWithClosures.process]. *)
            match Procdesc.load specialized_pname with
            | Some _ ->
                Some specialized_pname (* already exists*)
            | None ->
                if (Procdesc.get_attributes proc_desc).is_defined then (
                  let new_attributes =
                    { orig_attributes with
                      specialized_with_blocks_info= Some {orig_proc= callee_pname; formals_to_blocks}
                    ; captured=
                        (* order matters *)
                        List.concat
                          [ get_extra_captured extra_formals_to_blocks
                          ; get_captured actuals
                          ; orig_attributes.captured ]
                    ; proc_name= specialized_pname }
                  in
                  let specialized_pdesc = Procdesc.from_proc_attributes new_attributes in
                  Procdesc.deep_copy_code_from_pdesc ~orig_pdesc ~dest_pdesc:specialized_pdesc ;
                  IRAttributes.store ~proc_desc:(Some specialized_pdesc) new_attributes ;
                  Some specialized_pname )
                else (* No procdesc to specialize: not defined *)
                  None )
        | None ->
            (* Current call is ill defined: args' length doesn't match function parameters' *)
            None )
    | Some _ ->
        None (* No need for specialization*)
    | None ->
        None (* No procdesc to specialize: non-existant *)
  else (* No need for specialization*)
    None
