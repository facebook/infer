(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type formal_annot = {formal_type: Mangled.t * Typ.t; formal_annot: Annot.Item.t}
[@@deriving compare]

type formal_actual = {formal: formal_annot; actual: Exp.t * Typ.t [@ignore]} [@@deriving compare]

let pp_formal_annot fmt formal =
  F.fprintf fmt "(formal_type=%a, formal_annot=%a)"
    (Pp.pair ~fst:Mangled.pp ~snd:(Typ.pp Pp.text))
    formal.formal_type Annot.Item.pp formal.formal_annot


let pp_formal_actual fmt formal_actual =
  F.fprintf fmt "(formal=%a, actual=%a)" pp_formal_annot formal_actual.formal
    (Pp.pair ~fst:Exp.pp ~snd:(Typ.pp Pp.text))
    formal_actual.actual


module FormalsMap = PrettyPrintable.MakePPMap (struct
  type t = formal_annot [@@deriving compare]

  let pp = pp_formal_annot
end)

module FormalsActualsSet = PrettyPrintable.MakePPSet (struct
  type t = formal_actual [@@deriving compare]

  let pp = pp_formal_actual
end)

let formals_actuals_map formals annotations actual_params =
  let rec formals_actuals_map_inner acc formals annotations actual_params =
    match (formals, annotations, actual_params) with
    | [], [], [] ->
        Some acc
    | fml :: fmls, an :: ans, act :: acts ->
        let acc = FormalsMap.add {formal_type= fml; formal_annot= an} act acc in
        formals_actuals_map_inner acc fmls ans acts
    | _, _, _ ->
        None
  in
  formals_actuals_map_inner FormalsMap.empty formals annotations actual_params


let formals_actuals_new_set map =
  FormalsMap.fold
    (fun formal actual set ->
      match actual with
      | Exp.Closure closure, _ ->
          let set = FormalsActualsSet.add {formal; actual} set in
          List.fold_left closure.Exp.captured_vars ~init:set ~f:(fun set (exp, var, typ, _) ->
              let formal_annot =
                {formal_type= (Pvar.build_formal_from_pvar var, typ); formal_annot= Annot.Item.empty}
              in
              let formal_actual = {formal= formal_annot; actual= (exp, typ)} in
              FormalsActualsSet.add formal_actual set )
      | actual ->
          FormalsActualsSet.add {formal; actual} set )
    map FormalsActualsSet.empty


let formals_annots_actuals_lists new_formals_actuals =
  FormalsActualsSet.fold
    (fun {formal; actual} (fs, ans, acts) ->
      (formal.formal_type :: fs, formal.formal_annot :: ans, actual :: acts) )
    new_formals_actuals ([], [], [])


let has_closure actual_params =
  List.exists actual_params ~f:(fun (exp, _) ->
      match exp with Exp.Closure c -> Procname.is_objc_block c.name | _ -> false )


let should_specialize actual_params call_flags =
  let block_is_receiver actual_params =
    Int.equal (List.length actual_params) 1 && call_flags.CallFlags.cf_virtual
  in
  has_closure actual_params && not (block_is_receiver actual_params)


(* name for the specialized method instantiated with closure arguments *)
let pname_with_closure_args callee_pname actual_params =
  let block_args =
    List.filter_map actual_params ~f:(function
      | Exp.Closure cl, _ when Procname.is_objc_block cl.name ->
          Some (Procname.block_of_procname cl.name)
      | _ ->
          None )
  in
  Procname.with_block_parameters callee_pname block_args


let formals_closures_map map =
  FormalsMap.fold
    (fun formal actual new_map ->
      match actual with
      | Exp.Closure closure, _ ->
          let captured_as_formals =
            List.map
              ~f:(fun (_, var, typ, _) -> (Pvar.build_formal_from_pvar var, typ))
              closure.captured_vars
          in
          Mangled.Map.add (fst formal.formal_type) (closure.name, captured_as_formals) new_map
      | _ ->
          new_map )
    map Mangled.Map.empty


let is_objc_setter proc_desc =
  let attributes = Procdesc.get_attributes proc_desc in
  match attributes.ProcAttributes.objc_accessor with Some (Objc_setter _) -> true | _ -> false


let is_initializer proc_desc =
  let proc_name = Procdesc.get_proc_name proc_desc in
  Procname.is_constructor proc_name


let is_dispatch_model proc_desc =
  let proc_name = Procdesc.get_proc_name proc_desc in
  ObjCDispatchModels.is_model proc_name


let replace_with_specialize_methods cfg _node instr =
  match instr with
  | Sil.Call (ret, Exp.Const (Const.Cfun callee_pname), actual_params, loc, flags)
    when should_specialize actual_params flags -> (
    match Procname.Hash.find_opt cfg callee_pname with
    (*TODO(T74127433): This specialization works well only when the we specialize methods that take a block
      parameter and then run the block. It doesn't work well when the block is instead stored in
      a field. This case will be left as future work, and we won't specialize common cases where this
      happens such as setters or initializers. *)
    | Some proc_desc
      when (not (is_objc_setter proc_desc))
           && (not (is_initializer proc_desc))
           && not (is_dispatch_model proc_desc) -> (
        let callee_attributes = Procdesc.get_attributes proc_desc in
        match
          formals_actuals_map callee_attributes.formals callee_attributes.method_annotation.params
            actual_params
        with
        | Some map ->
            let set = formals_actuals_new_set map in
            let new_formals, new_annots, new_actuals = formals_annots_actuals_lists set in
            let annot = callee_attributes.method_annotation in
            let specialized_pname = pname_with_closure_args callee_pname actual_params in
            let new_attributes =
              { callee_attributes with
                specialized_with_blocks_info=
                  Some
                    { orig_proc= callee_pname
                    ; formals_to_procs_and_new_formals= formals_closures_map map }
              ; is_defined= true
              ; formals= new_formals
              ; method_annotation= {annot with params= new_annots}
              ; proc_name= specialized_pname }
            in
            (* To avoid duplicated additions on a specialized procname, it does a membership check.
               This may happen when there are multiple function calls with the same callees and the
               same closure parameters.  For the following additions, we can simply ignore them,
               because the function bodies of the same procname must be the same.

               Here, it adds an empty procdesc temporarily.  The function body will be filled later
               by [ClosureSubstSpecializedMethod]. *)
            if not (Cfg.mem cfg specialized_pname) then
              Cfg.create_proc_desc cfg new_attributes |> ignore ;
            Sil.Call (ret, Exp.Const (Const.Cfun specialized_pname), new_actuals, loc, flags)
        | None ->
            instr )
    | _ ->
        instr )
  | _ ->
      instr


let process cfg =
  let process_pdesc _proc_name proc_desc =
    ClosuresSubstitution.process_closure_param proc_desc ;
    Procdesc.replace_instrs proc_desc ~f:(replace_with_specialize_methods cfg) |> ignore
  in
  Procname.Hash.iter process_pdesc cfg
