(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(* Given two lists of tuples (exp1, var1, typ1) and (exp2, var2, typ2) append the lists avoiding
   duplicates, where if the variables exist we check their equality, otherwise we check the equality
   of the expressions. This is to avoid adding the same captured variable twice. *)
let append_no_duplicates_vars =
  let cmp (exp1, var1_opt, _) (exp2, var2_opt, _) =
    match (var1_opt, var2_opt) with
    | Some var1, Some var2 ->
        Pvar.compare var1 var2
    | None, None ->
        Exp.compare exp1 exp2
    | Some _, None ->
        1
    | None, Some _ ->
        -1
  in
  Staged.unstage (IList.append_no_duplicates ~cmp)


(* Given a list of actual parameters for a function, replaces the closures with the
captured variables, avoiding adding the same captured variable twice. *)
let get_extended_args_for_method_with_block_analysis act_params =
  let ext_actuals = List.map ~f:(fun (exp, typ) -> (exp, None, typ)) act_params in
  let args_and_captured =
    List.fold ext_actuals ~init:[] ~f:(fun all_args act_param ->
        match act_param with
        | Exp.Closure cl, _, _ ->
            let captured =
              List.map ~f:(fun (exp, var, typ) -> (exp, Some var, typ)) cl.captured_vars
            in
            append_no_duplicates_vars all_args captured
        | _ ->
            List.append all_args [act_param] )
  in
  List.map ~f:(fun (exp, _, typ) -> (exp, typ)) args_and_captured


let resolve_method_with_block_args_and_analyze ~caller_pdesc pname act_params =
  let pdesc_opt =
    match Ondemand.analyze_proc_name ~caller_pdesc pname with
    | Some summary ->
        Some (Summary.get_proc_desc summary)
    | None ->
        Ondemand.get_proc_desc pname
  in
  match pdesc_opt with
  | Some pdesc
    when Procdesc.is_defined pdesc
         && Int.equal (List.length (Procdesc.get_formals pdesc)) (List.length act_params)
         (* only specialize defined methods, and when formals and actuals have the same length  *)
        -> (
      (* a list with the same length of the actual params of the function,
        containing either a Closure or None. *)
      let block_args =
        List.map act_params ~f:(function
          | Exp.Closure cl, _ when Typ.Procname.is_objc_block cl.name ->
              Some cl
          | _ ->
              None )
      in
      (* name for the specialized method instantiated with block arguments *)
      let pname_with_block_args =
        let block_name_args =
          List.filter_map block_args ~f:(function
            | Some (cl : Exp.closure) ->
                Some (Typ.Procname.block_name_of_procname cl.name)
            | None ->
                None )
        in
        Typ.Procname.with_block_parameters pname block_name_args
      in
      (* new procdesc cloned from the original one, where the block parameters have been
       replaced by the block arguments. The formals have also been expanded with the captured variables  *)
      let specialized_pdesc =
        SpecializeProcdesc.with_block_args pdesc pname_with_block_args block_args
      in
      Logging.(debug Analysis Verbose) "Instructions of specialized method:@." ;
      Procdesc.iter_instrs
        (fun _ instr ->
          Logging.(debug Analysis Verbose) "%a@." (Sil.pp_instr ~print_types:false Pp.text) instr
          )
        specialized_pdesc ;
      Logging.(debug Analysis Verbose) "End of instructions@." ;
      match Ondemand.analyze_proc_desc ~caller_pdesc specialized_pdesc with
      | Some summary ->
          (* Since the closures in the formals were replaced by the captured variables,
           we do the same with the actual arguments *)
          let extended_args = get_extended_args_for_method_with_block_analysis act_params in
          Some (summary, extended_args)
      | None ->
          None )
  | _ ->
      None
