(*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module L = Logging
module F = Format

let checkers_repeated_calls_name = "CHECKERS_REPEATED_CALLS"


(** Extension for the repeated calls check. *)
module RepeatedCallsExtension : Eradicate.ExtensionT =
struct
  module InstrSet =
    Set.Make(struct
      type t = Sil.instr
      let compare i1 i2 = match i1, i2 with
        | Sil.Call (_, e1, etl1, _, cf1), Sil.Call (_, e2, etl2, _, cf2) ->
            (* ignore return ids and call flags *)
            let n = Exp.compare e1 e2 in
            if n <> 0 then n else let n = IList.compare Sil.exp_typ_compare etl1 etl2 in
              if n <> 0 then n else CallFlags.compare cf1 cf2
        | _ -> Sil.instr_compare i1 i2
    end)

  type extension = InstrSet.t

  let empty = InstrSet.empty

  let join calls1 calls2 =
    InstrSet.inter calls1 calls2

  let pp fmt calls =
    let pp_call instr = F.fprintf fmt "  %a@\n" (Sil.pp_instr pe_text) instr in
    if not (InstrSet.is_empty calls) then
      begin
        F.fprintf fmt "Calls:@\n";
        InstrSet.iter pp_call calls;
      end

  let get_old_call instr calls =
    try
      Some (InstrSet.find instr calls)
    with Not_found -> None

  let add_call instr calls =
    if InstrSet.mem instr calls then calls
    else InstrSet.add instr calls

  type paths =
    | AllPaths (** Check on all paths *)
    | SomePath (** Check if some path exists *)

  (** Check if the procedure performs an allocation operation.
      If [paths] is AllPaths, check if an allocation happens on all paths.
      If [paths] is SomePath, check if a path with an allocation exists. *)
  let proc_performs_allocation tenv pdesc paths : Location.t option =

    let node_allocates node : Location.t option =
      let found = ref None in
      let proc_is_new pn =
        Procname.equal pn ModelBuiltins.__new ||
        Procname.equal pn ModelBuiltins.__new_array in
      let do_instr instr =
        match instr with
        | Sil.Call (_, Exp.Const (Const.Cfun pn), _, loc, _) when proc_is_new pn ->
            found := Some loc
        | _ -> () in
      IList.iter do_instr (Cfg.Node.get_instrs node);
      !found in

    let module DFAllocCheck = Dataflow.MakeDF(struct
        type t = Location.t option
        let equal = opt_equal Location.equal
        let _join _paths l1o l2o = (* join with left priority *)
          match l1o, l2o with
          | None, None ->
              None
          | Some loc, None
          | None, Some loc ->
              if _paths = AllPaths then None else Some loc
          | Some loc1, Some _ ->
              Some loc1 (* left priority *)
        let join = _join paths
        let do_node _ node lo1 =
          let lo2 = node_allocates node in
          let lo' = (* use left priority join to implement transfer function *)
            _join SomePath lo1 lo2 in
          [lo'], [lo']
        let proc_throws _ = Dataflow.DontKnow
      end) in

    let transitions = DFAllocCheck.run tenv pdesc None in
    match transitions (Cfg.Procdesc.get_exit_node pdesc) with
    | DFAllocCheck.Transition (loc, _, _) -> loc
    | DFAllocCheck.Dead_state -> None

  (** Check repeated calls to the same procedure. *)
  let check_instr tenv get_proc_desc curr_pname curr_pdesc extension instr normalized_etl =

    (* Arguments are not temporary variables. *)
    let arguments_not_temp args =
      let filter_arg (e, _) = match e with
        | Exp.Lvar pvar ->
            (* same temporary variable does not imply same value *)
            not (Pvar.is_frontend_tmp pvar)
        | _ -> true in
      IList.for_all filter_arg args in

    match instr with
    | Sil.Call (Some _ as ret_id, Exp.Const (Const.Cfun callee_pname), _, loc, call_flags)
      when arguments_not_temp normalized_etl ->
        let instr_normalized_args = Sil.Call (
            ret_id,
            Exp.Const (Const.Cfun callee_pname),
            normalized_etl,
            loc,
            call_flags) in
        let report proc_desc =
          match get_old_call instr_normalized_args extension with
          | Some (Sil.Call (_, _, _, loc_old, _)) ->
              begin
                match proc_performs_allocation tenv proc_desc AllPaths with
                | Some alloc_loc ->
                    let description =
                      Printf.sprintf "call to %s seen before on line %d (may allocate at %s:%n)"
                        (Procname.to_simplified_string callee_pname)
                        loc_old.Location.line
                        (DB.source_file_to_string alloc_loc.Location.file)
                        alloc_loc.Location.line in
                    Checkers.ST.report_error tenv
                      curr_pname curr_pdesc checkers_repeated_calls_name loc description
                | None -> ()
              end
          | _ -> () in

        let () = match get_proc_desc callee_pname with
          | None -> ()
          | Some proc_desc ->
              if Cfg.Procdesc.is_defined proc_desc
              then report proc_desc in
        add_call instr_normalized_args extension
    | _ -> extension

  let ext =
    {
      TypeState.empty = empty;
      check_instr = check_instr;
      join = join;
      pp = pp;
    }

  let update_payload _ payload = payload
end (* CheckRepeatedCalls *)

module MainRepeatedCalls =
  Eradicate.Build(RepeatedCallsExtension)

let callback_check_repeated_calls callback_args =
  let checks =
    {
      TypeCheck.eradicate = false;
      check_extension = Config.checkers_repeated_calls;
      check_ret_type = [];
    } in
  MainRepeatedCalls.callback checks callback_args
