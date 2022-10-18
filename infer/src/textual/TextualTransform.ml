(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
open Textual

(* returns all the idents that are defined in the procdesc *)
let collect_ident_defs ({nodes} : ProcDesc.t) : Ident.Set.t =
  List.fold nodes ~init:Ident.Set.empty ~f:(fun set (node : Node.t) ->
      let set =
        List.fold node.ssa_parameters ~init:set ~f:(fun set (id, _) -> Ident.Set.add id set)
      in
      List.fold node.instrs ~init:set ~f:(fun set (instr : Instr.t) ->
          match instr with Load {id} | Let {id} -> Ident.Set.add id set | Store _ | Prune _ -> set ) )


let module_map_procs ~f (module_ : Module.t) =
  let open Module in
  let decls =
    List.map module_.decls ~f:(fun decl ->
        match decl with Proc pdesc -> Proc (f pdesc) | Global _ | Struct _ | Procdecl _ -> decl )
  in
  {module_ with decls}


module Subst = struct
  let rec of_exp_one exp ~id ~by =
    let open Exp in
    match exp with
    | Var id' when Ident.equal id id' ->
        by
    | Var _ | Lvar _ | Const _ | Typ _ ->
        exp
    | Field f ->
        Field {f with exp= of_exp_one f.exp ~id ~by}
    | Index (exp1, exp2) ->
        Index (of_exp_one exp1 ~id ~by, of_exp_one exp2 ~id ~by)
    | Call f ->
        Call {f with args= List.map f.args ~f:(fun exp -> of_exp_one exp ~id ~by)}


  let rec of_exp exp eqs =
    let open Exp in
    match exp with
    | Var id ->
        Ident.Map.find_opt id eqs |> Option.value ~default:exp
    | Lvar _ | Const _ | Typ _ ->
        exp
    | Field f ->
        Field {f with exp= of_exp f.exp eqs}
    | Index (exp1, exp2) ->
        Index (of_exp exp1 eqs, of_exp exp2 eqs)
    | Call f ->
        Call {f with args= List.map f.args ~f:(fun exp -> of_exp exp eqs)}


  let of_instr instr eqs =
    let open Instr in
    match instr with
    | Load args ->
        Load {args with exp= of_exp args.exp eqs}
    | Store args ->
        Store {args with exp1= of_exp args.exp1 eqs; exp2= of_exp args.exp2 eqs}
    | Prune args ->
        Prune {args with exp= of_exp args.exp eqs}
    | Let args ->
        Let {args with exp= of_exp args.exp eqs}


  let of_terminator t eqs =
    let open Terminator in
    match t with
    | Ret exp ->
        Ret (of_exp exp eqs)
    | Jump node_call_list ->
        let f {label; ssa_args} =
          {label; ssa_args= List.map ssa_args ~f:(fun exp -> of_exp exp eqs)}
        in
        Jump (List.map node_call_list ~f)
    | Throw exp ->
        Throw (of_exp exp eqs)
    | Unreachable ->
        t


  let of_node node eqs =
    let open Node in
    let rev_instrs =
      List.fold node.instrs ~init:[] ~f:(fun rev_instrs (instr : Instr.t) ->
          match instr with
          | Let {id} when Ident.Map.mem id eqs ->
              rev_instrs
          | _ ->
              of_instr instr eqs :: rev_instrs )
    in
    let instrs = List.rev rev_instrs in
    {node with last= of_terminator node.last eqs; instrs}


  let of_procdesc pdesc eqs =
    let open ProcDesc in
    {pdesc with nodes= List.map pdesc.nodes ~f:(fun node -> of_node node eqs)}
end

let remove_internal_calls _module =
  let module State = struct
    type t = {instrs_rev: Instr.t list; fresh_ident: Ident.t}

    let push_instr instr state = {state with instrs_rev= instr :: state.instrs_rev}

    let incr_fresh state = {state with fresh_ident= Ident.next state.fresh_ident}
  end in
  let rec flatten_exp (exp : Exp.t) state : Exp.t * State.t =
    match exp with
    | Var _ | Lvar _ | Const _ | Typ _ ->
        (exp, state)
    | Field f ->
        let exp, state = flatten_exp f.exp state in
        (Field {f with exp}, state)
    | Index (exp1, exp2) ->
        let exp1, state = flatten_exp exp1 state in
        let exp2, state = flatten_exp exp2 state in
        (Index (exp1, exp2), state)
    | Call {proc; args; kind} ->
        let args, state = flatten_exp_list args state in
        if ProcDecl.is_side_effect_free_sil_expr proc then (Call {proc; args; kind}, state)
        else
          let fresh = state.State.fresh_ident in
          let new_instr : Instr.t =
            Let {id= fresh; exp= Call {proc; args; kind}; loc= Location.Unknown}
          in
          (Var fresh, State.push_instr new_instr state |> State.incr_fresh)
  and flatten_exp_list exp_list state =
    let exp_list, state =
      List.fold exp_list ~init:([], state) ~f:(fun (args, state) exp ->
          let exp, state = flatten_exp exp state in
          (exp :: args, state) )
    in
    (List.rev exp_list, state)
  in
  let flatten_in_instr (instr : Instr.t) state : State.t =
    match instr with
    | Load args ->
        let exp, state = flatten_exp args.exp state in
        State.push_instr (Load {args with exp}) state
    | Store args ->
        let exp1, state = flatten_exp args.exp1 state in
        let exp2, state = flatten_exp args.exp2 state in
        State.push_instr (Store {args with exp1; exp2}) state
    | Prune args ->
        let exp, state = flatten_exp args.exp state in
        State.push_instr (Prune {args with exp}) state
    | Let {id; exp= Call {proc; args; kind}; loc}
      when not (ProcDecl.is_side_effect_free_sil_expr proc) ->
        let args, state = flatten_exp_list args state in
        State.push_instr (Let {id; exp= Call {proc; args; kind}; loc}) state
    | Let {id; exp; loc} ->
        let exp, state = flatten_exp exp state in
        State.push_instr (Let {id; exp; loc}) state
  in
  let flatten_in_terminator (last : Terminator.t) state : Terminator.t * State.t =
    match last with
    | Ret exp ->
        let exp, state = flatten_exp exp state in
        (Ret exp, state)
    | Jump node_calls ->
        let node_calls_rev, state =
          List.fold node_calls ~init:([], state)
            ~f:(fun (node_calls, state) {Terminator.label; ssa_args} ->
              let ssa_args, state = flatten_exp_list ssa_args state in
              ({Terminator.label; ssa_args} :: node_calls, state) )
        in
        (Jump (List.rev node_calls_rev), state)
    | Throw exp ->
        let exp, state = flatten_exp exp state in
        (Throw exp, state)
    | Unreachable ->
        (last, state)
  in
  let flatten_node (node : Node.t) fresh_ident : Node.t * Ident.t =
    let state =
      let init : State.t = {instrs_rev= []; fresh_ident} in
      List.fold node.instrs ~init ~f:(fun state instr -> flatten_in_instr instr state)
    in
    let last, ({instrs_rev; fresh_ident} : State.t) = flatten_in_terminator node.last state in
    ({node with last; instrs= List.rev instrs_rev}, fresh_ident)
  in
  let flatten_pdesc (pdesc : ProcDesc.t) =
    let fresh = collect_ident_defs pdesc |> Ident.fresh in
    let _, rev_nodes =
      List.fold pdesc.nodes ~init:(fresh, []) ~f:(fun (fresh, instrs) node ->
          let node, fresh = flatten_node node fresh in
          (fresh, node :: instrs) )
    in
    {pdesc with nodes= List.rev rev_nodes}
  in
  module_map_procs ~f:flatten_pdesc _module


(* TODO (T131910123): replace with STORE+LOAD transform *)
let let_propagation module_ =
  let get id ident_map =
    try Ident.Map.find id ident_map
    with Caml.Not_found ->
      L.die InternalError "Textual.let_propagation.get failed: unknown identifier %a" Ident.pp id
  in
  let build_equations pdesc : Exp.t Ident.Map.t =
    (* we collect all rule of the form [id = exp] where [exp] is not a regular call nor an
       allocation *)
    List.fold pdesc.ProcDesc.nodes ~init:Ident.Map.empty ~f:(fun eqs (node : Node.t) ->
        List.fold node.instrs ~init:eqs ~f:(fun eqs (instr : Instr.t) ->
            match instr with
            | Load _ | Store _ | Prune _ ->
                eqs
            | Let {exp= Call {proc}} when not (ProcDecl.is_side_effect_free_sil_expr proc) ->
                eqs
            | Let {id; exp} ->
                Ident.Map.add id exp eqs ) )
  in
  let compute_dependencies equations : Ident.Set.t Ident.Map.t =
    (* for each equation we record which equation it depends on for its evaluation *)
    let domain = Ident.Map.fold (fun id _ set -> Ident.Set.add id set) equations Ident.Set.empty in
    let vars exp = Ident.Set.inter (Exp.vars exp) domain in
    Ident.Map.map vars equations
  in
  let sort_equations equations dependencies : Ident.t list =
    (* returns a topological sorted list of identifiers such that if the equation of [id1] depends
       on [id2], then [id1] is after [id2] in the list.
       [dependencies] must be equal to [compute_dependencies equations] *)
    let init = (Ident.Map.empty, []) in
    let rec visit id ((status, sorted_idents) as state) =
      match Ident.Map.find_opt id status with
      | Some `VisitInProgress ->
          L.die InternalError
            "Textual transformation error: sort_equation was given a set of equations with cyclic \
             dependencies"
      | Some `VisitCompleted ->
          state
      | None ->
          let status = Ident.Map.add id `VisitInProgress status in
          let vars = get id dependencies in
          let status, sorted_idents = Ident.Set.fold visit vars (status, sorted_idents) in
          (Ident.Map.add id `VisitCompleted status, id :: sorted_idents)
    in
    let _, sorted_idents =
      Ident.Map.fold
        (fun id _ ((status, _) as state) ->
          if Ident.Map.mem id status then state else visit id state )
        equations init
    in
    List.rev sorted_idents
  in
  let transform pdesc =
    let equations = build_equations pdesc in
    let dependencies = compute_dependencies equations in
    let sorted = sort_equations equations dependencies in
    (* we saturate the equation set (id1, exp1), .. (idn, expn) by rewriting
       enough in each expi such that none depends on id1, .., idn at the end *)
    let saturated_equations =
      List.fold sorted ~init:Ident.Map.empty ~f:(fun saturated_equations id ->
          let eq = get id equations in
          let vars = get id dependencies in
          let saturated_eq =
            Ident.Set.fold
              (fun id' exp ->
                (* thanks to the topological sort, id' has already been processed *)
                let saturated_eq' = get id' saturated_equations in
                Subst.of_exp_one exp ~id:id' ~by:saturated_eq' )
              vars eq
          in
          Ident.Map.add id saturated_eq saturated_equations )
    in
    Subst.of_procdesc pdesc saturated_equations
  in
  module_map_procs ~f:transform module_


let out_of_ssa module_ =
  let transform (pdesc : ProcDesc.t) : ProcDesc.t =
    let get_node : NodeName.t -> Node.t =
      let map =
        List.fold pdesc.nodes ~init:NodeName.Map.empty ~f:(fun map (node : Node.t) ->
            NodeName.Map.add node.label node map )
      in
      fun node ->
        try NodeName.Map.find node map
        with Caml.Not_found -> L.die InternalError "Textual.remove_ssa_params internal error"
    in
    let zip_ssa_args call_location (node_call : Terminator.node_call) (end_node : Node.t) :
        Instr.t list =
      match
        List.map2 end_node.ssa_parameters node_call.ssa_args ~f:(fun (id, typ) exp2 ->
            let var_name = Ident.to_ssa_var id in
            Instr.Store {exp1= Lvar var_name; typ; exp2; loc= Location.Unknown} )
      with
      | Ok equations ->
          equations
      | Unequal_lengths ->
          L.die InternalError
            "Jmp arguments at %a and block parameters at %a should have the same size" Location.pp
            call_location Location.pp end_node.label_loc
    in
    let build_assignements (start_node : Node.t) : Instr.t list =
      match (start_node.last : Terminator.t) with
      | Ret _ | Throw _ | Unreachable ->
          []
      | Jump node_calls ->
          List.fold node_calls ~init:[] ~f:(fun instrs (node_call : Terminator.node_call) ->
              let end_node : Node.t = get_node node_call.label in
              if List.is_empty end_node.ssa_parameters then instrs
              else
                let let_instrs = zip_ssa_args start_node.last_loc node_call end_node in
                List.rev_append let_instrs instrs )
    in
    let terminator_remove_args (terminator : Terminator.t) : Terminator.t =
      let node_call_remove_args (node_call : Terminator.node_call) : Terminator.node_call =
        {node_call with ssa_args= []}
      in
      match terminator with
      | Ret _ | Throw _ | Unreachable ->
          terminator
      | Jump node_calls ->
          Jump (List.map node_calls ~f:node_call_remove_args)
    in
    let nodes =
      List.map pdesc.nodes ~f:(fun node ->
          let rev_instrs = build_assignements node in
          let load_param (id, typ) : Instr.t =
            Load {id; exp= Lvar (Ident.to_ssa_var id); typ; loc= Location.Unknown}
          in
          let prefix = List.map node.Node.ssa_parameters ~f:load_param in
          let last = terminator_remove_args node.Node.last in
          let instrs =
            if List.is_empty rev_instrs then prefix @ node.Node.instrs
            else prefix @ node.Node.instrs @ List.rev rev_instrs
          in
          ({node with instrs; ssa_parameters= []; last} : Node.t) )
    in
    {pdesc with nodes}
  in
  module_map_procs ~f:transform module_
