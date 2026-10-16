(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module IRAttributes = Attributes
open PulseBasicInterface
open PulseDomainInterface

(** Syntactic over-approximation of what a loop body may modify, in terms of program variables.
    Variables in [assigned] have their own cells assigned: their value, or the fields and elements
    of a struct or array variable. Variables in [escaped] have their address passed to a call,
    stored, or captured by reference by a closure, so that they and the memory reachable from them
    may change in arbitrary ways. Variables in [written_through] may have the memory reachable from
    their value modified: pointers written through, passed to a call, stored in memory, or copied
    into a local pointer that is written through. This includes pointers that are only updated from
    their own value, like [p = p->next] or [p++], which are not in [assigned] so that they remain
    attached to the data structure they traverse. Writes whose target cannot be traced back to a
    program variable (e.g. through the return value of a call with several pointer arguments) are
    ignored: the memory they write is usually reachable from an argument of that call, which is
    accounted for. Variables in [aggregates] may be structs or arrays: they are accessed through
    their fields or elements or with a struct or array type, or their address escapes without its
    type. [written_cells] are the scalar cells assigned that are a variable or the value of a
    variable followed by fields, like [x.f.g] or [x->f]. *)
type effects =
  { assigned: Pvar.Set.t
  ; escaped: Pvar.Set.t
  ; written_through: Pvar.Set.t
  ; aggregates: Pvar.Set.t
  ; written_cells: written_cell list
  ; pointer_sources: Pvar.t option list Pvar.Map.t
        (** for the local pointer variables assigned in the loop, the variables that each value
            assigned is derived from, if any *) }

and written_cell = {root: [`In of Pvar.t | `Through of Pvar.t]; fields: Fieldname.t list}
[@@deriving compare]

let pp_written_cell fmt {root; fields} =
  ( match root with
  | `In pvar ->
      Pvar.pp_value fmt pvar
  | `Through pvar ->
      F.fprintf fmt "*%a" Pvar.pp_value pvar ) ;
  List.iter fields ~f:(F.fprintf fmt ".%a" Fieldname.pp)


let pp_effects fmt {assigned; escaped; written_through; aggregates; written_cells} =
  F.fprintf fmt "assigned=%a; escaped=%a; written_through=%a; aggregates=%a; written_cells=[%a]"
    Pvar.Set.pp assigned Pvar.Set.pp escaped Pvar.Set.pp written_through Pvar.Set.pp aggregates
    (Pp.semicolon_seq pp_written_cell)
    written_cells


(** the program variable that an expression denoting an address is rooted at: [`In x] if the address
    is inside [x] itself, [`Through x] if it is obtained by dereferencing the value of [x] *)
let rec root_of_addr_exp temps (e : Exp.t) =
  match e with
  | Lvar pvar ->
      Some (`In pvar)
  | Lfield ({exp}, _, _) | Lindex (exp, _) | Cast (_, exp) | BinOp ((PlusPI | MinusPI), exp, _) ->
      root_of_addr_exp temps exp
  | Var id ->
      Ident.Map.find_opt id temps |> Option.map ~f:(fun pvar -> `Through pvar)
  | _ ->
      None


(** the program variable whose value an expression is derived from, if any *)
let rec root_of_value_exp temps (e : Exp.t) =
  match e with
  | Var id ->
      Ident.Map.find_opt id temps
  | Cast (_, e) | BinOp ((PlusPI | MinusPI | PlusA _ | MinusA _), e, _) ->
      root_of_value_exp temps e
  | Lvar _ | Lfield _ | Lindex _ ->
      root_of_addr_exp temps e |> Option.map ~f:(fun (`In pvar | `Through pvar) -> pvar)
  | _ ->
      None


let add_written_through raw pvar = {raw with written_through= Pvar.Set.add pvar raw.written_through}

(** add the effects of passing the address [root] to code that may write through it *)
let add_root raw root =
  match root with
  | Some (`In pvar) ->
      {raw with escaped= Pvar.Set.add pvar raw.escaped}
  | Some (`Through pvar) ->
      add_written_through raw pvar
  | None ->
      raw


(** record the variable that [addr] is inside of as a struct or array if [addr] is the address of
    one of its fields or elements, or if [typ], the type of what [addr] points to, is a struct or
    array type *)
let add_aggregate temps raw (addr : Exp.t) (typ : Typ.t) =
  match root_of_addr_exp temps addr with
  | Some (`In pvar) -> (
    match (addr, typ.desc) with
    | Lvar _, (Tstruct _ | Tarray _) | (Lfield _ | Lindex _ | Cast _ | BinOp _), _ ->
        {raw with aggregates= Pvar.Set.add pvar raw.aggregates}
    | _ ->
        raw )
  | Some (`Through _) | None ->
      raw


(** the cell that [addr] denotes if it is a variable or the value of a variable followed by fields;
    [own_values] are the temporaries that hold the value of a variable *)
let written_cell_of_addr_exp own_values (addr : Exp.t) =
  let rec aux fields (e : Exp.t) =
    match e with
    | Lvar pvar ->
        Some {root= `In pvar; fields}
    | Var id ->
        Ident.Map.find_opt id own_values
        |> Option.map ~f:(fun pvar -> {root= `Through pvar; fields})
    | Lfield ({exp}, field, _) ->
        aux (field :: fields) exp
    | _ ->
        None
  in
  aux [] addr


let formal_types_of_callee (callee : Exp.t) =
  match callee with
  | Const (Cfun callee_pname) ->
      IRAttributes.load callee_pname
      |> Option.value_map ~default:[] ~f:(fun attrs ->
          ProcAttributes.get_pvar_formals attrs |> List.map ~f:snd )
  | _ ->
      []


(** add the variables whose address is part of the value [e], which may be modified through it *)
let add_escaping_roots temps raw (e : Exp.t) =
  (* the type of the variable is not known here: assume it is a struct or array *)
  let add_escaped raw pvar =
    add_root {raw with aggregates= Pvar.Set.add pvar raw.aggregates} (Some (`In pvar))
  in
  match e with
  | Closure {captured_vars} ->
      List.fold captured_vars ~init:raw ~f:(fun raw (captured, {CapturedVar.typ}) ->
          match root_of_addr_exp temps captured with
          | Some (`In pvar) ->
              add_escaped raw pvar
          | Some (`Through _) as root when Typ.is_pointer typ ->
              add_root raw root
          | Some (`Through _) | None ->
              raw )
  | _ -> (
    match root_of_addr_exp temps e with
    | Some (`In pvar) ->
        add_escaped raw pvar
    | Some (`Through _) | None ->
        raw )


let add_call_effects temps callee actuals raw =
  let formal_types = formal_types_of_callee callee in
  List.foldi actuals ~init:raw ~f:(fun i raw ((actual : Exp.t), actual_typ) ->
      match actual with
      | Closure _ ->
          add_escaping_roots temps raw actual
      | _
        when Typ.is_pointer actual_typ
             && (not (Typ.is_pointer_to_const actual_typ))
             && not (Option.exists (List.nth formal_types i) ~f:Typ.is_pointer_to_const) ->
          let raw = add_root raw (root_of_addr_exp temps actual) in
          add_aggregate temps raw actual (Typ.strip_ptr actual_typ)
      | _ ->
          (* the callee cannot write through this argument *)
          raw )


(** the variable that the pointer returned by a call is assumed to be derived from: that of its only
    pointer argument, as for accessors like [next(p)] *)
let root_of_return temps ret_typ actuals =
  if Typ.is_pointer ret_typ then
    match
      List.filter_map actuals ~f:(fun (actual, typ) ->
          if Typ.is_pointer typ then root_of_value_exp temps actual else None )
      |> List.dedup_and_sort ~compare:Pvar.compare
    with
    | [pvar] ->
        Some pvar
    | _ ->
        None
  else None


let add_instr_effects (temps, own_values, raw) (instr : Sil.instr) =
  match instr with
  | Load {id; e} ->
      let temps =
        Option.value_map (root_of_addr_exp temps e) ~default:temps
          ~f:(fun (`In pvar | `Through pvar) -> Ident.Map.add id pvar temps )
      in
      let own_values =
        match e with Lvar pvar -> Ident.Map.add id pvar own_values | _ -> own_values
      in
      (temps, own_values, raw)
  | Store {e1= Lvar pvar; typ; e2} when Typ.is_pointer typ && not (Pvar.is_global pvar) ->
      let source = root_of_value_exp temps e2 in
      let pointer_sources =
        Pvar.Map.update pvar
          (fun sources -> Some (source :: Option.value sources ~default:[]))
          raw.pointer_sources
      in
      (temps, own_values, add_escaping_roots temps {raw with pointer_sources} e2)
  | Store {e1; typ; e2} ->
      let raw =
        match root_of_addr_exp temps e1 with
        | Some (`In pvar) ->
            add_aggregate temps {raw with assigned= Pvar.Set.add pvar raw.assigned} e1 typ
        | Some (`Through pvar) ->
            add_written_through raw pvar
        | None ->
            raw
      in
      let raw = add_escaping_roots temps raw e2 in
      let raw =
        (* the memory that the pointer stored points to may now be modified from elsewhere *)
        match root_of_value_exp temps e2 with
        | Some pvar when Typ.is_pointer typ ->
            add_written_through raw pvar
        | _ ->
            raw
      in
      let raw =
        match (written_cell_of_addr_exp own_values e1, typ.desc) with
        | _, (Tstruct _ | Tarray _) | None, _ ->
            raw
        | Some cell, _ ->
            {raw with written_cells= cell :: raw.written_cells}
      in
      (temps, own_values, raw)
  | Call ((ret_id, ret_typ), callee, actuals, _, _) ->
      let temps =
        Option.value_map (root_of_return temps ret_typ actuals) ~default:temps ~f:(fun pvar ->
            Ident.Map.add ret_id pvar temps )
      in
      (temps, own_values, add_call_effects temps callee actuals raw)
  | Prune _ | Metadata _ ->
      (temps, own_values, raw)


let effects_of_loop loop_nodes =
  let init =
    ( Ident.Map.empty
    , Ident.Map.empty
    , { assigned= Pvar.Set.empty
      ; escaped= Pvar.Set.empty
      ; written_through= Pvar.Set.empty
      ; aggregates= Pvar.Set.empty
      ; written_cells= []
      ; pointer_sources= Pvar.Map.empty } )
  in
  let ( _temps
      , _own_values
      , ({assigned; escaped; written_through; written_cells; pointer_sources} as raw) ) =
    Procdesc.NodeSet.fold
      (fun node acc -> Instrs.fold (Procdesc.Node.get_instrs node) ~init:acc ~f:add_instr_effects)
      loop_nodes init
  in
  let sources pvar = Pvar.Map.find_opt pvar pointer_sources |> Option.value ~default:[] in
  (* whether all the values assigned to [pvar] in the loop are derived from [root], possibly through
     other local pointers assigned in the loop, like [it] in [while ((next = it->next)) it = next] *)
  let rec derived_from root ~visited pvar =
    List.for_all (sources pvar) ~f:(function
      | Some source ->
          Pvar.equal source root
          || (not (Pvar.Set.mem source visited))
             && (not (Pvar.Set.mem source escaped))
             && Pvar.Map.mem source pointer_sources
             && derived_from root ~visited:(Pvar.Set.add source visited) source
      | None ->
          false )
  in
  let self_updated, assigned =
    Pvar.Map.fold
      (fun pvar _ (self_updated, assigned) ->
        if derived_from pvar ~visited:(Pvar.Set.singleton pvar) pvar then
          (Pvar.Set.add pvar self_updated, assigned)
        else (self_updated, Pvar.Set.add pvar assigned) )
      pointer_sources (Pvar.Set.empty, assigned)
  in
  (* writes through a local pointer may write to the memory of the variables it was assigned from *)
  let rec add_sources written_through =
    let written_through' =
      Pvar.Set.fold
        (fun pvar written_through ->
          List.fold (sources pvar) ~init:written_through ~f:(fun written_through source ->
              Option.fold source ~init:written_through ~f:(Fn.flip Pvar.Set.add) ) )
        written_through written_through
    in
    if Pvar.Set.equal written_through written_through' then written_through
    else add_sources written_through'
  in
  let written_through =
    Pvar.Set.union self_updated (add_sources (Pvar.Set.union written_through escaped))
  in
  { raw with
    assigned= Pvar.Set.diff assigned escaped
  ; written_through= Pvar.Set.diff written_through escaped
  ; written_cells= List.dedup_and_sort written_cells ~compare:compare_written_cell }


type loop_info = {effects: effects; loop_nodes: Procdesc.IdSet.t}

let compute_loop_infos pdesc =
  let loop_head_to_loop_nodes =
    Procdesc.Loop.get_loop_head_to_source_nodes pdesc |> Procdesc.Loop.get_loop_head_to_loop_nodes
  in
  Procdesc.NodeMap.fold
    (fun loop_head loop_nodes loop_infos ->
      let loop_info =
        { effects= effects_of_loop loop_nodes
        ; loop_nodes=
            Procdesc.NodeSet.fold
              (fun node ids -> Procdesc.IdSet.add (Procdesc.Node.get_id node) ids)
              loop_nodes Procdesc.IdSet.empty }
      in
      Procdesc.IdMap.add (Procdesc.Node.get_id loop_head) loop_info loop_infos )
    loop_head_to_loop_nodes Procdesc.IdMap.empty


let loop_infos_dls = DLS.new_key (fun () -> lazy Procdesc.IdMap.empty)

let () =
  if Config.is_checker_enabled Pulse then
    AnalysisGlobalState.register_dls_with_proc_desc_and_tenv loop_infos_dls ~init:(fun pdesc _ ->
        lazy (compute_loop_infos pdesc) )


let find_loop_info loop_head_id =
  Procdesc.IdMap.find_opt loop_head_id (Lazy.force (DLS.get loop_infos_dls))


let is_in_loop loop_head_id node =
  Option.exists (find_loop_info loop_head_id) ~f:(fun {loop_nodes} ->
      Procdesc.IdSet.mem (Procdesc.Node.get_id node) loop_nodes )


let havoc path location {assigned; escaped; written_through; aggregates; written_cells} astate =
  let hist = ValueHistory.epoch in
  (* like for unknown calls, record that the memory reachable from these values has been havoced so
     that callers apply the same effect on their (potentially larger) view of that memory *)
  let unknown_effect = Attribute.UnknownEffect (CallEvent.Model "loop iterations", hist) in
  let is_scalar_global pvar = Pvar.is_global pvar && not (Pvar.Set.mem pvar aggregates) in
  (* the remaining iterations may write globals, or through them, that the explored ones did not
     access: add them so that their new values are not abduced from the pre and callers apply the
     effects on them *)
  let astate =
    Pvar.Set.fold
      (fun pvar astate ->
        if Pvar.is_global pvar then fst (AbductiveDomain.Stack.eval hist (Var.of_pvar pvar) astate)
        else astate )
      (List.fold written_cells ~init:(Pvar.Set.union assigned escaped)
         ~f:(fun pvars {root= `In pvar | `Through pvar} -> Pvar.Set.add pvar pvars ) )
      astate
  in
  let stack_addr pvar astate =
    AbductiveDomain.Stack.find_opt (Var.of_pvar pvar) astate |> Option.map ~f:ValueOrigin.value
  in
  (* the address of the variable and, for struct and array variables, of their fields and elements *)
  let own_cells pvar astate =
    Option.value_map (stack_addr pvar astate) ~default:[] ~f:(fun addr ->
        AbductiveDomain.reachable_addresses_from
          ~edge_filter:(function FieldAccess _ | ArrayAccess _ -> true | Dereference -> false)
          (Seq.return addr) astate `Post
        |> AbstractValue.Set.elements )
  in
  let contents cells astate =
    List.filter_map cells ~f:(fun cell ->
        AbductiveDomain.Memory.find_edge_opt cell Dereference astate |> Option.map ~f:fst )
  in
  let set_fresh_contents cell astate =
    AbductiveDomain.Memory.add_edge path (cell, hist) Dereference
      (AbstractValue.mk_fresh (), hist)
      location astate
  in
  (* callers may know cells of a global, or memory reachable from it, that [astate] does not *)
  let add_unknown_effect_if_global pvar astate =
    match stack_addr pvar astate with
    | Some addr when Pvar.is_global pvar ->
        AbductiveDomain.AddressAttributes.add_one addr unknown_effect astate
    | _ ->
        astate
  in
  let astate =
    Pvar.Set.fold
      (fun pvar astate ->
        List.fold
          (contents (own_cells pvar astate) astate)
          ~init:astate
          ~f:(fun astate value ->
            AbductiveDomain.apply_unknown_effect hist value astate
            |> AbductiveDomain.AddressAttributes.add_one value unknown_effect ) )
      written_through astate
  in
  let astate =
    Pvar.Set.fold
      (fun pvar astate ->
        let cells = own_cells pvar astate in
        (* the values overwritten may have been stored elsewhere or freed by the remaining
           iterations: stop tracking the allocations reachable from them, as for unknown calls,
           without changing that memory *)
        let astate =
          AbstractValue.Set.fold AbductiveDomain.AddressAttributes.remove_allocation_attr
            (AbductiveDomain.reachable_addresses_from
               (Stdlib.List.to_seq (contents cells astate))
               astate `Post )
            astate
        in
        let astate =
          List.fold cells ~init:astate ~f:(fun astate cell ->
              let astate = AbductiveDomain.AddressAttributes.initialize cell astate in
              if
                is_scalar_global pvar
                || Option.is_some (AbductiveDomain.Memory.find_edge_opt cell Dereference astate)
              then set_fresh_contents cell astate
              else astate )
        in
        if is_scalar_global pvar then astate else add_unknown_effect_if_global pvar astate )
      assigned astate
  in
  let astate =
    Pvar.Set.fold
      (fun pvar astate ->
        match stack_addr pvar astate with
        | None ->
            astate
        | Some addr ->
            let astate =
              AbductiveDomain.Memory.fold_edges addr astate ~init:astate
                ~f:(fun astate (_, (value, _)) ->
                  AbductiveDomain.AddressAttributes.add_one value unknown_effect astate )
              |> AbductiveDomain.apply_unknown_effect hist addr
            in
            let astate =
              if
                is_scalar_global pvar
                && Option.is_none (AbductiveDomain.Memory.find_edge_opt addr Dereference astate)
              then set_fresh_contents addr astate
              else astate
            in
            add_unknown_effect_if_global pvar astate )
      escaped astate
  in
  (* the pointers written through may be null in callers if their value comes from them, i.e. it is
     the content of a cell in the pre: unless they are known to be valid, split into a state where
     they are null, with no cells to write, and one where they are not; split on at most two
     pointers so that the number of states stays small *)
  let split_on_nullness astates pvar =
    let is_valid astate value =
      let has_edge pre_or_post =
        AbductiveDomain.Memory.exists_edge ~pre_or_post value astate ~f:(fun _ -> true)
      in
      has_edge `Pre || has_edge `Post
    in
    let is_from_caller astate value =
      let is_value_of_cell v (rev_accesses : Access.t list) =
        AbstractValue.equal v value
        && match rev_accesses with Access.Dereference :: _ -> true | _ -> false
      in
      AbductiveDomain.fold_all astate `Pre ~init:false ~finish:Fn.id
        ~f:(fun _ found v rev_accesses ->
          if is_value_of_cell v rev_accesses then Continue_or_stop.Stop true else Continue found )
        ~f_revisit:(fun _ found v rev_accesses -> found || is_value_of_cell v rev_accesses)
    in
    let prune prune_value value astate =
      match prune_value value astate with Sat (Ok astate) -> Some astate | Sat _ | Unsat _ -> None
    in
    if List.length astates >= 4 then astates
    else
      List.concat_map astates ~f:(fun astate ->
          match stack_addr pvar astate with
          | None ->
              [astate]
          | Some addr -> (
              let astate, (value, _) =
                AbductiveDomain.Memory.eval_edge (addr, hist) Dereference astate
              in
              if is_valid astate value || not (is_from_caller astate value) then [astate]
              else
                match
                  List.filter_opt
                    [ prune PulseArithmetic.prune_eq_zero value astate
                    ; prune PulseArithmetic.prune_ne_zero value astate ]
                with
                | [] ->
                    [astate]
                | astates ->
                    astates ) )
  in
  let astates =
    List.filter_map written_cells ~f:(function
      | {root= `Through pvar} ->
          Some pvar
      | {root= `In _} ->
          None )
    |> List.dedup_and_sort ~compare:Pvar.compare
    |> List.fold ~init:[astate] ~f:split_on_nullness
  in
  (* the remaining iterations may also write cells that the explored ones did not access: give them
     an unknown value so that reads after the loop do not abduce their value from the pre *)
  let havoc_written_cells astate =
    List.fold written_cells ~init:astate ~f:(fun astate {root; fields} ->
        let base =
          match root with
          | `In pvar ->
              Option.map (stack_addr pvar astate) ~f:(fun addr -> (astate, (addr, hist)))
          | `Through pvar ->
              Option.bind (stack_addr pvar astate) ~f:(fun addr ->
                  let astate, ((value, _) as value_hist) =
                    AbductiveDomain.Memory.eval_edge (addr, hist) Dereference astate
                  in
                  if PulseArithmetic.is_known_zero astate value then None
                  else Some (astate, value_hist) )
        in
        (* the pointer may be null in callers: add the edges to its fields to the post only so that
           applying the pre does not require it to be valid *)
        let eval_field_edge astate addr_hist field =
          match root with
          | `In _ ->
              AbductiveDomain.Memory.eval_edge addr_hist (FieldAccess field) astate
          | `Through _ -> (
            match
              AbductiveDomain.Memory.find_edge_opt (fst addr_hist) (FieldAccess field) astate
            with
            | Some field_hist ->
                (astate, field_hist)
            | None ->
                let field_hist = (AbstractValue.mk_fresh (), hist) in
                ( AbductiveDomain.Memory.add_edge path addr_hist (FieldAccess field) field_hist
                    location astate
                , field_hist ) )
        in
        Option.value_map base ~default:astate ~f:(fun (astate, addr_hist) ->
            let astate, (cell, _) =
              List.fold fields ~init:(astate, addr_hist) ~f:(fun (astate, addr_hist) field ->
                  eval_field_edge astate addr_hist field )
            in
            if Option.is_some (AbductiveDomain.Memory.find_edge_opt cell Dereference astate) then
              astate
            else AbductiveDomain.AddressAttributes.initialize cell astate |> set_fresh_contents cell ) )
  in
  List.map astates ~f:(fun astate ->
      havoc_written_cells astate |> AbductiveDomain.declare_unknown_values
      |> AbductiveDomain.set_from_interrupted_loop )


(** the generalizations computed so far in the current procedure, so that a loop head that keeps
    receiving the same disjunct gets the same generalized disjunct back and stabilizes *)
let generalized_dls = DLS.new_key (fun () -> [])

let () =
  if Config.is_checker_enabled Pulse then
    AnalysisGlobalState.register_dls_with_proc_desc_and_tenv generalized_dls ~init:(fun _ _ -> [])


let generalize loop_head effects path astate =
  let loop_head_id = Procdesc.Node.get_id loop_head in
  let is_same (loop_head_id', astate') =
    Procdesc.Node.equal_id loop_head_id loop_head_id' && phys_equal astate astate'
  in
  match List.find (DLS.get generalized_dls) ~f:(fun (key, _) -> is_same key) with
  | Some (_, generalized) ->
      generalized
  | None ->
      let generalized = havoc path (Procdesc.Node.get_loc loop_head) effects astate in
      DLS.set generalized_dls (((loop_head_id, astate), generalized) :: DLS.get generalized_dls) ;
      generalized


(** loop heads where the widening threshold has dropped disjuncts so far in the current procedure *)
let cut_loop_heads_dls = DLS.new_key (fun () -> Procdesc.IdSet.empty)

let () =
  if Config.is_checker_enabled Pulse then
    AnalysisGlobalState.register_dls_with_proc_desc_and_tenv cut_loop_heads_dls ~init:(fun _ _ ->
        Procdesc.IdSet.empty )


let is_enabled () =
  Config.pulse_havoc_interrupted_loops
  &&
  (* the havoc relies on [apply_unknown_effect], which only forgets about resources in Java, Hack,
     and Python *)
  match AbductiveDomain.should_havoc_if_unknown () with
  | `ShouldHavoc ->
      true
  | `ShouldOnlyHavocResources ->
      false


let widen_interrupted_loop loop_head ~prev ~next =
  if not (is_enabled ()) then []
  else
    let loop_head_id = Procdesc.Node.get_id loop_head in
    match find_loop_info loop_head_id with
    | None ->
        []
    | Some {effects; loop_nodes} ->
        let mem_prev astate =
          List.exists prev ~f:(function
            | ExecutionDomain.ContinueProgram astate', _ ->
                phys_equal astate astate'
            | _ ->
                false )
        in
        let generalizable ((exec_state : ExecutionDomain.t), (path : PathContext.t)) =
          match exec_state with
          | ContinueProgram ({AbductiveDomain.loop_invariant_under_inference= None} as astate)
            when Option.is_none path.PathContext.loop_exit_only ->
              (* the abstract states of [--pulse-eternal] are dropped at loop exits anyway, and
                 states already generalized need not be generalized again *)
              Some (astate, path)
          | _ ->
              None
        in
        let to_generalize =
          match
            List.filter_map next ~f:(fun disj ->
                Option.filter (generalizable disj) ~f:(fun (astate, _) -> not (mem_prev astate)) )
          with
          | _ :: _ as dropped ->
              DLS.set cut_loop_heads_dls
                (Procdesc.IdSet.add loop_head_id (DLS.get cut_loop_heads_dls)) ;
              dropped
          | [] ->
              let cut_loop_heads = DLS.get cut_loop_heads_dls in
              if
                (not (Procdesc.IdSet.mem loop_head_id cut_loop_heads))
                && Procdesc.IdSet.exists
                     (fun cut_head ->
                       (not (Procdesc.Node.equal_id cut_head loop_head_id))
                       && Procdesc.IdSet.mem cut_head loop_nodes )
                     cut_loop_heads
              then
                (* nothing new reached the loop head, for instance because the states that would
                   have continued its iterations were dropped in an inner loop that was cut too:
                   generalize the most recent state instead *)
                List.find_map prev ~f:generalizable |> Option.to_list
              else []
        in
        let generalized =
          List.concat_map to_generalize ~f:(fun (astate, path) ->
              generalize loop_head effects path astate
              |> List.filter_map ~f:(fun generalized ->
                  if mem_prev generalized then None
                  else
                    Some
                      ( ExecutionDomain.ContinueProgram generalized
                      , PathContext.set_loop_exit_only (Some loop_head_id) path ) ) )
        in
        if not (List.is_empty generalized) then
          L.d_printfln "Havocking the effects of the loop at %a: %a" Procdesc.Node.pp_id
            loop_head_id pp_effects effects ;
        generalized
