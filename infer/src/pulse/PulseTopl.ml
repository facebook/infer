(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
module L = Logging

type value = AbstractValue.t

type event = Call of {return: value option; arguments: value list; procname: Procname.t}

let pp_comma_seq f xs = Pp.comma_seq ~print_env:Pp.text_break f xs

let pp_event f (Call {return; arguments; procname}) =
  let procname = Procname.hashable_name procname (* as in [static_match] *) in
  Format.fprintf f "@[call@ %a=%s(%a)@]" (Pp.option AbstractValue.pp) return procname
    (pp_comma_seq AbstractValue.pp) arguments


type vertex = ToplAutomaton.vindex

type register = ToplAst.register_name

(* TODO(rgrigore): Change the memory assoc list to a Map. *)
type configuration = {vertex: vertex; memory: (register * value) list}

type predicate = Binop.t * PathCondition.operand * PathCondition.operand

type step =
  { step_location: Location.t
  ; step_predecessor: simple_state  (** state before this step *)
  ; step_data: step_data }

and step_data = SmallStep of event | LargeStep of (Procname.t * (* post *) simple_state)

(* TODO(rgrigore): | LargeStep of simple_state *)
and simple_state =
  { pre: configuration  (** at the start of the procedure *)
  ; post: configuration  (** at the current program point *)
  ; pruned: predicate list  (** path-condition for the automaton *)
  ; last_step: step option  (** for trace error reporting *) }

(* TODO: include a hash of the automaton in a summary to avoid caching problems. *)
(* TODO: limit the number of simple_states to some configurable number (default ~5) *)
type state = simple_state list

let pp_predicate f (op, l, r) =
  Format.fprintf f "@[%a%a%a@]" PathCondition.pp_operand l Binop.pp op PathCondition.pp_operand r


let pp_mapping f (x, value) = Format.fprintf f "@[%s↦%a@]@," x AbstractValue.pp value

let pp_memory f memory = Format.fprintf f "@[<2>[%a]@]" (pp_comma_seq pp_mapping) memory

let pp_configuration f {vertex; memory} =
  Format.fprintf f "@[{ topl-config@;vertex=%d@;memory=%a }@]" vertex pp_memory memory


let pp_simple_state f {pre; post; pruned} =
  Format.fprintf f "@[<2>{ topl-simple-state@;pre=%a@;post=%a@;pruned=(%a) }@]" pp_configuration pre
    pp_configuration post (Pp.seq ~sep:"∧" pp_predicate) pruned


let pp_state f = Format.fprintf f "@[<2>[ %a ]@]" (pp_comma_seq pp_simple_state)

let start () =
  let mk_simple_states () =
    let a = Topl.automaton () in
    let memory =
      List.map ~f:(fun r -> (r, AbstractValue.mk_fresh ())) (ToplAutomaton.registers a)
    in
    let configurations =
      let n = ToplAutomaton.vcount a in
      let f acc vertex = {vertex; memory} :: acc in
      IContainer.forto n ~init:[] ~f
    in
    List.map ~f:(fun c -> {pre= c; post= c; pruned= []; last_step= None}) configurations
  in
  if Topl.is_deep_active () then mk_simple_states () else (* Avoids work later *) []


let get env x =
  match List.Assoc.find env ~equal:String.equal x with
  | Some v ->
      v
  | None ->
      L.die InternalError "TOPL: Cannot find %s. Should be caught by static checks" x


let set = List.Assoc.add ~equal:String.equal

let is_trivially_true (predicate : predicate) =
  match predicate with
  | Eq, AbstractValueOperand l, AbstractValueOperand r when AbstractValue.equal l r ->
      true
  | _ ->
      false


let eval_guard memory tcontext guard =
  let operand_of_value (value : ToplAst.value) : PathCondition.operand =
    match value with
    | Constant (LiteralInt x) ->
        LiteralOperand (IntLit.of_int x)
    | Register reg ->
        AbstractValueOperand (get memory reg)
    | Binding v ->
        AbstractValueOperand (get tcontext v)
  in
  let add predicate pruned = if is_trivially_true predicate then pruned else predicate :: pruned in
  let conjoin_predicate pruned (predicate : ToplAst.predicate) =
    match predicate with
    | Binop (binop, l, r) ->
        let l = operand_of_value l in
        let r = operand_of_value r in
        let binop = ToplUtils.binop_to binop in
        add (binop, l, r) pruned
    | Value v ->
        let v = operand_of_value v in
        let one = PathCondition.LiteralOperand IntLit.one in
        add (Binop.Ne, v, one) pruned
  in
  List.fold ~init:[] ~f:conjoin_predicate guard


let apply_action tcontext assignments memory =
  let apply_one memory (register, variable) = set memory register (get tcontext variable) in
  List.fold ~init:memory ~f:apply_one assignments


type tcontext = (ToplAst.variable_name * AbstractValue.t) list

let pp_tcontext f tcontext =
  Format.fprintf f "@[[%a]@]" (pp_comma_seq (Pp.pair ~fst:String.pp ~snd:AbstractValue.pp)) tcontext


(** Returns a list of transitions whose pattern matches (e.g., event type matches). Each match
    produces a tcontext (transition context), which matches transition-local variables to abstract
    values. *)
let static_match (Call {return; arguments; procname} as event) :
    (ToplAutomaton.transition * tcontext) list =
  (* TODO(rgrigore): if both [Topl.evaluate_static_guard] and [PulseTopl.static_match] remain, try to factor code. *)
  let rev_arguments = List.rev arguments in
  let procname = Procname.hashable_name procname in
  let match_one t =
    let ret c = Some (t, c) in
    let f label =
      let match_name () : bool =
        let re = Str.regexp label.ToplAst.procedure_name in
        Str.string_match re procname 0
      in
      let match_args () : (ToplAutomaton.transition * tcontext) option =
        let match_formals formals =
          let bind ~init rev_formals =
            let f tcontext variable value = (variable, value) :: tcontext in
            match List.fold2 ~init ~f rev_formals rev_arguments with
            | Ok c ->
                ret c
            | Unequal_lengths ->
                None
          in
          match (List.rev formals, return) with
          | [], Some _ ->
              None
          | rev_formals, None ->
              bind ~init:[] rev_formals
          | r :: rev_formals, Some v ->
              bind ~init:[(r, v)] rev_formals
        in
        Option.value_map ~default:(ret []) ~f:match_formals label.ToplAst.arguments
      in
      if match_name () then match_args () else None
    in
    let result = Option.value_map ~default:(ret []) ~f t.ToplAutomaton.label in
    let pp_second pp f (_, x) = pp f x in
    L.d_printfln "@[<2>PulseTopl.static_match:@;transition %a@;event %a@;result %a@]"
      ToplAutomaton.pp_transition t pp_event event
      (Pp.option (pp_second pp_tcontext))
      result ;
    result
  in
  ToplAutomaton.tfilter_map (Topl.automaton ()) ~f:match_one


let conjoin_pruned path_condition pruned =
  let f path_condition (op, l, r) =
    let path_condition, _new_eqs = PathCondition.prune_binop ~negated:false op l r path_condition in
    path_condition
  in
  List.fold ~init:path_condition ~f pruned


let is_unsat path_condition pruned =
  PathCondition.is_unsat_cheap (conjoin_pruned path_condition pruned)


let negate_predicate =
  Binop.(
    function
    | Eq, l, r ->
        (Ne, l, r)
    | Ne, l, r ->
        (Eq, l, r)
    | Ge, l, r ->
        (Lt, r, l)
    | Gt, l, r ->
        (Le, r, l)
    | Le, l, r ->
        (Gt, r, l)
    | Lt, l, r ->
        (Ge, r, l)
    | _ ->
        L.die InternalError
          "PulseTopl.negate_predicate should handle all outputs of ToplUtils.binop_to")


let skip_pruned_of_nonskip_pruned nonskip_list =
  IList.product (List.map ~f:(List.map ~f:negate_predicate) nonskip_list)


let drop_infeasible path_condition state =
  let f {pruned} = not (is_unsat path_condition pruned) in
  List.filter ~f state


let small_step loc path_condition event simple_states =
  let tmatches = static_match event in
  let evolve_transition (old : simple_state) (transition, tcontext) : state =
    let mk ?(memory = old.post.memory) ?(pruned = []) significant =
      let last_step =
        if significant then
          Some {step_location= loc; step_predecessor= old; step_data= SmallStep event}
        else old.last_step
      in
      (* NOTE: old pruned is discarded, because evolve_simple_state needs to see only new prunes
         to determine skip transitions. It will then add back old prunes. *)
      let post = {vertex= transition.ToplAutomaton.target; memory} in
      {old with post; pruned; last_step}
    in
    match transition.ToplAutomaton.label with
    | None ->
        (* "any" transition *)
        let is_loop = Int.equal transition.ToplAutomaton.source transition.ToplAutomaton.target in
        [mk (not is_loop)]
    | Some label ->
        let memory = old.post.memory in
        let pruned = eval_guard memory tcontext label.ToplAst.condition in
        let memory = apply_action tcontext label.ToplAst.action memory in
        [mk ~memory ~pruned true]
  in
  let evolve_simple_state old =
    let path_condition = conjoin_pruned path_condition old.pruned in
    let tmatches =
      List.filter ~f:(fun (t, _) -> Int.equal old.post.vertex t.ToplAutomaton.source) tmatches
    in
    let nonskip =
      drop_infeasible path_condition (List.concat_map ~f:(evolve_transition old) tmatches)
    in
    let skip =
      let nonskip_pruned_list = List.map ~f:(fun {pruned} -> pruned) nonskip in
      let skip_pruned_list = skip_pruned_of_nonskip_pruned nonskip_pruned_list in
      let f pruned = {old with pruned} (* keeps last_step from old *) in
      drop_infeasible path_condition (List.map ~f skip_pruned_list)
    in
    let add_old_pruned s = {s with pruned= List.rev_append s.pruned old.pruned} in
    List.map ~f:add_old_pruned (List.rev_append nonskip skip)
  in
  let result = List.concat_map ~f:evolve_simple_state simple_states in
  L.d_printfln "@[<2>PulseTopl.small_step:@;%a@ -> %a@]" pp_state simple_states pp_state result ;
  result


let sub_value (sub, value) =
  match AbstractValue.Map.find_opt value sub with
  | Some (v, _history) ->
      (sub, v)
  | None ->
      let v = AbstractValue.mk_fresh () in
      let sub = AbstractValue.Map.add value (v, []) sub in
      (sub, v)


let sub_list sub_elem (sub, xs) =
  let f (sub, xs) x =
    let sub, x = sub_elem (sub, x) in
    (sub, x :: xs)
  in
  let sub, xs = List.fold ~init:(sub, []) ~f xs in
  (sub, List.rev xs)


let of_unequal =
  List.Or_unequal_lengths.(
    function
    | Ok x ->
        x
    | Unequal_lengths ->
        L.die InternalError "PulseTopl expected lists to be of equal lengths")


let sub_configuration (sub, {vertex; memory}) =
  let keys, values = List.unzip memory in
  let sub, values = sub_list sub_value (sub, values) in
  let memory = of_unequal (List.zip keys values) in
  (sub, {vertex; memory})


let sub_predicate (sub, predicate) =
  let avo x : PathCondition.operand = AbstractValueOperand x in
  match (predicate : predicate) with
  | op, AbstractValueOperand l, AbstractValueOperand r ->
      let sub, l = sub_value (sub, l) in
      let sub, r = sub_value (sub, r) in
      (sub, (op, avo l, avo r))
  | op, AbstractValueOperand l, r ->
      let sub, l = sub_value (sub, l) in
      (sub, (op, avo l, r))
  | op, l, AbstractValueOperand r ->
      let sub, r = sub_value (sub, r) in
      (sub, (op, l, avo r))
  | _ ->
      (sub, predicate)


let sub_pruned = sub_list sub_predicate

(* Does not substitute in [last_step]: not usually needed, and takes much time. *)
let sub_simple_state (sub, {pre; post; pruned; last_step}) =
  let sub, pre = sub_configuration (sub, pre) in
  let sub, post = sub_configuration (sub, post) in
  let sub, pruned = sub_pruned (sub, pruned) in
  (sub, {pre; post; pruned; last_step})


let sub_state = sub_list sub_simple_state

let large_step ~call_location ~callee_proc_name ~substitution ~condition ~callee_prepost state =
  let seq ((p : simple_state), (q : simple_state)) =
    if not (Int.equal p.post.vertex q.pre.vertex) then None
    else
      let add_eq eqs (register, value_a) =
        let value_b =
          match List.Assoc.find ~equal:String.equal q.pre.memory register with
          | Some x ->
              x
          | None ->
              L.die InternalError "PulseTopl expects all registers in memory"
        in
        let op x = PathCondition.AbstractValueOperand x in
        (Binop.Eq, op value_a, op value_b) :: eqs
      in
      let pruned = List.fold ~init:(p.pruned @ q.pruned) ~f:add_eq p.post.memory in
      let last_step =
        Some
          { step_location= call_location
          ; step_predecessor= p
          ; step_data= LargeStep (callee_proc_name, q) }
      in
      Some {pre= p.pre; post= q.post; pruned; last_step}
  in
  let _updated_substitution, callee_prepost = sub_state (substitution, callee_prepost) in
  (* TODO(rgrigore): may be worth optimizing the cartesian_product *)
  let state = List.filter_map ~f:seq (List.cartesian_product state callee_prepost) in
  drop_infeasible condition state


let filter_for_summary path_condition state =
  List.filter ~f:(fun x -> not (is_unsat path_condition x.pruned)) state


let simplify ~keep state =
  let simplify_simple_state {pre; post; pruned; last_step} =
    (* NOTE(rgrigore): registers could be considered live for the program path_condition as well.
       That should improve precision, but I'm wary of altering what the Pulse program state is just
       because Topl is enabled. *)
    let collect memory keep =
      List.fold ~init:keep ~f:(fun keep (_reg, value) -> AbstractValue.Set.add value keep) memory
    in
    let keep = keep |> collect pre.memory |> collect post.memory in
    let is_live_operand =
      PathCondition.(
        function LiteralOperand _ -> true | AbstractValueOperand v -> AbstractValue.Set.mem v keep)
    in
    let is_live_predicate (_op, l, r) = is_live_operand l && is_live_operand r in
    let pruned = List.filter ~f:is_live_predicate pruned in
    {pre; post; pruned; last_step}
  in
  List.map ~f:simplify_simple_state state


let description_of_step_data step_data =
  let procname =
    match step_data with SmallStep (Call {procname}) | LargeStep (procname, _) -> procname
  in
  Format.fprintf Format.str_formatter "@[call to %a@]" Procname.pp procname ;
  Format.flush_str_formatter ()


let report_errors proc_desc err_log state =
  let a = Topl.automaton () in
  let rec make_trace nesting acc q =
    match q.last_step with
    | None ->
        acc
    | Some {step_location; step_predecessor; step_data} ->
        let description = description_of_step_data step_data in
        let acc =
          Errlog.make_trace_element nesting step_location description []
          ::
          ( match step_data with
          | SmallStep _ ->
              acc
          | LargeStep (_, qq) ->
              make_trace (nesting + 1) acc qq )
        in
        make_trace nesting acc step_predecessor
  in
  let report_simple_state q =
    if ToplAutomaton.is_start a q.pre.vertex && ToplAutomaton.is_error a q.post.vertex then
      let loc = Procdesc.get_loc proc_desc in
      let ltr = make_trace 0 [] q in
      let message = Format.asprintf "%a" ToplAutomaton.pp_message_of_state (a, q.post.vertex) in
      Reporting.log_issue proc_desc err_log ~loc ~ltr ToplOnPulse IssueType.topl_pulse_error message
  in
  List.iter ~f:report_simple_state state
