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

type configuration = {vertex: vertex; memory: (register * value) list}

type predicate = Binop.t * PathCondition.operand * PathCondition.operand

type simple_state =
  { pre: configuration  (** at the start of the procedure *)
  ; post: configuration  (** at the current program point *)
  ; pruned: predicate list  (** path-condition for the automaton *) }

(* TODO(rgrigore): use Formula.Atom.Set for [pruned]?? *)

(* TODO: include a hash of the automaton in a summary to avoid caching problems. *)
(* TODO: limit the number of simple_states to some configurable number (default ~5) *)
type state = simple_state list

let pp_predicate f (op, l, r) =
  Format.fprintf f "@[%a%a%a@]" PathCondition.pp_operand l Binop.pp op PathCondition.pp_operand r


let pp_mapping f (x, value) = Format.fprintf f "@[%s↦%a@]@," x AbstractValue.pp value

let pp_memory f memory = Format.fprintf f "@[<2>[%a]@]" (pp_comma_seq pp_mapping) memory

let pp_configuration f {vertex; memory} =
  Format.fprintf f "@[{topl-config@;vertex=%d@;memory=%a}@]" vertex pp_memory memory


let pp_simple_state f {pre; post; pruned} =
  Format.fprintf f "@[<2>{topl-simple-state@;pre=%a@;post=%a@;pruned=(%a)}@]" pp_configuration pre
    pp_configuration post (Pp.seq ~sep:"∧" pp_predicate) pruned


let pp_state f = Format.fprintf f "@[<2>[ %a]@]" (pp_comma_seq pp_simple_state)

let start () =
  let a = Topl.automaton () in
  let starts = ToplAutomaton.starts a in
  let mk_memory =
    let registers = ToplAutomaton.registers a in
    fun () -> List.map ~f:(fun r -> (r, AbstractValue.mk_fresh ())) registers
  in
  let configurations = List.map ~f:(fun vertex -> {vertex; memory= mk_memory ()}) starts in
  List.map ~f:(fun c -> {pre= c; post= c; pruned= []}) configurations


let get env x =
  match List.Assoc.find env ~equal:String.equal x with
  | Some v ->
      v
  | None ->
      L.die InternalError "TOPL: Cannot find %s. Should be caught by static checks" x


let set = List.Assoc.add ~equal:String.equal

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
  let conjoin_predicate pruned (predicate : ToplAst.predicate) =
    match predicate with
    | Binop (binop, l, r) ->
        let l = operand_of_value l in
        let r = operand_of_value r in
        let binop = ToplUtils.binop_to binop in
        (binop, l, r) :: pruned
    | Value v ->
        let v = operand_of_value v in
        let one = PathCondition.LiteralOperand IntLit.one in
        (Binop.Ne, v, one) :: pruned
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


let small_step path_condition event simple_states =
  let tmatches = static_match event in
  let evolve_transition memory (transition, tcontext) : (configuration * predicate list) list =
    match transition.ToplAutomaton.label with
    | None ->
        (* "any" transition *)
        [({vertex= transition.ToplAutomaton.target; memory}, [])]
    | Some label ->
        let pruned = eval_guard memory tcontext label.ToplAst.condition in
        let memory = apply_action tcontext label.ToplAst.action memory in
        [({vertex= transition.ToplAutomaton.target; memory}, pruned)]
  in
  let evolve_state_cond ({vertex; memory}, pruned) =
    let path_condition = conjoin_pruned path_condition pruned in
    let simplify result =
      (* TODO(rgrigore): Remove from extra_pruned what is implied by path_condition *)
      let f (_configuration, extra_pruned) = not (is_unsat path_condition extra_pruned) in
      List.filter ~f result
    in
    let tmatches =
      List.filter ~f:(fun (t, _) -> Int.equal vertex t.ToplAutomaton.source) tmatches
    in
    let nonskip = simplify (List.concat_map ~f:(evolve_transition memory) tmatches) in
    let skip =
      let nonskip_pruned_list = List.map ~f:snd nonskip in
      let skip_pruned_list = skip_pruned_of_nonskip_pruned nonskip_pruned_list in
      let f pruned = ({vertex; memory}, pruned) in
      simplify (List.map ~f skip_pruned_list)
    in
    let add_old_pruned (configuration, extra_pruned) = (configuration, extra_pruned @ pruned) in
    List.map ~f:add_old_pruned (List.rev_append nonskip skip)
  in
  let evolve_simple_state {pre; post; pruned} =
    List.map ~f:(fun (post, pruned) -> {pre; post; pruned}) (evolve_state_cond (post, pruned))
  in
  let result = List.concat_map ~f:evolve_simple_state simple_states in
  L.d_printfln "@[<2>PulseTopl.small_step:@;%a@ -> %a@]" pp_state simple_states pp_state result ;
  result


let large_step ~substitution:_ ~condition:_ ~callee_prepost:_ _state = (* TODO *) []
