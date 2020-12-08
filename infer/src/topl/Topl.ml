(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let tt = ToplUtils.debug

let parse topl_file =
  let f ch =
    let lexbuf = Lexing.from_channel ch in
    try ToplParser.properties (ToplLexer.token ()) lexbuf
    with ToplParser.Error ->
      let Lexing.{pos_lnum; pos_bol; pos_cnum; _} = Lexing.lexeme_start_p lexbuf in
      let col = pos_cnum - pos_bol + 1 in
      L.die UserError "@[%s:%d:%d: topl parse error@]@\n@?" topl_file pos_lnum col
  in
  try In_channel.with_file topl_file ~f
  with Sys_error msg -> L.die UserError "@[topl:%s: %s@]@\n@?" topl_file msg


let properties = lazy (List.concat_map ~f:parse Config.topl_properties)

let automaton = lazy (ToplAutomaton.make (Lazy.force properties))

let is_shallow_requested () =
  Config.is_checker_enabled Checker.ToplOnBiabduction
  || Config.is_checker_enabled Checker.ToplOnPulse


let is_shallow_active () = is_shallow_requested () && not (List.is_empty (Lazy.force properties))

let is_deep_active () =
  (not (is_shallow_requested ())) && not (List.is_empty (Lazy.force properties))


let get_proc_desc proc_name =
  (* Avoid calling [ToplMonitor.generate] when inactive to avoid side-effects. *)
  if is_shallow_active () then ToplMonitor.generate (Lazy.force automaton) proc_name else None


let get_proc_attr proc_name =
  (* TODO: optimize -- don't generate body just to get attributes  *)
  Option.map ~f:Procdesc.get_attributes (get_proc_desc proc_name)


let get_transitions_count () = ToplAutomaton.tcount (Lazy.force automaton)

(* NOTE: Code instrumentation only handles ProcedureNamePattern patterns. *)

(** Checks whether the method name and the number of arguments matches the conditions in a
    transition label. Possible optimization: also evaluate if arguments equal certain constants. *)
let evaluate_static_guard label_o (e_fun, arg_ts) =
  let pname_of_label label =
    match label.ToplAst.pattern with
    | ProcedureNamePattern pname ->
        pname
    | _ ->
        L.die UserError
          "Topl: The implementation based on SIL-instrumentation only supports ProcedureNamePattern"
  in
  let evaluate_nonany label =
    let match_name () =
      match e_fun with
      | Exp.Const (Const.Cfun n) ->
          (* TODO: perhaps handle inheritance *)
          let name = Procname.hashable_name n in
          let re = Str.regexp (pname_of_label label) in
          let result = Str.string_match re name 0 in
          tt "  check name='%s'@\n" name ;
          result
      | _ ->
          tt "  check name-unknown@\n" ;
          false
    in
    let pattern_len = Option.map ~f:List.length label.ToplAst.arguments in
    let match_args () =
      let arg_len = 1 + List.length arg_ts in
      (* patterns include the return value *)
      tt "  check arg-len=%d@\n" arg_len ;
      Option.value_map ~default:true ~f:(Int.equal arg_len) pattern_len
    in
    tt "match name-pattern='%s' arg-len-pattern=%a@\n" (pname_of_label label) (Pp.option Int.pp)
      pattern_len ;
    let log f =
      f ()
      ||
      ( tt "  match result FALSE@\n" ;
        false )
    in
    log match_args && log match_name
    &&
    ( tt "  match result TRUE@\n" ;
      true )
  in
  Option.value_map ~default:true ~f:evaluate_nonany label_o


(** For each transition in the automaton, evaluate its static guard. *)
let get_active_transitions e_fun arg_ts =
  let a = Lazy.force automaton in
  let f i = evaluate_static_guard (ToplAutomaton.transition a i).label (e_fun, arg_ts) in
  Array.init (ToplAutomaton.tcount a) ~f


let set_transitions loc active_transitions =
  let store i b =
    let value = if b then Exp.one else Exp.zero in
    Sil.Store
      { e1= ToplUtils.static_var (ToplName.transition i)
      ; root_typ= ToplUtils.topl_class_typ
      ; typ= ToplUtils.topl_class_typ
      ; e2= value
      ; loc }
  in
  Array.mapi ~f:store active_transitions


let call_save_args loc ret_id ret_typ arg_ts =
  let dummy_ret_id = Ident.create_fresh Ident.knormal in
  [| ToplUtils.topl_call dummy_ret_id Tvoid loc ToplName.save_args
       (arg_ts @ [(Exp.Var ret_id, ret_typ)]) |]


let call_execute loc =
  let dummy_ret_id = Ident.create_fresh Ident.knormal in
  [|ToplUtils.topl_call dummy_ret_id Tvoid loc ToplName.execute []|]


let instrument_instruction = function
  | Sil.Call ((ret_id, ret_typ), e_fun, arg_ts, loc, _call_flags) as i ->
      let active_transitions = get_active_transitions e_fun arg_ts in
      let instrument =
        let a = Lazy.force automaton in
        let is_interesting t active = active && not (ToplAutomaton.is_skip a t) in
        Array.existsi ~f:is_interesting active_transitions
      in
      if not instrument then (* optimization *) [|i|]
      else
        let i1s = set_transitions loc active_transitions in
        let i2s = call_save_args loc ret_id ret_typ arg_ts in
        let i3s = call_execute loc in
        Array.concat [[|i|]; i1s; i2s; i3s]
  | i ->
      [|i|]


(* PRE: Calling [Tenv.mk_struct tenv <args>] twice is a no-op. *)
let add_types tenv =
  let get_registers () =
    let a = Lazy.force automaton in
    let h = String.Table.create () in
    let record reg = Hashtbl.set h ~key:reg ~data:() in
    let record_value = function ToplAst.Register reg -> record reg | _ -> () in
    let record_predicate =
      ToplAst.(
        function
        | Binop (_, v1, v2) ->
            record_value v1 ;
            record_value v2
        | Value v ->
            record_value v)
    in
    let record_assignment (reg, _) = record reg in
    let record_label label =
      List.iter ~f:record_predicate label.ToplAst.condition ;
      List.iter ~f:record_assignment label.ToplAst.action
    in
    for i = 0 to ToplAutomaton.tcount a - 1 do
      Option.iter ~f:record_label (ToplAutomaton.transition a i).label
    done ;
    Hashtbl.keys h
  in
  let transition_field i =
    (ToplUtils.make_field (ToplName.transition i), Typ.mk (Tint IBool), [])
  in
  let saved_arg_field i = (ToplUtils.make_field (ToplName.saved_arg i), ToplUtils.any_type, []) in
  let register_field name = (ToplUtils.make_field (ToplName.reg name), ToplUtils.any_type, []) in
  let statics =
    let state = (ToplUtils.make_field ToplName.state, Typ.mk (Tint IInt), []) in
    let transitions = List.init (get_transitions_count ()) ~f:transition_field in
    let saved_args = List.init (ToplAutomaton.max_args (Lazy.force automaton)) ~f:saved_arg_field in
    let registers = List.map ~f:register_field (get_registers ()) in
    List.concat [[state]; transitions; saved_args; registers]
  in
  let _topl_class = Tenv.mk_struct tenv ToplUtils.topl_class_name ~statics in
  ()


let instrument {InterproceduralAnalysis.proc_desc; tenv; _} =
  if not (ToplUtils.is_synthesized (Procdesc.get_proc_name proc_desc)) then (
    let f _node = instrument_instruction in
    tt "instrument@\n" ;
    let _updated = Procdesc.replace_instrs_by proc_desc ~f in
    tt "add types@\n" ;
    add_types tenv ;
    tt "done@\n" )


(** [lookup_static_var var prop] expects [var] to have the form [Exp.Lfield (obj, fieldname)], and
    looks up inside the spatial part of [prop]. This function is currently used only to get around
    some limitations:

    + one cannot do boolean-conjunction on symbolic heaps; and
    + the prover fails to see that 0!=o.f * o|-f->0 is inconsistent *)
let lookup_static_var env (var : Exp.t) (prop : 'a Prop.t) : Exp.t option =
  let from_strexp = function Predicates.Eexp (e, _) -> Some e | _ -> None in
  let get_field field (f, e) = if Fieldname.equal field f then from_strexp e else None in
  let get_strexp field = function
    | Predicates.Estruct (fs, _inst) ->
        List.find_map ~f:(get_field field) fs
    | _ ->
        None
  in
  let get_hpred obj field = function
    | Predicates.Hpointsto (obj', se, _typ) when Exp.equal obj obj' ->
        get_strexp field se
    | _ ->
        None
  in
  match var with
  | Exp.Lfield (obj, field, _typ) ->
      let iter = Prop.prop_iter_create prop in
      let iter = Option.bind ~f:(fun x -> Prop.prop_iter_find x (get_hpred obj field)) iter in
      let state = Option.map ~f:(Prop.prop_iter_current env) iter in
      Option.map ~f:snd state
  | _ ->
      None


let is_inconsistent env query =
  tt "ask if inconsistent: %a@\n" (Prop.pp_prop Pp.text) query ;
  let prover_result = Prover.check_inconsistency_base env query in
  tt "prover_result = %b@\n" prover_result ;
  prover_result


let conjoin_props env post pre =
  (* PRE: p and q have no footprints: that would make no sense in pre/posts. *)
  (* TODO: Ideally, this should be boolean-conjunction. The function [Dom.prop_partial_meet]
     comes close but fails in all practical cases. *)
  List.fold ~init:post ~f:(Prop.prop_atom_and env) (Prop.get_pure pre)


(** For each (pre, post) pair of symbolic heaps and each (start, error) pair of Topl automata
    states, define

    φ := pre & post & (state_pre==start)

    ψ := φ & (state_post!=error)

    where & stands for boolean conjunction, in pre all program variables get suffix "_pre", and in
    post all program variables get suffix "_post".

    Warn when φ is consistent and ψ is inconsistent. (TODO: experiment with asking if it is not
    forced but *possible* to get to error.)

    To compute (pre & post) the function [conjoin_props] from above is used, which returns a weaker
    formula: in particular, the spatial part of pre is dropped. To get around some limitations of
    the prover we also use [lookup_static_var]; if a call to this function fails, we don't warn. *)
let add_errors_biabduction {InterproceduralAnalysis.proc_desc; tenv; err_log} biabduction_summary =
  let proc_name = Procdesc.get_proc_name proc_desc in
  if not (ToplUtils.is_synthesized proc_name) then
    let preposts : Prop.normal BiabductionSummary.spec list =
      let check_phase x =
        if not BiabductionSummary.(equal_phase x.phase RE_EXECUTION) then
          L.die InternalError "Topl.add_errors should only be called after RE_EXECUTION"
      in
      let extract_specs x = BiabductionSummary.(normalized_specs_to_specs x.preposts) in
      check_phase biabduction_summary ;
      extract_specs biabduction_summary
    in
    let subscript varname sub = Printf.sprintf "%s_%s" varname sub in
    let subscript_pre varname = subscript varname "pre" in
    let subscript_post varname = subscript varname "post" in
    let state_var = ToplUtils.static_var ToplName.state in
    let handle_preposts BiabductionSummary.{pre; posts} =
      let pre = BiabductionSummary.Jprop.to_prop pre in
      tt "PRE = %a@\n" (Prop.pp_prop Pp.text) pre ;
      let handle_start_error start_pre_value (start, error) =
        let start_exp = Exp.int (IntLit.of_int start) in
        let error_exp = Exp.int (IntLit.of_int error) in
        let pre_start =
          Prop.normalize tenv (Prop.prop_expmap (Exp.rename_pvars ~f:subscript_pre) pre)
        in
        let pre_start = Prop.conjoin_eq tenv start_exp start_pre_value pre_start in
        let handle_post (post, _path (* TODO: use for getting a trace*)) =
          let handle_state_post_value state_post_value =
            tt "POST = %a@\n" (Prop.pp_prop Pp.text) post ;
            let loc = Procdesc.get_loc proc_desc in
            let post =
              Prop.normalize tenv (Prop.prop_expmap (Exp.rename_pvars ~f:subscript_post) post)
            in
            let phi = conjoin_props tenv post pre_start in
            let psi = Prop.conjoin_neq tenv error_exp state_post_value phi in
            if (not (is_inconsistent tenv phi)) && is_inconsistent tenv psi then (
              let message =
                Format.asprintf "%a" ToplAutomaton.pp_message_of_state (Lazy.force automaton, error)
              in
              tt "WARN@\n" ;
              Reporting.log_issue proc_desc err_log ToplOnBiabduction IssueType.topl_biabd_error
                ~loc message )
          in
          (* Don't warn if [lookup_static_var] fails. *)
          Option.iter ~f:handle_state_post_value (lookup_static_var tenv state_var post)
        in
        List.iter ~f:handle_post posts
      in
      let start_error_pairs = ToplAutomaton.get_start_error_pairs (Lazy.force automaton) in
      let handle_state_pre_value state_pre_value =
        List.iter ~f:(handle_start_error state_pre_value) start_error_pairs
      in
      (* Don't warn if [lookup_static_var] fails. *)
      Option.iter ~f:handle_state_pre_value (lookup_static_var tenv state_var pre)
    in
    List.iter ~f:handle_preposts preposts


let sourcefile () =
  if not (is_shallow_active ()) then
    L.die InternalError "Called Topl.sourcefile when Topl is inactive" ;
  ToplMonitor.sourcefile ()


let cfg () =
  if not (is_shallow_active ()) then L.die InternalError "Called Topl.cfg when Topl is inactive" ;
  ToplMonitor.cfg ()


let analyze_with postprocess analyze analysis_data =
  if is_shallow_active () then instrument analysis_data ;
  let summary = analyze analysis_data in
  if is_shallow_active () then Option.iter ~f:(postprocess analysis_data) summary ;
  summary


let analyze_with_biabduction biabduction analysis_data =
  analyze_with add_errors_biabduction biabduction analysis_data


let automaton () = Lazy.force automaton

type 'summary analysis = 'summary InterproceduralAnalysis.t -> 'summary option

type 'summary postprocess = 'summary InterproceduralAnalysis.t -> 'summary -> unit

type 'summary analysis_transformer = 'summary analysis -> 'summary analysis
