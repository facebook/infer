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

let is_active () = not (List.is_empty (Lazy.force properties))

let get_proc_desc proc_name =
  (* Avoid calling [ToplMonitor.generate] when inactive to avoid side-effects. *)
  if is_active () then ToplMonitor.generate (Lazy.force automaton) proc_name else None


let get_proc_attr proc_name =
  (* TODO: optimize -- don't generate body just to get attributes  *)
  Option.map ~f:Procdesc.get_attributes (get_proc_desc proc_name)


let max_args = ref 0

let get_max_args () =
  let instrument_max_args = !max_args in
  let automaton_max_args = ToplAutomaton.max_args (Lazy.force automaton) in
  Int.max instrument_max_args automaton_max_args


let get_transitions_count () = ToplAutomaton.tcount (Lazy.force automaton)

(** Checks whether the method name and the number of arguments matches the conditions in a
transition label. Possible optimization: also evaluate if arguments equal certain constants. *)
let evaluate_static_guard label (e_fun, arg_ts) =
  let match_name () =
    match e_fun with
    | Exp.Const (Const.Cfun n) ->
        (* TODO: perhaps handle inheritance *)
        let name = Typ.Procname.hashable_name n in
        let re = Str.regexp label.ToplAst.procedure_name in
        let result = Str.string_match re name 0 in
        if Config.trace_topl then
          tt "match name='%s' re='%s' result=%b@\n" name label.ToplAst.procedure_name result ;
        result
    | _ ->
        false
  in
  let match_args () =
    let same_length xs ys = Int.equal (Caml.List.compare_lengths xs ys) 0 in
    Option.value_map label.ToplAst.arguments ~f:(same_length arg_ts) ~default:true
  in
  match_name () && match_args ()


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
       ((Exp.Var ret_id, ret_typ) :: arg_ts) |]


let call_execute loc =
  let dummy_ret_id = Ident.create_fresh Ident.knormal in
  [|ToplUtils.topl_call dummy_ret_id Tvoid loc ToplName.execute []|]


(* Side-effect: increases [!max_args] when it sees a call with more arguments. *)
let instrument_instruction = function
  | Sil.Call ((ret_id, ret_typ), e_fun, arg_ts, loc, _call_flags) as i ->
      let active_transitions = get_active_transitions e_fun arg_ts in
      if not (Array.exists ~f:(fun x -> x) active_transitions) then [|i|]
      else (
        max_args := Int.max !max_args (List.length arg_ts) ;
        tt "found %d arguments@\n" (List.length arg_ts) ;
        let i1s = set_transitions loc active_transitions in
        let i2s = call_save_args loc ret_id ret_typ arg_ts in
        let i3s = call_execute loc in
        Array.concat [[|i|]; i1s; i2s; i3s] )
  | i ->
      [|i|]


(* PRE: Calling [Tenv.mk_struct tenv <args>] twice is a no-op. *)
let add_types tenv =
  let get_registers () =
    let a = Lazy.force automaton in
    let h = String.Table.create () in
    let record =
      let open ToplAst in
      function
      | SaveInRegister reg | EqualToRegister reg -> Hashtbl.set h ~key:reg ~data:() | _ -> ()
    in
    for i = 0 to ToplAutomaton.tcount a - 1 do
      let label = (ToplAutomaton.transition a i).label in
      record label.return ;
      Option.iter ~f:(List.iter ~f:record) label.arguments
    done ;
    Hashtbl.keys h
  in
  let transition_field i =
    (Typ.Fieldname.Java.from_string (ToplName.transition i), Typ.mk (Tint IBool), [])
  in
  let saved_arg_field i =
    (Typ.Fieldname.Java.from_string (ToplName.saved_arg i), ToplUtils.any_type, [])
  in
  let register_field name =
    (Typ.Fieldname.Java.from_string (ToplName.reg name), ToplUtils.any_type, [])
  in
  let statics =
    let state = (Typ.Fieldname.Java.from_string ToplName.state, Typ.mk (Tint IInt), []) in
    let retval = (Typ.Fieldname.Java.from_string ToplName.retval, ToplUtils.any_type, []) in
    let transitions = List.init (get_transitions_count ()) ~f:transition_field in
    let saved_args = List.init (get_max_args ()) ~f:saved_arg_field in
    let registers = List.map ~f:register_field (get_registers ()) in
    List.concat [[retval; state]; transitions; saved_args; registers]
  in
  let _topl_class = Tenv.mk_struct tenv ToplUtils.topl_class_name ~statics in
  ()


let instrument tenv procdesc =
  if not (ToplUtils.is_synthesized (Procdesc.get_proc_name procdesc)) then (
    let f _node = instrument_instruction in
    tt "instrument@\n" ;
    let _updated = Procdesc.replace_instrs_by procdesc ~f in
    tt "add types %d@\n" !max_args ; add_types tenv ; tt "done@\n" )


(** [lookup_static_var var prop] expects [var] to have the form [Exp.Lfield (obj, fieldname)],
and looks up inside the spatial part of [prop]. This function is currently used only to get
around some limitations:
  (a) one cannot do boolean-conjunction on symbolic heaps; and
  (b) the prover fails to see that 0!=o.f * o|-f->0 is inconsistent
*)
let lookup_static_var env (var : Exp.t) (prop : 'a Prop.t) : Exp.t option =
  let from_strexp = function Sil.Eexp (e, _) -> Some e | _ -> None in
  let get_field field (f, e) = if Typ.Fieldname.equal field f then from_strexp e else None in
  let get_strexp field = function
    | Sil.Estruct (fs, _inst) ->
        List.find_map ~f:(get_field field) fs
    | _ ->
        None
  in
  let get_hpred obj field = function
    | Sil.Hpointsto (obj', se, _typ) when Exp.equal obj obj' ->
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


(**
For each (pre, post) pair of symbolic heaps and each (start, error) pair of Topl automata states,
define
  φ := pre & post & (state_pre==start)
  ψ := φ & (state_post!=error)
where & stands for boolean conjunction, in pre all program variables get suffix "_pre", and in post
all program variables get suffix "_post".

Warn when φ is consistent and ψ is inconsistent. (TODO: experiment with asking if it is not forced
but *possible* to get to error.)

To compute (pre & post) the function [conjoin_props] from above is used, which returns a weaker
formula: in particular, the spatial part of pre is dropped. To get around some limitations of the
prover we also use [lookup_static_var]; if a call to this function fails, we don't warn.
*)
let add_errors exe_env summary =
  let proc_desc = summary.Summary.proc_desc in
  let proc_name = Procdesc.get_proc_name proc_desc in
  if not (ToplUtils.is_synthesized proc_name) then
    let env = Exe_env.get_tenv exe_env proc_name in
    let preposts : Prop.normal BiabductionSummary.spec list =
      let biabduction_summary = summary.Summary.payloads.Payloads.biabduction in
      let check_phase x =
        if not BiabductionSummary.(equal_phase x.phase RE_EXECUTION) then
          L.die InternalError "Topl.add_errors should only be called after RE_EXECUTION"
      in
      let extract_specs x = BiabductionSummary.(normalized_specs_to_specs x.preposts) in
      Option.iter ~f:check_phase biabduction_summary ;
      Option.value_map ~default:[] ~f:extract_specs biabduction_summary
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
          Prop.normalize env (Prop.prop_expmap (Exp.rename_pvars ~f:subscript_pre) pre)
        in
        let pre_start = Prop.conjoin_eq env start_exp start_pre_value pre_start in
        let handle_post (post, _path (* TODO: use for getting a trace*)) =
          let handle_state_post_value state_post_value =
            tt "POST = %a@\n" (Prop.pp_prop Pp.text) post ;
            let loc = Procdesc.get_loc proc_desc in
            let post =
              Prop.normalize env (Prop.prop_expmap (Exp.rename_pvars ~f:subscript_post) post)
            in
            let phi = conjoin_props env post pre_start in
            let psi = Prop.conjoin_neq env error_exp state_post_value phi in
            if (not (is_inconsistent env phi)) && is_inconsistent env psi then (
              let property, _vname = ToplAutomaton.vname (Lazy.force automaton) error in
              let message = Printf.sprintf "property %s reaches error" property in
              tt "INCONSISTENT (WARN)@\n" ;
              Reporting.log_error summary IssueType.topl_error ~loc message )
            else tt "CONSISTENT (do NOT warn)@\n"
          in
          (* Don't warn if [lookup_static_var] fails. *)
          Option.iter ~f:handle_state_post_value (lookup_static_var env state_var post)
        in
        List.iter ~f:handle_post posts
      in
      let start_error_pairs = ToplAutomaton.get_start_error_pairs (Lazy.force automaton) in
      let handle_state_pre_value state_pre_value =
        List.iter ~f:(handle_start_error state_pre_value) start_error_pairs
      in
      (* Don't warn if [lookup_static_var] fails. *)
      Option.iter ~f:handle_state_pre_value (lookup_static_var env state_var pre)
    in
    List.iter ~f:handle_preposts preposts


let sourcefile () =
  if not (is_active ()) then L.die InternalError "Called Topl.sourcefile when Topl is inactive" ;
  ToplMonitor.sourcefile ()


let cfg () =
  if not (is_active ()) then L.die InternalError "Called Topl.cfg when Topl is inactive" ;
  ToplMonitor.cfg ()
