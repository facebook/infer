(*
 * Copyright (c) 2019-present, Facebook, Inc.
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

let get_proc_desc proc_name = ToplMonitor.generate (Lazy.force automaton) proc_name

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
          tt "match name='%s' re='%s' result=%b\n" name label.ToplAst.procedure_name result ;
        result
    | _ ->
        false
  in
  let match_args () =
    let same_length xs ys = Option.is_some (List.zip xs ys) in
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
    Sil.Store (ToplUtils.static_var (ToplName.transition i), ToplUtils.topl_class_typ, value, loc)
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
        tt "found %d arguments\n" (List.length arg_ts) ;
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
    tt "instrument\n" ;
    let _updated = Procdesc.replace_instrs_by procdesc ~f in
    tt "add types %d\n" !max_args ; add_types tenv ; tt "done\n" )


let sourcefile = ToplMonitor.sourcefile

let cfg = ToplMonitor.cfg
