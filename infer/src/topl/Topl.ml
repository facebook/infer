(*
 * Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let tt fmt = L.d_printf "ToplTrace: " ; L.d_printfln fmt

let parse topl_file =
  let f ch =
    let lexbuf = Lexing.from_channel ch in
    try ToplParser.properties (ToplLexer.token ()) lexbuf
    with ToplParser.Error ->
      let Lexing.{pos_lnum; pos_bol; pos_cnum; _} = Lexing.lexeme_start_p lexbuf in
      let col = pos_cnum - pos_bol + 1 in
      L.(die UserError) "@[%s:%d:%d: topl parse error@]@\n@?" topl_file pos_lnum col
  in
  try In_channel.with_file topl_file ~f
  with Sys_error msg -> L.(die UserError) "@[topl:%s: %s@]@\n@?" topl_file msg


let properties = lazy (List.concat_map ~f:parse Config.topl_properties)

let automaton = lazy (ToplAutomaton.make (Lazy.force properties))

let is_active () = not (List.is_empty (Lazy.force properties))

let cfg = Cfg.create ()

let sourcefile = SourceFile.create "SynthesizedToplProperty.java"

let sourcefile_location = Location.none sourcefile

(* NOTE: For now, Topl is mostly untyped; that is, everything has type Object. *)
let type_of_paramtyp (_t : Typ.Procname.Parameter.t) : Typ.t =
  let classname = Typ.mk (Tstruct Typ.Name.Java.java_lang_object) in
  Typ.mk (Tptr (classname, Pk_pointer))


(** NOTE: Similar to [JTrans.formals_from_signature]. *)
let formals_of_procname proc_name =
  let params = Typ.Procname.get_parameters proc_name in
  let new_arg_name =
    let n = ref (-1) in
    fun () -> incr n ; Printf.sprintf "arg%d" !n
  in
  let f t =
    let name = Mangled.from_string (new_arg_name ()) in
    let typ = type_of_paramtyp t in
    (name, typ)
  in
  List.map ~f params


(** The procedure description and its nodes will have location [sourcefile_location]. *)
let empty_proc_desc proc_name =
  let attr =
    let formals = formals_of_procname proc_name in
    let is_defined = true in
    let loc = sourcefile_location in
    {(ProcAttributes.default sourcefile proc_name) with formals; is_defined; loc}
  in
  let proc_desc = Cfg.create_proc_desc cfg attr in
  let start_node =
    Procdesc.create_node proc_desc sourcefile_location Procdesc.Node.Start_node []
  in
  let exit_node = Procdesc.create_node proc_desc sourcefile_location Procdesc.Node.Exit_node [] in
  Procdesc.node_set_succs_exn proc_desc start_node [exit_node] [exit_node] ;
  Procdesc.set_start_node proc_desc start_node ;
  Procdesc.set_exit_node proc_desc exit_node ;
  proc_desc


let is_synthesized = function
  | Typ.Procname.Java j ->
      String.equal "topl.Property" (Typ.Procname.Java.get_class_name j)
  | _ ->
      false


let get_proc_desc proc_name =
  if is_synthesized proc_name then Some ((* TODO *) empty_proc_desc proc_name) else None


let get_proc_attr proc_name =
  (* TODO: optimize -- don't generate body just to get attributes  *)
  Option.map ~f:Procdesc.get_attributes (get_proc_desc proc_name)


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
          tt "match name='%s' re='%s' result=%b" name label.ToplAst.procedure_name result ;
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


let topl_class_exp =
  let class_name = Mangled.from_string "topl.Property" in
  let var_name = Pvar.mk_global class_name in
  Exp.Lvar var_name


let topl_class_name : Typ.Name.t = Typ.Name.Java.from_string "topl.Property"

let topl_class_typ = Typ.mk (Tstruct topl_class_name)

let transition_field_name i = Typ.Fieldname.Java.from_string (Printf.sprintf "transition%d" i)

let transition_var i = Exp.Lfield (topl_class_exp, transition_field_name i, topl_class_typ)

let set_transitions loc active_transitions =
  let store i b =
    let value = if b then Exp.one else Exp.zero in
    Sil.Store (transition_var i, topl_class_typ, value, loc)
  in
  Array.mapi ~f:store active_transitions


let topl_fun name n =
  let ret_typ = Some Typ.Name.Java.Split.void in
  let args_typ = List.init n ~f:(fun _ -> Typ.Name.Java.Split.java_lang_object) in
  Exp.Const
    (Const.Cfun
       (Typ.Procname.Java
          (Typ.Procname.Java.make topl_class_name ret_typ name args_typ Typ.Procname.Java.Static)))


let call_save_args loc ret_id ret_typ arg_ts =
  let arg_ts = arg_ts @ [(Exp.Var ret_id, ret_typ)] in
  let e_fun = topl_fun "saveArgs" (List.length arg_ts) in
  let ret_id = Ident.create_fresh Ident.knormal in
  let ret_typ = Typ.mk Tvoid in
  [|Sil.Call ((ret_id, ret_typ), e_fun, arg_ts, loc, CallFlags.default)|]


let call_execute loc =
  let e_fun = topl_fun "execute" 0 in
  let ret_id = Ident.create_fresh Ident.knormal in
  let ret_typ = Typ.mk Tvoid in
  [|Sil.Call ((ret_id, ret_typ), e_fun, [], loc, CallFlags.default)|]


let instrument_instruction = function
  | Sil.Call ((ret_id, ret_typ), e_fun, arg_ts, loc, _call_flags) as i ->
      let active_transitions = get_active_transitions e_fun arg_ts in
      if not (Array.exists ~f:(fun x -> x) active_transitions) then [|i|]
      else
        let i1s = set_transitions loc active_transitions in
        let i2s = call_save_args loc ret_id ret_typ arg_ts in
        let i3s = call_execute loc in
        Array.concat [[|i|]; i1s; i2s; i3s]
  | i ->
      [|i|]


(* PRE: Calling [Tenv.mk_struct tenv <args>] twice is a no-op. *)
let add_types tenv =
  let transition_field i = (transition_field_name i, Typ.mk (Tint IBool), []) in
  let rec transitions n =
    if Int.equal n 0 then [] else transition_field (n - 1) :: transitions (n - 1)
  in
  let statics = transitions (get_transitions_count ()) in
  let _topl_class = Tenv.mk_struct tenv topl_class_name ~statics in
  ()


let instrument tenv procdesc =
  add_types tenv ;
  let f _node = instrument_instruction in
  let _updated = Procdesc.replace_instrs_by procdesc ~f in
  ()
