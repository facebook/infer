(*
 * Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let initialized = ref false

let properties = ref []

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


let init () =
  if not !initialized then (
    properties := List.concat_map ~f:parse Config.topl_properties ;
    initialized := true )


let active () =
  init () ;
  not (List.is_empty !properties)


let cfg = Cfg.create ()

let sourcefile = SourceFile.create "SynthesizedToplProperty.java"

let sourcefile_location = Location.none sourcefile

(** The procedure description and its nodes will have location [sourcefile_location]. *)
let empty_proc_desc proc_name =
  let attr =
    {(ProcAttributes.default sourcefile proc_name) with is_defined= true; loc= sourcefile_location}
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


let get_proc_desc = function
  | Typ.Procname.Topl_method _ as proc_name ->
      init () ;
      (* TODO: generate code *)
      Some (empty_proc_desc proc_name)
  | _ ->
      None


let get_proc_attr proc_name =
  (* TODO: optimize -- don't generate body just to get attributes  *)
  Option.map ~f:Procdesc.get_attributes (get_proc_desc proc_name)


(** TODO *)
let get_transitions_count () = 1

(** TODO: For now, just makes sure that *some* synthetic method gets called, so that the hookup
mechanism gets tested. Later, it will need to compile TOPL properties into one automaton, number
its transitions, and use that information to implement this function. *)
let get_active_transitions _e_fun _arg_ts =
  let result = [|true|] in
  if not (Int.equal (Array.length result) (get_transitions_count ())) then
    L.(die InternalError) "get_active_transitions should compute one bool for each transition" ;
  result


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


(** TODO *)
let call_save_args _loc _ret_id _ret_typ _arg_ts = [||]

let call_execute loc =
  let ret_id = Ident.create_fresh Ident.knormal in
  let ret_typ = Typ.mk Tvoid in
  let ret_typ_name = Some (Typ.Name.Java.Split.make JConfig.void) in
  let e_fun =
    Exp.Const
      (Const.Cfun
         (Typ.Procname.Topl_method
            (Typ.Procname.Java.make topl_class_name ret_typ_name "execute" []
               Typ.Procname.Java.Static)))
  in
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
  init () ;
  add_types tenv ;
  let f _node = instrument_instruction in
  let _updated = Procdesc.replace_instrs_by procdesc ~f in
  ()
