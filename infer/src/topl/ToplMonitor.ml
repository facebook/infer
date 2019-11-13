(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let sourcefile =
  (* Avoid side-effect when [sourcefile ()] is never called. *)
  let x =
    lazy
      (let pid = Pid.to_int (Unix.getpid ()) in
       SourceFile.create (Printf.sprintf "SynthesizedToplProperty%d.java" pid) )
  in
  fun () -> Lazy.force x


let cfg =
  let x = lazy (Cfg.create ()) in
  fun () -> Lazy.force x


let sourcefile_location () = Location.none (sourcefile ())

let type_of_paramtyp (_t : Typ.Procname.Parameter.t) : Typ.t = ToplUtils.any_type

(** NOTE: Similar to [JTrans.formals_from_signature]. *)
let formals_of_procname proc_name =
  let params = Typ.Procname.get_parameters proc_name in
  let new_arg_name =
    let n = ref (-1) in
    fun () -> incr n ; ToplName.arg !n
  in
  let f t =
    let name = Mangled.from_string (new_arg_name ()) in
    let typ = type_of_paramtyp t in
    (name, typ)
  in
  List.map ~f params


type node_creator = Procdesc.Node.nodekind -> Sil.instr list -> Procdesc.Node.t

type succ_setter = Procdesc.Node.t -> Procdesc.Node.t list -> unit

type block = {start_node: Procdesc.Node.t; exit_node: Procdesc.Node.t}

(** [node_generator]s are the main concept used for organizing the code below. The main property
of node generators is that they compose, because of their return type. The two arguments
([node_creator] and [succ_setter]) are there mainly to ensure that there is a thin interface
with the underlying (heavily imperative) cfg data-structure from [Procdesc]. *)
type node_generator = node_creator -> succ_setter -> block

let procedure proc_name (make_body : node_generator) : Procdesc.t =
  let attr =
    let formals = formals_of_procname proc_name in
    let is_defined = true in
    let loc = sourcefile_location () in
    {(ProcAttributes.default (sourcefile ()) proc_name) with formals; is_defined; loc}
  in
  let proc_desc = Cfg.create_proc_desc (cfg ()) attr in
  let create_node kind instrs =
    Procdesc.create_node proc_desc (sourcefile_location ()) kind instrs
  in
  let exit_node = create_node Procdesc.Node.Exit_node [] in
  let set_succs node succs = Procdesc.node_set_succs_exn proc_desc node succs [exit_node] in
  let {start_node= body_start; exit_node= body_exit} = make_body create_node set_succs in
  let start_node = create_node Procdesc.Node.Start_node [] in
  set_succs start_node [body_start] ;
  set_succs body_exit [exit_node] ;
  Procdesc.set_start_node proc_desc start_node ;
  Procdesc.set_exit_node proc_desc exit_node ;
  proc_desc


let sequence (gens : node_generator list) : node_generator =
 fun create_node set_succs ->
  let blocks = List.map ~f:(fun g -> g create_node set_succs) gens in
  (* NOTE: Possible optimization: fuse successive stmt nodes, by concatenating instructions. *)
  let rec connect n = function
    | [] ->
        n
    | p :: qs ->
        set_succs n.exit_node [p.start_node] ;
        connect p qs
  in
  match blocks with
  | [] ->
      let n = create_node (Procdesc.Node.Stmt_node Procdesc.Node.MethodBody) [] in
      {start_node= n; exit_node= n}
  | n :: ns ->
      let p = connect n ns in
      {start_node= n.start_node; exit_node= p.exit_node}


(** Substitutes a fresh logical variable for any subexpression that is not an operator, a logical
variable or a constant. Also returns assignments from subexpressions to the logical variables
that replaced them. The goal is to get a Sil.Prune condition in a shape friendly to symbolic
execution. *)
let pure_exp e : Exp.t * Sil.instr list =
  let rec pluck =
    let open Exp in
    let open Sequence.Generator in
    function
    | UnOp (_, e, _) ->
        pluck e
    | BinOp (_, e1, e2) ->
        all_unit [pluck e1; pluck e2]
    | Var _ | Const _ ->
        return ()
    | e ->
        yield e
  in
  let es = Sequence.to_list (Sequence.Generator.run (pluck e)) in
  let es = List.dedup_and_sort ~compare:Exp.compare es in
  let pairs = List.map ~f:(fun e -> (e, Ident.create_fresh Ident.knormal)) es in
  let subst = List.map ~f:(function e, id -> (e, Exp.Var id)) pairs in
  let e' = Sil.exp_replace_exp subst e in
  let mk_load (e, id) =
    Sil.Load
      {id; e; root_typ= ToplUtils.any_type; typ= ToplUtils.any_type; loc= sourcefile_location ()}
  in
  let loads = List.map ~f:mk_load pairs in
  (e', loads)


let gen_if (cond : Exp.t) (true_branch : node_generator) (false_branch : node_generator) :
    node_generator =
 fun create_node set_succs ->
  let start_node = create_node (Procdesc.Node.Stmt_node Procdesc.Node.MethodBody) [] in
  let exit_node = create_node (Procdesc.Node.Stmt_node Procdesc.Node.MethodBody) [] in
  let {start_node= true_start_node; exit_node= true_exit_node} =
    true_branch create_node set_succs
  in
  let {start_node= false_start_node; exit_node= false_exit_node} =
    false_branch create_node set_succs
  in
  (* NOTE: Symbolic execution works with non-pure prune expressions but it generates symbolic
     states from which abstraction then removes too much information. *)
  let cond, preamble = pure_exp cond in
  let prune_true =
    let node_type = Procdesc.Node.Prune_node (true, Sil.Ik_if, PruneNodeKind_MethodBody) in
    let instr = Sil.Prune (cond, sourcefile_location (), true, Sil.Ik_if) in
    create_node node_type (preamble @ [instr])
  in
  let prune_false =
    let node_type = Procdesc.Node.Prune_node (false, Sil.Ik_if, PruneNodeKind_MethodBody) in
    let instr =
      Sil.Prune (Exp.UnOp (Unop.LNot, cond, None), sourcefile_location (), false, Sil.Ik_if)
    in
    create_node node_type (preamble @ [instr])
  in
  set_succs start_node [prune_true; prune_false] ;
  set_succs prune_true [true_start_node] ;
  set_succs prune_false [false_start_node] ;
  set_succs true_exit_node [exit_node] ;
  set_succs false_exit_node [exit_node] ;
  {start_node; exit_node}


let stmt_node instrs : node_generator =
 fun create_node _set_succs ->
  let node = create_node (Procdesc.Node.Stmt_node Procdesc.Node.MethodBody) instrs in
  {start_node= node; exit_node= node}


let sil_assign lhs rhs =
  let tempvar = Ident.create_fresh Ident.knormal in
  [ Sil.Load
      { id= tempvar
      ; e= rhs
      ; root_typ= ToplUtils.any_type
      ; typ= ToplUtils.any_type
      ; loc= sourcefile_location () }
  ; Sil.Store
      { e1= lhs
      ; root_typ= ToplUtils.any_type
      ; typ= ToplUtils.any_type
      ; e2= Exp.Var tempvar
      ; loc= sourcefile_location () } ]


let assign lhs rhs : node_generator = stmt_node (sil_assign lhs rhs)

let simple_call function_name : node_generator =
  let ret_id = Ident.create_fresh Ident.knormal in
  stmt_node [ToplUtils.topl_call ret_id Tvoid (sourcefile_location ()) function_name []]


let gen_maybe_call ret_id : node_generator =
  stmt_node [ToplUtils.topl_call ret_id (Tint IBool) (sourcefile_location ()) ToplName.maybe []]


let arguments_count proc_name = List.length (Typ.Procname.get_parameters proc_name)

(* NOTE: The order of parameters must correspond to what gets generated by [call_save_args]. *)
let generate_save_args _automaton proc_name =
  let n = arguments_count proc_name in
  let local_var = ToplUtils.local_var proc_name in
  procedure proc_name
    (sequence
       ( assign (ToplUtils.static_var ToplName.retval) (local_var (ToplName.arg 0))
       :: List.init (n - 1) ~f:(fun i ->
              assign
                (ToplUtils.static_var (ToplName.saved_arg i))
                (local_var (ToplName.arg (i + 1))) ) ))


let generate_execute automaton proc_name =
  let n = ToplAutomaton.vcount automaton in
  let call_execute_state i = simple_call (ToplName.execute_state i) in
  procedure proc_name (sequence (List.init n ~f:call_execute_state))


let generate_execute_state automaton proc_name =
  let state : ToplAutomaton.vindex =
    let re = Str.regexp "execute_state_\\([0-9]*\\)$" in
    let mname = Typ.Procname.get_method proc_name in
    if Str.string_match re mname 0 then int_of_string (Str.matched_group 1 mname)
    else L.die InternalError "Topl.Monitor.generate_execute_state called for %s" mname
  in
  let condition maybe t : Exp.t =
    let conjunct variable pattern =
      let open ToplAst in
      match pattern with
      | Ignore | SaveInRegister _ ->
          [] (* true *)
      | EqualToRegister i ->
          [Exp.eq variable (ToplUtils.static_var (ToplName.reg i))]
      | EqualToConstant ct ->
          [Exp.eq variable ct]
    in
    let label = (ToplAutomaton.transition automaton t).label in
    let all_conjuncts =
      let arg_conjunct i pattern = conjunct (ToplUtils.static_var (ToplName.saved_arg i)) pattern in
      List.concat
        ( Option.value_map ~default:[] ~f:(fun x -> [x]) maybe
        :: [ToplUtils.static_var (ToplName.transition t)]
        :: conjunct (ToplUtils.static_var ToplName.retval) label.ToplAst.return
        :: Option.value_map ~default:[] ~f:(List.mapi ~f:arg_conjunct) label.ToplAst.arguments )
    in
    Exp.and_nary all_conjuncts
  in
  let skip : node_generator = sequence [] in
  let action t : node_generator =
    let step variable pattern =
      match pattern with
      | ToplAst.SaveInRegister i ->
          assign (ToplUtils.static_var (ToplName.reg i)) variable
      | _ ->
          skip
    in
    let transition = ToplAutomaton.transition automaton t in
    let all_actions =
      let arg_action i pattern = step (ToplUtils.static_var (ToplName.saved_arg i)) pattern in
      stmt_node
        [ Sil.Store
            { e1= ToplUtils.static_var ToplName.state
            ; root_typ= Typ.mk (Tint IInt)
            ; typ= Typ.mk (Tint IInt)
            ; e2= Exp.int (IntLit.of_int transition.target)
            ; loc= sourcefile_location () } ]
      :: step (ToplUtils.static_var ToplName.retval) transition.label.ToplAst.return
      :: Option.value_map ~default:[] ~f:(List.mapi ~f:arg_action)
           transition.label.ToplAst.arguments
    in
    sequence all_actions
  in
  let branch_for_right_state : node_generator =
    let check_transition_maybe t (false_branch : node_generator) : node_generator =
      let tempid = Ident.create_fresh Ident.knormal in
      let tempvar = Exp.Var tempid in
      sequence [gen_maybe_call tempid; gen_if (condition (Some tempvar) t) (action t) false_branch]
    in
    let check_transition t (false_branch : node_generator) : node_generator =
      gen_if (condition None t) (action t) false_branch
    in
    let transitions = ToplAutomaton.outgoing automaton state in
    let fold f init = List.fold_right ~init ~f transitions in
    fold check_transition_maybe (fold check_transition skip)
  in
  let body =
    gen_if
      (Exp.eq (ToplUtils.static_var ToplName.state) (ToplUtils.constant_int state))
      branch_for_right_state skip
  in
  procedure proc_name body


(** INV: For the code generated here, biabduction infers the spec "returned value can be anything" *)
let generate_maybe _automaton proc_name = procedure proc_name (sequence [])

let name_matches re proc_name = Str.string_match re (Typ.Procname.get_method proc_name) 0

let has_name s = name_matches (Str.regexp (s ^ "$"))

let is_save_args = has_name ToplName.save_args

let is_execute = has_name ToplName.execute

let is_execute_state = has_name "execute_state_[0-9]*"

let is_maybe = has_name ToplName.maybe

let maybe_synthesize_it automaton proc_name =
  if ToplUtils.is_synthesized proc_name then
    if is_save_args proc_name then Some (generate_save_args automaton proc_name)
    else if is_execute proc_name then Some (generate_execute automaton proc_name)
    else if is_execute_state proc_name then Some (generate_execute_state automaton proc_name)
    else if is_maybe proc_name then Some (generate_maybe automaton proc_name)
    else
      L.die InternalError "TOPL instrumentation introduced a call to a method that is not generated"
  else None


let generate automaton proc_name =
  IList.force_until_first_some
    [ lazy (Typ.Procname.Hash.find_opt (cfg ()) proc_name)
    ; lazy (maybe_synthesize_it automaton proc_name) ]
