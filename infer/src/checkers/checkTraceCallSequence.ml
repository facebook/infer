(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module L = Logging
module F = Format
open Dataflow

(** Check the sequence of calls to tracing APIs in a method (intraprocedural). *)

let verbose = false

let widening_threshold = 10


(** Tracing methods of the form (package name, class name, begin name, end name). *)
let tracing_methods =
  let for_testing =
    [
      "", "TraceCallSequence", "begin", "end";
      "", "TraceCallSequence", "beginWrapper", "endWrapper";
    ] in
  for_testing @ FbTraceCalls.tracing_methods


(** Boolean variables to be tracked. *)
let boolean_variables =
  [
    "shouldTrace";
  ]

(** Report a warning in the spec file of the procedure. *)
let report_warning tenv description pn pd loc =
  if verbose then L.stderr "ERROR: %s@." description;
  Checkers.ST.report_error tenv pn pd "CHECKERS_TRACE_CALLS_SEQUENCE" loc description


(** Tracing APIs. *)
module APIs = struct
  let method_match pn pkgname cname mname =
    match pn with
    | Procname.Java pn_java ->
        String.equal (Procname.java_get_method pn_java) mname
        &&
        (match pkgname with
         | "" ->
             String.equal (Procname.java_get_simple_class_name pn_java) cname
         | _ ->
             String.equal (Procname.java_get_class_name pn_java) (pkgname ^ "." ^ cname))
    | _ ->
        false
  let is_begin pn =
    let filter (pkgname, cname, begin_name, _) = method_match pn pkgname cname begin_name in
    List.exists ~f:filter tracing_methods
  let is_end pn =
    let filter (pkgname, cname, _, end_name) = method_match pn pkgname cname end_name in
    List.exists ~f:filter tracing_methods
  let is_begin_or_end pn =
    is_begin pn || is_end pn
end


(** Environment for boolean variables. *)
module Env = struct
  type t = bool String.Map.t [@@deriving compare]
  let empty = String.Map.empty
  let add key data map = String.Map.add ~key ~data map
  let remove key map = String.Map.remove map key
  let get map name = String.Map.find map name
  let pp fmt map =
    let pp_elem fmt (s, b) = F.fprintf fmt "%s:%b" s b in
    let l = String.Map.to_alist map in
    if l <> [] then F.fprintf fmt "%a" (Pp.seq pp_elem) l
end

(** Element for the set domain: an integer (for pending traces), and an environment. *)
module Elem = struct
  type t = int * Env.t [@@deriving compare]
  let pp fmt (i, env) = F.fprintf fmt "(%d %a)" i Env.pp env
  let zero = (0, Env.empty)
  let is_consistent (i, _) = i >= 0
  let get_int (i, _) = i
  let get_env (_, env) = env
  let set_env (i, _) env' = (i, env')
  let incr (i, env) = (i + 1, env)
  let decr (i, env) = (i - 1, env)
end


module ElemSet = Caml.Set.Make(Elem)


module State = struct
  (** Each number represents the difference between begin() and end() on some path. *)
  type t = ElemSet.t

  let to_string n =
    let pp_intset fmt s =
      ElemSet.iter (fun d -> F.fprintf fmt "%a " Elem.pp d) s in
    let pp fmt =
      Format.fprintf fmt "{%a}" pp_intset n in
    F.asprintf "%t" pp

  (** Initial balanced state with empty environment. *)
  let balanced = ElemSet.singleton Elem.zero

  (** State is balanced. *)
  let is_balanced s =
    ElemSet.for_all (fun elem -> Int.equal (Elem.get_int elem) 0) s

  let equal = ElemSet.equal

  let has_zero s = ElemSet.exists (fun elem -> Int.equal (Elem.get_int elem) 0) s

  (** Map a function to the elements of the set, and filter out inconsistencies. *)
  let map2 (f : Elem.t -> Elem.t list) (s : t) : t =
    let l = ElemSet.elements s in
    let l' = List.filter ~f:Elem.is_consistent (List.concat (IList.map f l)) in
    List.fold_right ~f:ElemSet.add l' ~init:ElemSet.empty

  let map (f : Elem.t -> Elem.t) s =
    map2 (fun elem -> [f elem]) s

  (** Increment the number of begin/start. *)
  let incr s =
    map Elem.incr s

  (** Decrement the number of begin/start. *)
  let decr s =
    map Elem.decr s

  (** Max value of the set. *)
  let max s =
    ElemSet.max_elt s

  (** Prune the value of name w.r.t. boolean b. *)
  let prune s name b =
    let f elem =
      let env = Elem.get_env elem in
      match Env.get env name with
      | None ->
          let env' = Env.add name b env in
          let elem' = Elem.set_env elem env' in
          [elem']
      | Some b' ->
          if Bool.equal b b' then [elem]
          else [] in
    map2 f s

  (** Set name to the given bool, or nondeterministic if None. *)
  let set_bool s name b_opt =
    let f elem =
      let env = Elem.get_env elem in
      let env' = match b_opt with
        | None -> Env.remove name env
        | Some b -> Env.add name b env in
      Elem.set_env elem env' in
    map f s

  (** Stop growing sets if they go beyond a threshold. *)
  let widening s_before s_after =
    if ElemSet.cardinal s_after <= widening_threshold
    then s_after
    else s_before

  (** Join two states and perform widening if necessary. *)
  let join s1 s2 =
    let s' = ElemSet.union s1 s2 in
    let s'' = widening s1 s' in
    if verbose then L.stderr "  join %s %s --> %s@." (to_string s1) (to_string s2) (to_string s'');
    s''
end

(** Abstract domain for automata whose state transitions are triggered by calls. *)
module Automaton = struct

  (** Transfer function for a procedure call. *)
  let do_call tenv caller_pn caller_pd callee_pn (s : State.t) loc : State.t =
    let method_name () = match callee_pn with
      | Procname.Java pname_java ->
          Procname.java_get_method pname_java
      | _ ->
          Procname.to_simplified_string callee_pn in
    if APIs.is_begin callee_pn then
      begin
        if verbose then L.stderr "  calling %s@." (method_name ());
        State.incr s
      end
    else if APIs.is_end callee_pn then
      begin
        if verbose then L.stderr "  calling %s@." (method_name ());
        if State.has_zero s then report_warning tenv "too many end/stop" caller_pn caller_pd loc;
        State.decr s
      end
    else s

  (** Transfer function for an instruction. *)
  let do_instr tenv pn pd (instr : Sil.instr) (state : State.t) : State.t =
    match instr with
    | Sil.Call (_, Exp.Const (Const.Cfun callee_pn), _, loc, _) ->
        do_call tenv pn pd callee_pn state loc
    | _ -> state

  (** Check if the final state contains any numbers other than zero (balanced). *)
  let check_final_state tenv pn pd exit_node (s : State.t) : unit =
    if verbose then L.stderr "@.Final: %s@." (State.to_string s);
    if not (State.is_balanced s) then
      begin
        let description = Printf.sprintf "%d missing end/stop" (Elem.get_int (State.max s)) in
        let loc = Procdesc.Node.get_loc exit_node in
        report_warning tenv description pn pd loc
      end

end


(** Abstract domain to track the value of specific boolean variables. *)
module BooleanVars = struct

  (** Check if the expression exp is one of the listed boolean variables. *)
  let exp_boolean_var exp = match exp with
    | Exp.Lvar pvar when Pvar.is_local pvar ->
        let name = Mangled.to_string (Pvar.get_name pvar) in
        if List.mem ~equal:String.equal boolean_variables name
        then Some name
        else None
    | _ -> None

  (** Transfer function for an instruction. *)
  let do_instr _ _ idenv (instr : Sil.instr) (state : State.t) : State.t =
    (* Normalize a boolean condition. *)
    let normalize_condition cond_e =
      match cond_e with
      | Exp.UnOp (Unop.LNot, Exp.BinOp (Binop.Eq, e1, e2), _) ->
          Exp.BinOp (Binop.Ne, e1, e2)
      | Exp.UnOp (Unop.LNot, Exp.BinOp (Binop.Ne, e1, e2), _) ->
          Exp.BinOp (Binop.Eq, e1, e2)
      | _ -> cond_e in

    (* Normalize an instruction. *)
    let normalize_instr = function
      | Sil.Prune (cond_e, loc, tb, ik) ->
          let cond_e' = normalize_condition cond_e in
          Sil.Prune (cond_e', loc, tb, ik)
      | instr -> instr in

    match normalize_instr instr with
    | Sil.Prune (Exp.BinOp (Binop.Eq, _cond_e, Exp.Const (Const.Cint i)), _, _, _)
      when IntLit.iszero i ->
        let cond_e = Idenv.expand_expr idenv _cond_e in
        let state' = match exp_boolean_var cond_e with
          | Some name ->
              if verbose then L.stderr "  prune false on %s@." name;
              State.prune state name false
          | None -> state in
        state'
    | Sil.Prune (Exp.BinOp (Binop.Ne, _cond_e, Exp.Const (Const.Cint i)), _, _, _)
      when IntLit.iszero i ->
        let cond_e = Idenv.expand_expr idenv _cond_e in
        let state' = match exp_boolean_var cond_e with
          | Some name ->
              if verbose then L.stderr "  prune true on %s@." name;
              State.prune state name true
          | None -> state in
        state'
    | Sil.Store (_e1, _, e2, _) ->
        let e1 = Idenv.expand_expr idenv _e1 in
        let state' = match exp_boolean_var e1 with
          | Some name ->
              let b_opt = match e2 with
                | Exp.Const (Const.Cint i) -> Some (not (IntLit.iszero i))
                | _ -> None in
              if verbose then
                begin
                  let b_str = match b_opt with
                    | None -> "nondet"
                    | Some b -> string_of_bool b in
                  L.stderr "  setting %s to %s@." name b_str
                end;
              State.set_bool state name b_opt
          | None -> state in
        state'
    | _ -> state

  let check_final_state _ _ _ _ =
    ()

end


(** State transformation for a cfg node. *)
let do_node tenv pn pd idenv _ node (s : State.t) : (State.t list) * (State.t list) =
  if verbose then L.stderr "N:%d S:%s@." (Procdesc.Node.get_id node :> int) (State.to_string s);

  let curr_state = ref s in

  let do_instr instr : unit =
    let state1 = Automaton.do_instr tenv pn pd instr !curr_state in
    let state2 = BooleanVars.do_instr pn pd idenv instr state1 in
    curr_state := state2 in

  IList.iter do_instr (Procdesc.Node.get_instrs node);
  [!curr_state], [!curr_state]

(** Check the final state at the end of the analysis. *)
let check_final_state tenv proc_name proc_desc exit_node final_s =
  Automaton.check_final_state tenv proc_name proc_desc exit_node final_s;
  BooleanVars.check_final_state proc_name proc_desc exit_node final_s

(** Check that the trace calls are balanced.
    This is done by using the most general control flow including exceptions.
    The begin() and end() function are assumed not to throw exceptions. *)
let callback_check_trace_call_sequence { Callbacks.proc_desc; proc_name; idenv; tenv } : unit =

  let module DFTrace = MakeDF(struct
      type t = State.t
      let equal = State.equal
      let join = State.join
      let do_node = do_node tenv proc_name proc_desc idenv
      let proc_throws pn =
        if APIs.is_begin_or_end pn
        then DoesNotThrow (* assume the tracing function do not throw *)
        else DontKnow
    end) in

  let do_check () =
    begin
      if verbose then L.stderr "@.--@.PROC: %a@." Procname.pp proc_name;
      let transitions = DFTrace.run tenv proc_desc State.balanced in
      let exit_node = Procdesc.get_exit_node proc_desc in
      match transitions exit_node with
      | DFTrace.Transition (final_s, _, _) ->
          check_final_state tenv proc_name proc_desc exit_node final_s
      | DFTrace.Dead_state -> ()
    end in

  if not (APIs.is_begin_or_end proc_name) then do_check ()
