(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open PulseBasicInterface
module AbductiveDomain = PulseAbductiveDomain
module DecompilerExpr = PulseDecompilerExpr
module Diagnostic = PulseDiagnostic
module Formula = PulseFormula
module LatentIssue = PulseLatentIssue
module L = Logging

(* The type variable is needed to distinguish summaries from plain states.

   Some of the variants have summary-typed states instead of plain states, to ensure we have
   normalized them and don't need to normalize them again. *)
type stopped_execution =
  | ExitProgram of AbductiveDomain.Summary.t
  | AbortProgram of
      {astate: AbductiveDomain.Summary.t; diagnostic: Diagnostic.t; trace_to_issue: Trace.t}
  | LatentAbortProgram of {astate: AbductiveDomain.Summary.t; latent_issue: LatentIssue.t}
  | LatentInvalidAccess of
      { astate: AbductiveDomain.Summary.t
      ; address: DecompilerExpr.t
      ; must_be_valid: Trace.t * Invalidation.must_be_valid_reason option
      ; calling_context: (CallEvent.t * Location.t) list }
  | LatentSpecializedTypeIssue of
      {astate: AbductiveDomain.Summary.t; specialized_type: Typ.Name.t; trace: Trace.t}
[@@deriving equal, compare, yojson_of, variants]

type 'abductive_domain_t base_t =
  | ContinueProgram of 'abductive_domain_t
  | InfiniteLoop of 'abductive_domain_t
  | ExceptionRaised of 'abductive_domain_t
  | Stopped of stopped_execution
[@@deriving equal, compare, yojson_of, variants]

type t = AbductiveDomain.t base_t

let continue astate = ContinueProgram astate

let summary_of_stopped_execution = function
  | ExitProgram astate
  | AbortProgram {astate}
  | LatentAbortProgram {astate}
  | LatentInvalidAccess {astate}
  | LatentSpecializedTypeIssue {astate} ->
      astate


let leq_stopped_execution ~lhs ~rhs =
  phys_equal lhs rhs
  ||
  match (lhs, rhs) with
  | AbortProgram {astate= astate1}, AbortProgram {astate= astate2}
  | ExitProgram astate1, ExitProgram astate2 ->
      AbductiveDomain.Summary.leq ~lhs:astate1 ~rhs:astate2
  | ( LatentAbortProgram {astate= astate1; latent_issue= issue1}
    , LatentAbortProgram {astate= astate2; latent_issue= issue2} ) ->
      LatentIssue.equal issue1 issue2 && AbductiveDomain.Summary.leq ~lhs:astate1 ~rhs:astate2
  | ( LatentInvalidAccess {astate= astate1; address= v1; must_be_valid= _, reason_opt1}
    , LatentInvalidAccess {astate= astate2; address= v2; must_be_valid= _, reason_opt2} ) ->
      DecompilerExpr.equal v1 v2
      && Option.equal Invalidation.equal_must_be_valid_reason reason_opt1 reason_opt2
      && AbductiveDomain.Summary.leq ~lhs:astate1 ~rhs:astate2
  | _ ->
      false


let leq ~lhs ~rhs =
  phys_equal lhs rhs
  ||
  match (lhs, rhs) with
  | Stopped astate1, Stopped astate2 ->
      leq_stopped_execution ~lhs:astate1 ~rhs:astate2
  | ExceptionRaised astate1, ExceptionRaised astate2
  | InfiniteLoop astate1, InfiniteLoop astate2
  | ContinueProgram astate1, ContinueProgram astate2 ->
      AbductiveDomain.leq ~lhs:astate1 ~rhs:astate2
  | _ ->
      false


let to_astate = function
  | Stopped exec ->
      (summary_of_stopped_execution exec :> AbductiveDomain.t)
  | ExceptionRaised astate | ContinueProgram astate | InfiniteLoop astate ->
      astate


let pp_header kind fmt = function
  | InfiniteLoop _ ->
      Pp.with_color kind Red F.pp_print_string fmt "InfiniteLoop"
  | ContinueProgram _ ->
      F.pp_print_string fmt "ContinueProgram"
  | ExceptionRaised _ ->
      Pp.with_color kind Orange F.pp_print_string fmt "ExceptionRaised"
  | Stopped (AbortProgram {diagnostic}) ->
      Pp.with_color kind Red F.pp_print_string fmt "AbortProgram" ;
      let issue_type = Diagnostic.get_issue_type ~latent:false diagnostic in
      let message, _suggestion = Diagnostic.get_message_and_suggestion diagnostic in
      let location = Diagnostic.get_location diagnostic in
      F.fprintf fmt "(%a: %a: %s)" Location.pp location IssueType.pp issue_type message
  | Stopped (ExitProgram _) ->
      Pp.with_color kind Orange F.pp_print_string fmt "ExitProgram"
  | Stopped (LatentAbortProgram {latent_issue}) ->
      Pp.with_color kind Orange F.pp_print_string fmt "LatentAbortProgram" ;
      let diagnostic = LatentIssue.to_diagnostic latent_issue in
      let issue_type = Diagnostic.get_issue_type ~latent:true diagnostic in
      let message, _suggestion = Diagnostic.get_message_and_suggestion diagnostic in
      let location = Diagnostic.get_location diagnostic in
      F.fprintf fmt "(%a: %a: %s)@ %a" Location.pp location IssueType.pp issue_type message
        LatentIssue.pp latent_issue
  | Stopped (LatentInvalidAccess {address; must_be_valid= _}) ->
      Pp.with_color kind Orange F.pp_print_string fmt "LatentInvalidAccess" ;
      F.fprintf fmt "(%a)" DecompilerExpr.pp address
  | Stopped (LatentSpecializedTypeIssue {specialized_type; trace}) ->
      Pp.with_color kind Orange F.pp_print_string fmt "LatentSpecializedTypeIssue" ;
      let origin_location = Trace.get_start_location trace in
      F.fprintf fmt "(%a: %a)" Location.pp origin_location Typ.Name.pp specialized_type


let pp_with_kind kind path_opt fmt exec_state =
  F.fprintf fmt "%a%a" (pp_header kind) exec_state (PulsePp.pp kind path_opt) (to_astate exec_state)


let pp fmt (exec_state : 'abductive_domain_t base_t) =
  F.fprintf fmt "%a%a" (pp_header TEXT) exec_state AbductiveDomain.pp (to_astate exec_state)


(* Pulse infinite: INFINITE_LOOP checker

   This TermKey allows to identify loop states based on four components: CFG Node, Path Condition,
   Termination Atoms and Termination Terms.  Termination atoms are collected from inter-procedural
   propagation termination terms are solely from intra-procedural if/else/loop conditions *)
module TermKey = struct
  type t =
    Procdesc.Node.t
    * int PulseFormula.Atom.Map.t
    * PulseFormula.Atom.Set.t
    * PulseFormula.Term.Set.t
  [@@deriving compare]

  module Set = struct
    include Stdlib.Set.Make (struct
      type nonrec t = t [@@deriving compare]
    end)
  end
end

let widenstate = AnalysisGlobalState.make_dls ~init:(fun () -> Some (Stdlib.Hashtbl.create 16))

let get_widenstate () = DLS.get widenstate

let add_widenstate (key : Procdesc.Node.t) (data : TermKey.Set.t) =
  Utils.with_dls widenstate ~f:(fun widenstate ->
      match widenstate with
      | None ->
          widenstate
      | Some state ->
          Stdlib.Hashtbl.add state key data ;
          widenstate )


let has_infinite_state exec_states =
  List.exists exec_states ~f:(function
    | Stopped (AbortProgram {diagnostic= InfiniteLoopError _}) | InfiniteLoop _ ->
        true
    | _ ->
        false )


let back_edge (prev : t list) (next : t list) (num_iters : int) : int option =
  if has_infinite_state next then None
  else
    let cfgnode = AnalysisState.get_node () |> Option.value_exn in
    let same = phys_equal prev next in
    let rec compute_workset prev next newset nextit : (t * int) list =
      match next with
      | [] ->
          newset
      | hd :: tl -> (
        match List.findi prev ~f:(fun _i prevstate -> leq ~lhs:prevstate ~rhs:hd) with
        | None ->
            (hd, nextit) :: compute_workset prev tl newset (nextit + 1)
        | Some _ ->
            compute_workset prev tl newset (nextit + 1) )
    in
    let workset = compute_workset prev next [] 0 in
    let prevlen = List.length prev in
    let nextlen = List.length next in
    let extract_pathcond hd : Formula.t =
      match hd with
      | Stopped exec ->
          summary_of_stopped_execution exec |> AbductiveDomain.Summary.get_path_condition
      | InfiniteLoop astate | ExceptionRaised astate | ContinueProgram astate ->
          AbductiveDomain.get_path_condition astate
    in
    (* Used when the workset is not empty *)
    let rec record_pathcond ws kl =
      (* This brings 30% less false positives but also remove 10-15% of real bugs involving goto *)
      (* This should stay enabled in the upstream version to minimize FP *)
      let phys_equal4 (a, b, c, d) (e, f, g, h) =
        phys_equal a e && phys_equal b f && phys_equal c g && phys_equal d h
      in
      (* end 30% FP diminish *)
      match ws with
      | [] ->
          None
      | (hd, idx) :: tl -> (
          let cond = extract_pathcond hd in
          let pathcond = Formula.extract_path_cond cond in
          let termcond = Formula.extract_term_cond cond in
          let termcond2 = Formula.extract_term_cond2 cond in
          let (key : TermKey.t) = (cfgnode, pathcond, termcond, termcond2) in
          let dl_ws = get_widenstate () in
          match dl_ws with
          | None ->
              L.die InternalError "INFITE_LOOP: should never happen"
          | Some ws -> (
              let prevstate = Stdlib.Hashtbl.find_opt ws cfgnode in
              match prevstate with
              | None ->
                  let newset = TermKey.Set.add key TermKey.Set.empty in
                  add_widenstate cfgnode newset ;
                  record_pathcond tl (key :: kl)
              | Some tkset -> (
                match TermKey.Set.find_opt key tkset with
                | None ->
                    add_widenstate cfgnode (TermKey.Set.add key tkset) ;
                    record_pathcond tl (key :: kl)
                | Some _ -> (
                  match
                    ( Formula.set_is_empty termcond
                    , Formula.map_is_empty pathcond
                    , Formula.termset_is_empty termcond2 )
                  with
                  | true, true, true ->
                      Some idx (* -2 *)
                  | _ ->
                      if List.mem ~equal:phys_equal4 kl key then (* same iter, no bug *)
                        None
                      (* TODO: here may want to continue instead of short circuiting *)
                      (* record_pathcond tl kl *)
                        else (* non-term bug *)
                        Some idx ) ) ) )
    in
    (* In the first widening iteration (numiter = 0), always set the cfgnode state set to empty
       to prevent staled state. This can happen if func/loop is analyzed multiple times due to
       inter-procedural constraint propagation by the scheduler *)
    let repeated_wsidx =
      if same then None
      else (
        if Int.equal num_iters 0 then add_widenstate cfgnode TermKey.Set.empty ;
        record_pathcond workset [] )
    in
    (* Identify which state in the post is repeated and generate a new infinite state for it *)
    let find_duplicate_state lst =
      let rec loop (lst : t list) i =
        match lst with
        | hd :: tl ->
            let is_repeated = List.exists ~f:(fun rhs -> leq ~lhs:hd ~rhs) tl in
            (* Found duplicate state in POST with index = i *)
            if is_repeated then i else loop tl (i + 1)
        | _ ->
            L.die InternalError "INFITE_LOOP: should never happen"
      in
      loop lst 0
    in
    if (same && not (List.is_empty next)) || (List.is_empty workset && nextlen - prevlen > 0) then
      Some (find_duplicate_state next)
    else repeated_wsidx


type summary = AbductiveDomain.Summary.t base_t [@@deriving compare, equal, yojson_of]

let pp_summary pe fmt (exec_summary : summary) = pp_with_kind pe None fmt (exec_summary :> t)

let equal_fast_stopped_execution exec_state1 exec_state2 =
  phys_equal exec_state1 exec_state2
  ||
  match (exec_state1, exec_state2) with
  | ( AbortProgram {astate= astate1; diagnostic= diagnostic1}
    , AbortProgram {astate= astate2; diagnostic= diagnostic2} ) ->
      (* ignoring traces in [AbortProgram] *)
      phys_equal astate1 astate2 && phys_equal diagnostic1 diagnostic2
  | ExitProgram astate1, ExitProgram astate2 ->
      phys_equal astate1 astate2
  | _ ->
      false


let equal_fast exec_state1 exec_state2 =
  phys_equal exec_state1 exec_state2
  ||
  match (exec_state1, exec_state2) with
  | Stopped astate1, Stopped astate2 ->
      equal_fast_stopped_execution astate1 astate2
  | ContinueProgram astate1, ContinueProgram astate2
  | ExceptionRaised astate1, ExceptionRaised astate2
  | InfiniteLoop astate1, InfiniteLoop astate2 ->
      phys_equal astate1 astate2
  | _ ->
      false


let is_normal (exec_state : t) : bool =
  match exec_state with ExceptionRaised _ -> false | _ -> true


let is_exceptional (exec_state : t) : bool =
  match exec_state with ExceptionRaised _ -> true | _ -> false


let is_executable (exec_state : t) : bool =
  match exec_state with ContinueProgram _ | ExceptionRaised _ -> true | _ -> false


let exceptional_to_normal : t -> t = function
  | ExceptionRaised astate ->
      ContinueProgram astate
  | x ->
      x


let to_name = Variants_of_base_t.to_name
