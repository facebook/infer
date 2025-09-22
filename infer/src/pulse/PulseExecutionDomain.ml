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
module Metadata = AbstractInterpreter.DisjunctiveMetadata

(* The type variable is needed to distinguish summaries from plain states.

   Some of the variants have summary-typed states instead of plain states, to ensure we have
   normalized them and don't need to normalize them again. *)
type 'abductive_domain_t base_t =
  | ContinueProgram of 'abductive_domain_t
  | InfiniteLoop of 'abductive_domain_t
  | ExceptionRaised of 'abductive_domain_t
  | ExitProgram of AbductiveDomain.Summary.t
  | AbortProgram of AbductiveDomain.Summary.t
  | LatentAbortProgram of {astate: AbductiveDomain.Summary.t; latent_issue: LatentIssue.t}
  | LatentInvalidAccess of
      { astate: AbductiveDomain.Summary.t
      ; address: DecompilerExpr.t
      ; must_be_valid: (Trace.t * Invalidation.must_be_valid_reason option[@yojson.opaque])
      ; calling_context: ((CallEvent.t * Location.t) list[@yojson.opaque]) }
  | LatentSpecializedTypeIssue of
      { astate: AbductiveDomain.Summary.t
      ; specialized_type: Typ.Name.t
      ; trace: (Trace.t[@yojson.opaque]) }
[@@deriving equal, compare, yojson_of, variants]

type t = AbductiveDomain.t base_t

let continue astate = ContinueProgram astate

let leq ~lhs ~rhs =
  phys_equal lhs rhs
  ||
  match (lhs, rhs) with
  | AbortProgram astate1, AbortProgram astate2 | ExitProgram astate1, ExitProgram astate2 ->
      AbductiveDomain.Summary.leq ~lhs:astate1 ~rhs:astate2
  | ExceptionRaised astate1, ExceptionRaised astate2
  | InfiniteLoop astate1, InfiniteLoop astate2
  | ContinueProgram astate1, ContinueProgram astate2 ->
      AbductiveDomain.leq ~lhs:astate1 ~rhs:astate2
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


let to_astate = function
  | AbortProgram summary
  | ExitProgram summary
  | LatentAbortProgram {astate= summary}
  | LatentInvalidAccess {astate= summary}
  | LatentSpecializedTypeIssue {astate= summary} ->
      (summary :> AbductiveDomain.t)
  | ExceptionRaised astate | ContinueProgram astate | InfiniteLoop astate ->
      astate


let pp_header kind fmt = function
  | InfiniteLoop _ ->
      Pp.with_color kind Red F.pp_print_string fmt "InfiniteLoop"
  | AbortProgram _ ->
      Pp.with_color kind Red F.pp_print_string fmt "AbortProgram"
  | ContinueProgram _ ->
      F.pp_print_string fmt "ContinueProgram"
  | ExceptionRaised _ ->
      Pp.with_color kind Orange F.pp_print_string fmt "ExceptionRaised"
  | ExitProgram _ ->
      Pp.with_color kind Orange F.pp_print_string fmt "ExitProgram"
  | LatentAbortProgram {latent_issue} ->
      Pp.with_color kind Orange F.pp_print_string fmt "LatentAbortProgram" ;
      let diagnostic = LatentIssue.to_diagnostic latent_issue in
      let message, _suggestion = Diagnostic.get_message_and_suggestion diagnostic in
      let location = Diagnostic.get_location diagnostic in
      F.fprintf fmt "(%a: %s)@ %a" Location.pp location message LatentIssue.pp latent_issue
  | LatentInvalidAccess {address; must_be_valid= _} ->
      Pp.with_color kind Orange F.pp_print_string fmt "LatentInvalidAccess" ;
      F.fprintf fmt "(%a)" DecompilerExpr.pp address
  | LatentSpecializedTypeIssue {specialized_type; trace} ->
      Pp.with_color kind Orange F.pp_print_string fmt "LatentSpecializedTypeIssue" ;
      let origin_location = Trace.get_start_location trace in
      F.fprintf fmt "(%a: %a)" Location.pp origin_location Typ.Name.pp specialized_type


let pp_with_kind kind path_opt fmt exec_state =
  F.fprintf fmt "%a%a" (pp_header kind) exec_state (PulsePp.pp kind path_opt) (to_astate exec_state)


let pp fmt (exec_state : 'abductive_domain_t base_t) =
  F.fprintf fmt "%a%a" (pp_header TEXT) exec_state AbductiveDomain.pp (to_astate exec_state)


(* Pulse infinite *)

let widenstate = AnalysisGlobalState.make_dls ~init:(fun () -> Some (Stdlib.Hashtbl.create 16))

let get_widenstate () = DLS.get widenstate

let add_widenstate key =
  Utils.with_dls widenstate ~f:(fun widenstate ->
      match widenstate with
      | None ->
          widenstate
      | Some state ->
          Stdlib.Hashtbl.add state key () ;
          widenstate )


let has_infinite_state (lst : t list) : bool =
  List.exists ~f:(fun x -> match x with InfiniteLoop _ -> true | _ -> false) lst


let back_edge (prev : t list) (next : t list) (_num_iters : int) : t list * int =
  if has_infinite_state next then
    ( (* POST already had an infinite state - not adding more*)
      []
    , -1 )
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
      | AbortProgram astate
      | ExitProgram astate
      | LatentAbortProgram {astate}
      | LatentInvalidAccess {astate}
      | LatentSpecializedTypeIssue {astate} ->
          AbductiveDomain.Summary.get_path_condition astate
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
          let key = (cfgnode, termcond, pathcond, termcond2) in
          let dl_ws = get_widenstate () in
          match dl_ws with
          | None ->
              L.die InternalError "INFITE_LOOP: should never happen"
          | Some ws -> (
              let prevstate = Stdlib.Hashtbl.find_opt ws key in
              match prevstate with
              | None ->
                  add_widenstate key ;
                  record_pathcond tl (key :: kl)
              | Some _ -> (
                match
                  ( Formula.set_is_empty termcond
                  , Formula.map_is_empty pathcond
                  , Formula.termset_is_empty termcond2 )
                with
                | true, true, true ->
                    Some idx
                | _ ->
                    let test = List.mem ~equal:phys_equal4 kl key in
                    if test then (* same iter - no bug *)
                      None
                    else (* non-term bug *)
                      Some idx ) ) )
    in
    let repeated_wsidx = if same then None else record_pathcond workset [] in
    let create_infinite_state (hd : t) : t =
      (* Only create infinite state from a non-error state that is not already an infinite state *)
      match hd with
      | ContinueProgram astate ->
          InfiniteLoop astate
      | _ ->
          hd
    in
    let create_infinite_state_and_print (state_set : t list) (idx : int) =
      Metadata.record_alert_node cfgnode ;
      let nth =
        List.nth state_set idx
        |> Option.value_or_thunk ~default:(fun () ->
               L.die InternalError "INFITE_LOOP: should never happen" )
      in
      ([create_infinite_state nth], idx)
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
    (* we have a trivial lasso because prev = next - this should trigger an alert *)
    if same && not (List.is_empty next) then
      (* returning (no bug) *)
      create_infinite_state_and_print next (find_duplicate_state next)
      (* We have one or more newly created equivalent states in the post, trigger an alert *)
    else if List.is_empty workset && nextlen - prevlen > 0 then
      create_infinite_state_and_print next (find_duplicate_state next)
      (* We have a new post state whose path condition is equivalent to an existing state, trigger an alert *)
    else
      match repeated_wsidx with
      | Some idx ->
          create_infinite_state_and_print next idx
      (* No recurring state detected, return empty *)
      | None ->
          (* no recurring state detected - returning (no bug) *)
          ([], -1)


type summary = AbductiveDomain.Summary.t base_t [@@deriving compare, equal, yojson_of]

let pp_summary pe fmt (exec_summary : summary) = pp_with_kind pe None fmt (exec_summary :> t)

let equal_fast exec_state1 exec_state2 =
  phys_equal exec_state1 exec_state2
  ||
  match (exec_state1, exec_state2) with
  | AbortProgram astate1, AbortProgram astate2 | ExitProgram astate1, ExitProgram astate2 ->
      phys_equal astate1 astate2
  | ContinueProgram astate1, ContinueProgram astate2 ->
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
