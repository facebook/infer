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
module LatentIssue = PulseLatentIssue
module Formula = PulseFormula
module Metadata = AbstractInterpreter.DisjunctiveMetadata

(* The type variable is needed to distinguish summaries from plain states.

   Some of the variants have summary-typed states instead of plain states, to ensure we have
   normalized them and don't need to normalize them again. *)
type 'abductive_domain_t base_t =
  | ContinueProgram of 'abductive_domain_t
  | InfiniteProgram of 'abductive_domain_t
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
  | InfiniteProgram astate1, InfiniteProgram astate2
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
  | ExceptionRaised astate | ContinueProgram astate | InfiniteProgram astate ->
      astate


let pp_header kind fmt = function
  | InfiniteProgram _ ->
      Pp.with_color kind Red F.pp_print_string fmt "InfiniteProgram"
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


let pp fmt exec_state =
  F.fprintf fmt "%a%a" (pp_header TEXT) exec_state AbductiveDomain.pp (to_astate exec_state)


(* Infinite loop detector creates a new InfiniteProgram state when detected *)

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


let rec find_infinite_state (lst : t list) : bool =
  match lst with
  | hd :: tl -> (
    match hd with InfiniteProgram _ -> true | _ -> find_infinite_state tl )
  | _ ->
      false


let back_edge (prev : t list) (next : t list) (num_iters : int) : t list * int =
  let has_infinite_state = find_infinite_state next in
  if has_infinite_state then ([], -1)
  else
    let _ = num_iters in
    let cfgnode = AnalysisState.get_node_exn () in
    let same = phys_equal prev next in
    let rec detect_elem e lst curi : bool * int =
      match lst with
      | [] ->
          (false, -1)
      | hd :: tl ->
          if leq ~lhs:hd ~rhs:e then (true, curi) else detect_elem e tl (curi + 1)
    in
    let rec compute_workset prev next newset nextit : (t * int) list =
      match next with
      | [] ->
          newset
      | hd :: tl ->
          let res, idx = detect_elem hd prev 0 in
          let recidx = match idx with -1 -> nextit | _ -> idx in
          if res then compute_workset prev tl newset (nextit + 1)
          else [(hd, recidx)] @ compute_workset prev tl newset (nextit + 1)
    in
    let workset = compute_workset prev next [] 0 in
    let prevlen = List.length prev in
    let nextlen = List.length next in
    let worklen = List.length workset in
    let extract_pathcond hd : Formula.t =
      match hd with
      | AbortProgram summary ->
          AbductiveDomain.Summary.get_path_condition summary
      | ExitProgram summary ->
          AbductiveDomain.Summary.get_path_condition summary
      | LatentAbortProgram a ->
          AbductiveDomain.Summary.get_path_condition a.astate
      | LatentInvalidAccess a ->
          AbductiveDomain.Summary.get_path_condition a.astate
      | InfiniteProgram astate ->
          AbductiveDomain.get_path_condition astate
      | ExceptionRaised astate ->
          AbductiveDomain.get_path_condition astate
      | ContinueProgram astate ->
          AbductiveDomain.get_path_condition astate
      | LatentSpecializedTypeIssue {astate; _} ->
          AbductiveDomain.Summary.get_path_condition astate
    in
    (* Used when the workset is not empty *)
    let rec record_pathcond ws kl : int =
      let cmp_four (a, b, c, d) (e, f, g, h) =
        if phys_equal a e && phys_equal b f && phys_equal c g && phys_equal d h then true else false
      in
      match ws with
      | [] ->
          -1
      | (hd, idx) :: tl -> (
          let cond = extract_pathcond hd in
          let pathcond = Formula.extract_path_cond cond in
          let termcond = Formula.extract_terminal_atoms cond in
          let termcond2 = Formula.extract_terminal_terms cond in
          let key = (cfgnode, termcond, pathcond, termcond2) in
          let dl_ws = get_widenstate () in
          match dl_ws with
          | None ->
              -1
          | Some ws -> (
              let prevstate = Stdlib.Hashtbl.find_opt ws key in
              match prevstate with
              | None ->
                  let _ = add_widenstate key in
                  record_pathcond tl (kl @ [key])
              | Some _ -> (
                match
                  ( Formula.set_is_empty termcond
                  , Formula.map_is_empty pathcond
                  , Formula.termset_is_empty termcond2 )
                with
                | true, true, true ->
                    idx
                | _ ->
                    let test = List.mem ~equal:cmp_four kl key in
                    if test then -2 else idx ) ) )
    in
    let (repeated_wsidx : int) = if phys_equal same true then -1 else record_pathcond workset [] in
    let create_infinite_state (hd : t) : t =
      (* Only create infinite state from a non-error state that is not already an infinite state *)
      match hd with
      | ExceptionRaised astate ->
          InfiniteProgram astate
      | ContinueProgram astate ->
          InfiniteProgram astate
      | AbortProgram astate ->
          AbortProgram astate
      | ExitProgram astate ->
          ExitProgram astate
      | LatentAbortProgram a ->
          LatentAbortProgram a
      | LatentInvalidAccess a ->
          LatentInvalidAccess a
      | InfiniteProgram astate ->
          InfiniteProgram astate
      | LatentSpecializedTypeIssue astate ->
          LatentSpecializedTypeIssue astate
    in
    let create_infinite_state_and_print (state_set : t list) (idx : int) (case : int) =
      let _ = case in
      Metadata.record_infinite_node cfgnode ;
      let nth = List.nth state_set idx in
      match nth with None -> ([], -1) | Some state -> ([create_infinite_state state], idx)
    in
    let nextempty = phys_equal (List.length next) 0 in
    (* Identify which state in the post is repeated and generate a new infinite state for it *)
    let rec is_repeated e lst : bool =
      match lst with
      | [] ->
          false
      | hd :: tl ->
          if leq ~lhs:e ~rhs:hd then true else is_repeated e tl
    in
    let rec find_duplicate_state (lst : t list) (curi : int) (maxi : int) : int =
      match lst with
      | hd :: tl ->
          let isrep = is_repeated hd tl in
          if isrep && curi > maxi then find_duplicate_state tl (curi + 1) curi
          else find_duplicate_state tl (curi + 1) maxi
      | _ ->
          maxi
    in
    (* we have a trivial lasso because prev = next - this should trigger an alert *)
    if same && phys_equal nextempty false then
      create_infinite_state_and_print next (find_duplicate_state next 0 (-1)) 1
      (* We have one or more newly created equivalent states in the post, trigger an alert *)
    else if phys_equal worklen 0 && nextlen - prevlen > 0 then
      create_infinite_state_and_print next (find_duplicate_state next 0 (-1)) 2
      (* We have a new post state whose path condition is equivalent to an existing state, trigger an alert *)
    else if repeated_wsidx >= 0 then create_infinite_state_and_print next repeated_wsidx 3
      (* No recurring state detected, return empty *)
    else ([], -1)


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
