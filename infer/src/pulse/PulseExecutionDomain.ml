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


let back_edge (prev : t list) (next : t list) (num_iters : int) : t list * int =
  (* L.debug Analysis Quiet "PULSEINF: Entered EXECDOM BACK-EDGE NUMITER %u \n" num_iters; *)
  if has_infinite_state next then
    ( (* L.debug Analysis Quiet "PULSEINF: POST already had an infinite state - not adding more \n"; *)
      []
    , -1 )
  else
    let _ = num_iters in
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
    (* let worklen = List.length(workset) in *)
    (* L.debug Analysis Quiet "PULSEINF: BACKEDGE prevlen %d nextlen %d diff %d worklen %d NUMITER %u \n" 
     prevlen nextlen (nextlen - prevlen) worklen num_iters; *)

    (* Do-nothing version to avoid debug output *)
    let print_workset _ = true in
    (* Pulse-inf debug output: useful but verbose *)
    (* let rec print_workset ws =
    match ws with
    | [] -> L.flush_formatters(); true
    | (hd,_)::tl ->                 
       pp Format.std_formatter hd;
       print_workset tl
  in *)

    (* Use this when disabling debug output *)
    let print_warning _ _ _ = () in
    (* Force workset printing *)
    (* let _ = print_workset workset in *)

    (* let print_warning s cnt state =
    let _ = state in 
    L.debug Analysis Quiet "PULSEINF: FOUND infinite state from %s with cnt %i numiter %u \n" s cnt num_iters;
    L.flush_formatters();
    pp Format.err_formatter state;
    L.flush_formatters();
    L.debug Analysis Quiet "PULSEINF: End infinite state -- now printing workset at bugloc (numiter %u) \n" num_iters;
    L.flush_formatters();
    let _ = print_workset workset in
    L.flush_formatters();
    L.debug Analysis Quiet "PULSEINF: End Printing Vulnerable Workset numiter %u \n" num_iters;
    L.flush_formatters(); ()
  in *)
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
    let rec record_pathcond ws kl : int =
      (* This brings 30% less false positives but also remove 10-15% of real bugs involving goto *)
      (* This should stay enabled in the upstream version to minimize FP *)
      let phys_equal4 (a, b, c, d) (e, f, g, h) =
        phys_equal a e && phys_equal b f && phys_equal c g && phys_equal d h
      in
      (* end 30% FP diminish *)
      (* let phys_equal4 _ _ = false in  *)

      (* let curlen = (List.length ws) in *)
      match ws with
      | [] ->
          -1
      | (hd, idx) :: tl -> (
          let cond = extract_pathcond hd in
          let pathcond = Formula.extract_path_cond cond in
          let termcond = Formula.extract_term_cond cond in
          let termcond2 = Formula.extract_term_cond2 cond in
          let key = (cfgnode, termcond, pathcond, termcond2) in
          let dl_ws = get_widenstate () in
          match dl_ws with
          | None ->
              L.debug Analysis Quiet
                "PULSEINF: widenstate htable NONE - should never happen! num_iter %u \n" num_iters ;
              -1
          | Some ws -> (
              let prevstate = Stdlib.Hashtbl.find_opt ws key in
              match prevstate with
              | None ->
                  (* L.debug Analysis Quiet "PULSEINF: Recorded pathcond NOT in htable (ADDING) idx %d numiter %u \n" idx num_iters; *)
                  let _ = add_widenstate key in
                  record_pathcond tl ([key] @ kl)
              | Some _ -> (
                match
                  ( Formula.set_is_empty termcond
                  , Formula.map_is_empty pathcond
                  , Formula.termset_is_empty termcond2 )
                with
                | true, true, true ->
                    idx (* -2 *)
                | _ ->
                    let test = List.mem ~equal:phys_equal4 kl key in
                    if test then
                      (* (L.debug Analysis Quiet "PULSEINF: Recorded pathcond ALREADY in htable (SAME ITER NO BUG) idx %d numiter %u \n" idx num_iters; *)
                      -2
                    else
                      (* L.debug Analysis Quiet "PULSEINF: Recorded pathcond ALREADY in htable! (NON-TERM BUG) idx %d numiter %u \n" idx num_iters; *)
                      idx ) ) )
    in
    let _ = print_workset workset in
    let (repeated_wsidx : int) = if Bool.equal same true then -1 else record_pathcond workset [] in
    let create_infinite_state (hd : t) (cnt : int) : t =
      (* Only create infinite state from a non-error state that is not already an infinite state *)
      match hd with
      | ExceptionRaised astate ->
          print_warning "Exception" cnt hd ;
          InfiniteLoop astate
      | ContinueProgram astate ->
          print_warning "Continue" cnt hd ;
          InfiniteLoop astate
      | AbortProgram astate ->
          AbortProgram astate
      | ExitProgram astate ->
          ExitProgram astate
      | LatentAbortProgram a ->
          LatentAbortProgram a
      | LatentInvalidAccess a ->
          LatentInvalidAccess a
      | InfiniteLoop astate ->
          InfiniteLoop astate
      | LatentSpecializedTypeIssue astate ->
          LatentSpecializedTypeIssue astate
    in
    let create_infinite_state_and_print (state_set : t list) (idx : int) (_case : int) =
      (* L.debug Analysis Quiet "PULSEINF: BUG FOUND - DETECTED REPEATED STATE IDX %d CASE %d numiter %d \n" idx case num_iters; *)
      Metadata.record_alert_node cfgnode ;
      let nth = List.nth state_set idx in
      match nth with
      | None ->
          (* L.debug Analysis Quiet "PULSEINF: create_infinite_state Nth NONE prevlen %u nextlen %u idx %d case %d numiter %u -- SHOULD NEVER HAPPEN! \n" 
       prevlen nextlen idx case num_iters; *)
          ([], -1)
      | Some state ->
          ([create_infinite_state state idx], idx)
    in
    let nextempty = List.is_empty next in
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
          (* L.debug Analysis Quiet "JV: Found duplicate state in POST with index = %d \n" curi; *)
          if isrep && curi > maxi then find_duplicate_state tl (curi + 1) curi
          else find_duplicate_state tl (curi + 1) maxi
      | _ ->
          (* L.debug Analysis Quiet "JV: Chosen duplicate in POST has index = %d \n" maxi; *) maxi
    in
    (* we have a trivial lasso because prev = next - this should trigger an alert *)
    if same && Bool.equal nextempty false then
      (* L.debug Analysis Quiet "PULSEINF: same && nextempty is true - returning (no bug) \n"; *)
      create_infinite_state_and_print next
        (find_duplicate_state next 0 (-1))
        1 (* We have one or more newly created equivalent states in the post, trigger an alert *)
    else if List.is_empty workset && nextlen - prevlen > 0 then
      create_infinite_state_and_print next (find_duplicate_state next 0 (-1)) 2
      (* We have a new post state whose path condition is equivalent to an existing state, trigger an alert *)
    else if repeated_wsidx >= 0 then create_infinite_state_and_print next repeated_wsidx 3
    (* No recurring state detected, return empty *)
      else
      (*L.debug Analysis Quiet "PULSEINF: no recurring state detected - returning (no bug) - same: %b nextempty: %b \n" same nextempty; *)
      let _ = print_workset workset in
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
