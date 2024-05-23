(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface

type 'ok t = 'ok AccessResult.t SatUnsat.t

module Import = struct
  include PulseResult.Let_syntax

  let bind_bind sat_result ~f =
    let exception Unsat in
    try
      SatUnsat.bind
        (fun result ->
          Sat
            (let* x = result in
             match (f x : _ SatUnsat.t) with Sat y -> y | Unsat -> raise_notrace Unsat ) )
        sat_result
    with Unsat -> SatUnsat.Unsat


  let ( let** ) x f = bind_bind x ~f

  let ( >>== ) x f = bind_bind x ~f

  let map_map sat_result ~f = SatUnsat.map (PulseResult.map ~f) sat_result

  let ( let++ ) x f = map_map x ~f

  let ( >>|| ) x f = map_map x ~f

  let map_bind sat_result ~f = SatUnsat.map (PulseResult.bind ~f) sat_result

  let ( let+* ) x f = map_bind x ~f

  let ( >>|= ) x f = map_bind x ~f

  let bind_result result ~f =
    match result with
    | Ok x ->
        f x
    | FatalError _ as err ->
        Sat err
    | Recoverable (x, errors) ->
        SatUnsat.map
          (fun y ->
            match y with
            | Ok y' ->
                Recoverable (y', errors)
            | Recoverable (y', errors') ->
                Recoverable (y', errors' @ errors)
            | FatalError (fatal, errors') ->
                FatalError (fatal, errors' @ errors) )
          (f x)


  let ( let=* ) result f = bind_result result ~f

  let ( >>>= ) result f = bind_result result ~f

  let map_result result ~f = bind_result result ~f:(fun x -> Sat (f x))

  let ( let=+ ) result f = map_result result ~f

  let ( >>>| ) result f = map_result result ~f

  let ( let<*> ) x f =
    match (x : _ PulseResult.t) with
    | FatalError _ as err ->
        [err]
    | Ok y ->
        f y
    | Recoverable (y, errors) ->
        List.map (f y) ~f:(fun result -> PulseResult.append_errors errors result)


  let ( let<**> ) x f =
    match x with
    | Unsat ->
        []
    | Sat (FatalError _ as err) ->
        [err]
    | Sat (Ok y) ->
        f y
    | Sat (Recoverable (y, errors)) ->
        List.map (f y) ~f:(fun result -> PulseResult.append_errors errors result)


  let bind_sat_result non_disj x f =
    match x with
    | Unsat ->
        ([], non_disj)
    | Sat (FatalError _ as err) ->
        ([err], non_disj)
    | Sat (Ok y) ->
        f y
    | Sat (Recoverable (y, errors)) ->
        let execs, non_disj = f y in
        (List.map execs ~f:(fun result -> PulseResult.append_errors errors result), non_disj)


  let ( let<+> ) x f : _ PulseResult.t list =
    match (x : _ PulseResult.t) with
    | FatalError _ as err ->
        [err]
    | Ok y ->
        [Ok (ExecutionDomain.ContinueProgram (f y))]
    | Recoverable (y, errors) ->
        [Recoverable (ExecutionDomain.ContinueProgram (f y), errors)]


  let ( let<++> ) x f =
    match x with
    | Unsat ->
        []
    | Sat (FatalError _ as err) ->
        [err]
    | Sat (Ok y) ->
        [Ok (ExecutionDomain.ContinueProgram (f y))]
    | Sat (Recoverable (y, errors)) ->
        [Recoverable (ExecutionDomain.ContinueProgram (f y), errors)]


  type access_mode = Read | Write | NoAccess

  type 'abductive_domain_t execution_domain_base_t = 'abductive_domain_t ExecutionDomain.base_t =
    | ContinueProgram of 'abductive_domain_t
    | ExceptionRaised of 'abductive_domain_t
    | ExitProgram of AbductiveDomain.Summary.t
    | AbortProgram of AbductiveDomain.Summary.t
    | LatentAbortProgram of {astate: AbductiveDomain.Summary.t; latent_issue: LatentIssue.t}
    | LatentInvalidAccess of
        { astate: AbductiveDomain.Summary.t
        ; address: DecompilerExpr.t
        ; must_be_valid: Trace.t * Invalidation.must_be_valid_reason option
        ; calling_context: (CallEvent.t * Location.t) list }
    | LatentSpecializedTypeIssue of
        {astate: AbductiveDomain.Summary.t; specialized_type: Typ.Name.t; trace: Trace.t}

  type base_error = AccessResult.error =
    | PotentialInvalidAccess of
        { astate: AbductiveDomain.t
        ; address: DecompilerExpr.t
        ; must_be_valid: Trace.t * Invalidation.must_be_valid_reason option }
    | PotentialInvalidSpecializedCall of
        {astate: AbductiveDomain.t; specialized_type: Typ.Name.t; trace: Trace.t}
    | ReportableError of {astate: AbductiveDomain.t; diagnostic: Diagnostic.t}
    | WithSummary of base_error * AbductiveDomain.Summary.t
end

open Import

let sat_ok : 'ok t -> _ = function
  | Sat (Ok x) ->
      Some x
  | Unsat | Sat (FatalError _ | Recoverable _) ->
      None


let list_fold l ~init ~f =
  List.fold l ~init:(Sat (Ok init)) ~f:(fun result x ->
      let** acc = result in
      f acc x )
