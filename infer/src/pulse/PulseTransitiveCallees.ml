(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type call_kind = Static | Virtual | Closure [@@deriving equal, compare]

type resolution =
  | ResolvedUsingDynamicType (* the most precise resolution *)
  | ResolvedUsingStaticType (* may not be exact *)
  | Unresolved
    (* the worst resolution because we don't have enough type
       information or the capture was incomplete *)
[@@deriving equal, compare]

module CallSite = struct
  type t =
    { callsite_loc: Location.t
    ; caller_name: (string[@compare.ignore])
    ; caller_loc: (Location.t[@compare.ignore]) }
  [@@deriving compare]

  let pp fmt {callsite_loc} = Location.pp_file_pos fmt callsite_loc
end

module Status = struct
  type kind = call_kind [@@deriving equal, compare]

  type t = {kind: kind; resolution: resolution} [@@deriving equal, compare]

  let pp fmt {kind; resolution} =
    let kind = match kind with Static -> "static" | Virtual -> "virtual" | Closure -> "closure" in
    let resolution =
      match resolution with
      | Unresolved ->
          "unresolved"
      | ResolvedUsingDynamicType ->
          "resolved using receiver dynamic type"
      | ResolvedUsingStaticType ->
          "resolved using receiver static type"
    in
    F.fprintf fmt "%s call was %s" kind resolution


  (* this is a total order *)
  let leq ~lhs ~rhs =
    if equal lhs rhs then true
    else
      match (lhs.resolution, rhs.resolution) with
      | _, ResolvedUsingDynamicType | Unresolved, _ ->
          false
      | _, _ ->
          true


  let join lhs rhs = if leq ~lhs ~rhs then rhs else lhs

  let widen ~prev ~next ~num_iters:_ = join prev next
end

include AbstractDomain.Map (CallSite) (Status)

let compare = compare Status.compare

let equal = equal Status.equal

let record ~caller_name ~caller_loc ~callsite_loc kind resolution history =
  let callsite = {CallSite.caller_name; caller_loc; callsite_loc} in
  add callsite {kind; resolution} history


type item =
  { callsite_loc: Location.t
  ; caller_name: string
  ; caller_loc: Location.t
  ; kind: call_kind
  ; resolution: resolution }

let report_as_extra_info history =
  fold
    (fun {callsite_loc; caller_name; caller_loc} ({kind; resolution} : Status.t) acc : item list ->
      {callsite_loc; caller_name; caller_loc; kind; resolution} :: acc )
    history []
