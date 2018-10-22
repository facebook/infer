(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging
open Result.Monad_infix

(** An abstract address in memory. *)
module AbstractAddress : sig
  type t = private int [@@deriving compare]

  val equal : t -> t -> bool

  val mk_fresh : unit -> t

  val pp : F.formatter -> t -> unit
end = struct
  type t = int [@@deriving compare]

  let equal = [%compare.equal: t]

  let next_fresh = ref 0

  let mk_fresh () =
    let l = !next_fresh in
    incr next_fresh ; l


  let pp = F.pp_print_int
end

module AbstractAddressDomain : AbstractDomain.S with type astate = AbstractAddress.t = struct
  type astate = AbstractAddress.t

  let ( <= ) ~lhs ~rhs = AbstractAddress.equal lhs rhs

  let join l1 l2 =
    if AbstractAddress.equal l1 l2 then l1 else (* TODO: scary *) AbstractAddress.mk_fresh ()


  let widen ~prev ~next ~num_iters:_ = join prev next

  let pp = AbstractAddress.pp
end

module Access = struct
  type t = AccessPath.access [@@deriving compare]

  let pp = AccessPath.pp_access
end

module MemoryEdges = AbstractDomain.InvertedMap (Access) (AbstractAddressDomain)

module MemoryDomain = struct
  include AbstractDomain.InvertedMap (AbstractAddress) (MemoryEdges)

  let add_edge addr_src access addr_end memory =
    let edges =
      match find_opt addr_src memory with Some edges -> edges | None -> MemoryEdges.empty
    in
    add addr_src (MemoryEdges.add access addr_end edges) memory


  let find_edge_opt addr access memory =
    let open Option.Monad_infix in
    find_opt addr memory >>= MemoryEdges.find_opt access
end

module AliasingDomain = AbstractDomain.InvertedMap (Var) (AbstractAddressDomain)

type actor = {access_expr: AccessExpression.t; location: Location.t}

let pp_actor f {access_expr; location} =
  F.fprintf f "%a@%a" AccessExpression.pp access_expr Location.pp location


module type InvalidAddressesDomain = sig
  include AbstractDomain.S

  val empty : astate

  val add : AbstractAddress.t -> actor -> astate -> astate

  val get_invalidation : AbstractAddress.t -> astate -> actor option
end

module InvalidAddressesDomain : InvalidAddressesDomain = struct
  module InvalidationReason = struct
    type astate = actor

    let join actor _ = actor

    let ( <= ) ~lhs:_ ~rhs:_ = true

    let widen ~prev ~next:_ ~num_iters:_ = prev

    let pp = pp_actor
  end

  include AbstractDomain.Map (AbstractAddress) (InvalidationReason)

  let get_invalidation address invalids = find_opt address invalids
end

type t =
  {heap: MemoryDomain.astate; stack: AliasingDomain.astate; invalids: InvalidAddressesDomain.astate}

let initial =
  {heap= MemoryDomain.empty; stack= AliasingDomain.empty; invalids= InvalidAddressesDomain.empty}


module Domain : AbstractDomain.S with type astate = t = struct
  type astate = t

  (* This is very naive and should be improved. We can compare two memory graphs by trying to
     establish that the graphs reachable from each root are the same up to some additional
     unfolding, i.e. are not incompatible when we prune everything that is not reachable from the
     roots. Here the roots are the known aliases in the stack since that's the only way to address
     into the heap. *)
  let ( <= ) ~lhs ~rhs =
    phys_equal lhs rhs
    || InvalidAddressesDomain.( <= ) ~lhs:lhs.invalids ~rhs:rhs.invalids
       && AliasingDomain.( <= ) ~lhs:lhs.stack ~rhs:rhs.stack
       && MemoryDomain.( <= ) ~lhs:lhs.heap ~rhs:rhs.heap


  (* Like (<=) this is probably too naive *)
  let join astate1 astate2 =
    if phys_equal astate1 astate2 then astate1
    else
      { heap= MemoryDomain.join astate1.heap astate2.heap
      ; stack= AliasingDomain.join astate1.stack astate2.stack
      ; invalids= InvalidAddressesDomain.join astate1.invalids astate2.invalids }


  let max_widening = 5

  let widen ~prev ~next ~num_iters =
    (* probably pretty obvious but that widening is just bad... We need to add a wildcard [*] to
       access path elements in our graph representing repeated paths if we hope to converge (like
       {!AccessPath.Abs.Abstracted}, we actually need something very similar). *)
    if num_iters > max_widening then prev
    else if phys_equal prev next then prev
    else
      { heap= MemoryDomain.widen ~num_iters ~prev:prev.heap ~next:next.heap
      ; stack= AliasingDomain.widen ~num_iters ~prev:prev.stack ~next:next.stack
      ; invalids= InvalidAddressesDomain.widen ~num_iters ~prev:prev.invalids ~next:next.invalids
      }


  let pp fmt {heap; stack; invalids} =
    F.fprintf fmt "{@[<v1> heap=@[<hv>%a@];@;stack=@[<hv>%a@];@;invalids=@[<hv>%a@];@]}"
      MemoryDomain.pp heap AliasingDomain.pp stack InvalidAddressesDomain.pp invalids
end

include Domain

module Diagnostic = struct
  type t =
    | AccessToInvalidAddress of
        { invalidated_at: actor
        ; accessed_by: actor
        ; address: AbstractAddress.t }

  let get_location (AccessToInvalidAddress {accessed_by= {location}}) = location

  let get_message (AccessToInvalidAddress {accessed_by; invalidated_at; address}) =
    let pp_debug_address f =
      if Config.debug_mode then F.fprintf f " (debug: %a)" AbstractAddress.pp address
    in
    F.asprintf "`%a` accesses address `%a` past its lifetime%t" AccessExpression.pp
      accessed_by.access_expr AccessExpression.pp invalidated_at.access_expr pp_debug_address


  let get_trace (AccessToInvalidAddress {accessed_by; invalidated_at}) =
    [ Errlog.make_trace_element 0 invalidated_at.location
        (F.asprintf "invalidated `%a` here" AccessExpression.pp invalidated_at.access_expr)
        []
    ; Errlog.make_trace_element 0 accessed_by.location
        (F.asprintf "accessed `%a` here" AccessExpression.pp accessed_by.access_expr)
        [] ]


  let get_issue_type (AccessToInvalidAddress _) = IssueType.use_after_lifetime
end

type 'a access_result = ('a, Diagnostic.t) result

(** Check that the address is not known to be invalid *)
let check_addr_access actor address astate =
  match InvalidAddressesDomain.get_invalidation address astate.invalids with
  | Some invalidated_at ->
      Error (Diagnostic.AccessToInvalidAddress {invalidated_at; accessed_by= actor; address})
  | None ->
      Ok astate


(** Walk the heap starting from [addr] and following [path]. Stop either at the element before last
   and return [new_addr] if [overwrite_last] is [Some new_addr], or go until the end of the path if it
   is [None]. Create more addresses into the heap as needed to follow the [path]. Check that each
   address reached is valid. *)
let rec walk actor ~overwrite_last addr path astate =
  match (path, overwrite_last) with
  | [], None ->
      Ok (astate, addr)
  | [], Some _ ->
      L.die InternalError "Cannot overwrite last address in empty path"
  | [a], Some new_addr ->
      check_addr_access actor addr astate
      >>| fun astate ->
      let heap = MemoryDomain.add_edge addr a new_addr astate.heap in
      ({astate with heap}, new_addr)
  | a :: path, _ -> (
      check_addr_access actor addr astate
      >>= fun astate ->
      match MemoryDomain.find_edge_opt addr a astate.heap with
      | None ->
          let addr' = AbstractAddress.mk_fresh () in
          let heap = MemoryDomain.add_edge addr a addr' astate.heap in
          let astate = {astate with heap} in
          walk actor ~overwrite_last addr' path astate
      | Some addr' ->
          walk actor ~overwrite_last addr' path astate )


(** add addresses to the state to give a address to the destination of the given access path *)
let walk_access_expr ?overwrite_last astate access_expr location =
  let (access_var, _), access_list = AccessExpression.to_access_path access_expr in
  match (overwrite_last, access_list) with
  | Some new_addr, [] ->
      let stack = AliasingDomain.add access_var new_addr astate.stack in
      Ok ({astate with stack}, new_addr)
  | None, _ | Some _, _ :: _ ->
      let astate, base_addr =
        match AliasingDomain.find_opt access_var astate.stack with
        | Some addr ->
            (astate, addr)
        | None ->
            let addr = AbstractAddress.mk_fresh () in
            let stack = AliasingDomain.add access_var addr astate.stack in
            ({astate with stack}, addr)
      in
      let actor = {access_expr; location} in
      walk actor ~overwrite_last base_addr access_list astate


(** Use the stack and heap to walk the access path represented by the given expression down to an
    abstract address representing what the expression points to.

    Return an error state if it traverses some known invalid address or if the end destination is
    known to be invalid. *)
let materialize_address astate access_expr = walk_access_expr astate access_expr

(** Use the stack and heap to walk the access path represented by the given expression down to an
    abstract address representing what the expression points to, and replace that with the given
    address.

    Return an error state if it traverses some known invalid address. *)
let overwrite_address astate access_expr new_addr =
  walk_access_expr ~overwrite_last:new_addr astate access_expr


(** Add the given address to the set of know invalid addresses. *)
let mark_invalid actor address astate =
  {astate with invalids= InvalidAddressesDomain.add address actor astate.invalids}


let read location access_expr astate =
  materialize_address astate access_expr location
  >>= fun (astate, addr) ->
  let actor = {access_expr; location} in
  check_addr_access actor addr astate >>| fun astate -> (astate, addr)


let read_all location access_exprs astate =
  List.fold_result access_exprs ~init:astate ~f:(fun astate access_expr ->
      read location access_expr astate >>| fst )


let write location access_expr addr astate =
  overwrite_address astate access_expr addr location >>| fun (astate, _) -> astate


let havoc var astate = {astate with stack= AliasingDomain.remove var astate.stack}

let invalidate location access_expr astate =
  materialize_address astate access_expr location
  >>= fun (astate, addr) ->
  let actor = {access_expr; location} in
  check_addr_access actor addr astate >>| mark_invalid actor addr
