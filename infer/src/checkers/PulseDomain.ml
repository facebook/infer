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
module AbstractLocation : sig
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

module AbstractLocationDomain : AbstractDomain.S with type astate = AbstractLocation.t = struct
  type astate = AbstractLocation.t

  let ( <= ) ~lhs ~rhs = AbstractLocation.equal lhs rhs

  let join l1 l2 =
    if AbstractLocation.equal l1 l2 then l1 else (* TODO: scary *) AbstractLocation.mk_fresh ()


  let widen ~prev ~next ~num_iters:_ = join prev next

  let pp = AbstractLocation.pp
end

module MemoryKey = struct
  type t = AbstractLocation.t * AccessPath.access [@@deriving compare]

  let pp f k = Pp.pair ~fst:AbstractLocation.pp ~snd:AccessPath.pp_access f k
end

module MemoryDomain = AbstractDomain.Map (MemoryKey) (AbstractLocationDomain)
module AliasingDomain = AbstractDomain.Map (Var) (AbstractLocationDomain)
module AbstractLocationsDomain = AbstractDomain.FiniteSet (AbstractLocation)
module InvalidLocationsDomain = AbstractLocationsDomain

type t =
  {heap: MemoryDomain.astate; stack: AliasingDomain.astate; invalids: InvalidLocationsDomain.astate}

module Domain : AbstractDomain.S with type astate = t = struct
  type astate = t

  (* This is very naive and should be improved. We can compare two memory graphs by trying to
     establish that the graphs reachable from each root are the same up to some additional
     unfolding, i.e. are not incompatible when we prune everything that is not reachable from the
     roots. Here the roots are the known aliases in the stack since that's the only way to address
     into the heap. *)
  let ( <= ) ~lhs ~rhs =
    phys_equal lhs rhs
    || InvalidLocationsDomain.( <= ) ~lhs:lhs.invalids ~rhs:rhs.invalids
       && AliasingDomain.( <= ) ~lhs:lhs.stack ~rhs:rhs.stack
       && MemoryDomain.( <= ) ~lhs:lhs.heap ~rhs:rhs.heap


  (* Like (<=) this is probably too naive *)
  let join astate1 astate2 =
    if phys_equal astate1 astate2 then astate1
    else
      { heap= MemoryDomain.join astate1.heap astate2.heap
      ; stack= AliasingDomain.join astate1.stack astate2.stack
      ; invalids= InvalidLocationsDomain.join astate1.invalids astate2.invalids }


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
      ; invalids= InvalidLocationsDomain.widen ~num_iters ~prev:prev.invalids ~next:next.invalids
      }


  let pp fmt {heap; stack; invalids} =
    F.fprintf fmt "{@[<v1> heap=@[<hv>%a@];@;stack=@[<hv>%a@];@;invalids=@[<hv>%a@];@]}"
      MemoryDomain.pp heap AliasingDomain.pp stack InvalidLocationsDomain.pp invalids
end

include Domain

let initial =
  {heap= MemoryDomain.empty; stack= AliasingDomain.empty; invalids= AbstractLocationsDomain.empty}


module Diagnostic = struct
  (* TODO: more structured error type so that we can actually report something informative about
       the variables being accessed along with a trace *)
  type t = InvalidLocation

  let to_string InvalidLocation = "invalid location"
end

type access_result = (t, t * Diagnostic.t) result

(** Check that the location is not known to be invalid *)
let check_loc_access loc astate =
  if AbstractLocationsDomain.mem loc astate.invalids then Error (astate, Diagnostic.InvalidLocation)
  else Ok astate


(** Walk the heap starting from [loc] and following [path]. Stop either at the element before last
   and return [new_loc] if [overwrite_last] is [Some new_loc], or go until the end of the path if it
   is [None]. Create more locations into the heap as needed to follow the [path]. Check that each
   location reached is valid. *)
let rec walk ~overwrite_last loc path astate =
  match (path, overwrite_last) with
  | [], None ->
      Ok (astate, loc)
  | [], Some _ ->
      L.die InternalError "Cannot overwrite last location in empty path"
  | [a], Some new_loc ->
      check_loc_access loc astate
      >>| fun astate ->
      let heap = MemoryDomain.add (loc, a) new_loc astate.heap in
      ({astate with heap}, new_loc)
  | a :: path, _ -> (
      check_loc_access loc astate
      >>= fun astate ->
      match MemoryDomain.find_opt (loc, a) astate.heap with
      | None ->
          let loc' = AbstractLocation.mk_fresh () in
          let heap = MemoryDomain.add (loc, a) loc' astate.heap in
          let astate = {astate with heap} in
          walk ~overwrite_last loc' path astate
      | Some loc' ->
          walk ~overwrite_last loc' path astate )


(** add locations to the state to give a location to the destination of the given access path *)
let walk_access_expr ?overwrite_last astate access_expr =
  let (access_var, _), access_list = AccessExpression.to_access_path access_expr in
  match (overwrite_last, access_list) with
  | Some new_loc, [] ->
      let stack = AliasingDomain.add access_var new_loc astate.stack in
      Ok ({astate with stack}, new_loc)
  | None, _ | Some _, _ :: _ ->
      let astate, base_loc =
        match AliasingDomain.find_opt access_var astate.stack with
        | Some loc ->
            (astate, loc)
        | None ->
            let loc = AbstractLocation.mk_fresh () in
            let stack = AliasingDomain.add access_var loc astate.stack in
            ({astate with stack}, loc)
      in
      walk ~overwrite_last base_loc access_list astate


(** Use the stack and heap to walk the access path represented by the given expression down to an
    abstract location representing what the expression points to.

    Return an error state if it traverses some known invalid location or if the end destination is
    known to be invalid. *)
let materialize_location astate access_expr = walk_access_expr astate access_expr

(** Use the stack and heap to walk the access path represented by the given expression down to an
    abstract location representing what the expression points to, and replace that with the given
    location.

    Return an error state if it traverses some known invalid location. *)
let overwrite_location astate access_expr new_loc =
  walk_access_expr ~overwrite_last:new_loc astate access_expr


(** Add the given location to the set of know invalid locations. *)
let mark_invalid loc astate =
  {astate with invalids= AbstractLocationsDomain.add loc astate.invalids}


let read astate access_expr =
  materialize_location astate access_expr >>= fun (astate, loc) -> check_loc_access loc astate


let read_all access_exprs astate = List.fold_result access_exprs ~init:astate ~f:read

let write access_expr astate =
  overwrite_location astate access_expr (AbstractLocation.mk_fresh ())
  >>| fun (astate, _) -> astate


let invalidate access_expr astate =
  materialize_location astate access_expr
  >>= fun (astate, loc) -> check_loc_access loc astate >>| mark_invalid loc
