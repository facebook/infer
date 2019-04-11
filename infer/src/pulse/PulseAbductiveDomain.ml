(*
 * Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module AbstractAddress = PulseDomain.AbstractAddress
module Attributes = PulseDomain.Attributes
module BaseStack = PulseDomain.Stack
module BaseMemory = PulseDomain.Memory

(** signature common to the "normal" [Domain], representing the post at the current program point,
    and the inverted [InvertedDomain], representing the inferred pre-condition*)
module type BaseDomain = sig
  (* private because the lattice is not the same for preconditions and postconditions so we don't
     want to confuse them *)
  type t = private PulseDomain.t [@@deriving compare]

  val empty : t

  val make : BaseStack.t -> BaseMemory.t -> t

  val update : ?stack:BaseStack.t -> ?heap:BaseMemory.t -> t -> t

  include AbstractDomain.NoJoin with type t := t
end

(* just to expose the [heap] and [stack] record field names without having to type
   [PulseDomain.heap] *)
type base_domain = PulseDomain.t = {heap: BaseMemory.t; stack: BaseStack.t}

(** operations common to [Domain] and [InvertedDomain], see also the [BaseDomain] signature *)
module BaseDomainCommon = struct
  let make stack heap = {stack; heap}

  let update ?stack ?heap foot =
    let new_stack, new_heap =
      (Option.value ~default:foot.stack stack, Option.value ~default:foot.heap heap)
    in
    if phys_equal new_stack foot.stack && phys_equal new_heap foot.heap then foot
    else {stack= new_stack; heap= new_heap}
end

(** represents the post abstract state at each program point *)
module Domain : BaseDomain = struct
  include BaseDomainCommon
  include PulseDomain
end

(** represents the inferred pre-condition at each program point, biabduction style *)
module InvertedDomain : BaseDomain = struct
  include BaseDomainCommon

  type t = PulseDomain.t [@@deriving compare]

  let empty = PulseDomain.empty

  let pp = PulseDomain.pp

  (** inverted lattice *)
  let ( <= ) ~lhs ~rhs = PulseDomain.( <= ) ~rhs:lhs ~lhs:rhs
end

(** biabduction-style pre/post state *)
type t =
  { post: Domain.t  (** state at the current program point*)
  ; pre: InvertedDomain.t  (** inferred pre at the current program point *) }
[@@deriving compare]

let pp f {post; pre} = F.fprintf f "@[<v>%a@;[%a]@]" Domain.pp post InvertedDomain.pp pre

let ( <= ) ~lhs ~rhs =
  match
    PulseDomain.isograph_map PulseDomain.empty_mapping
      ~lhs:(rhs.pre :> PulseDomain.t)
      ~rhs:(lhs.pre :> PulseDomain.t)
  with
  | NotIsomorphic ->
      false
  | IsomorphicUpTo foot_mapping ->
      PulseDomain.is_isograph foot_mapping
        ~lhs:(lhs.post :> PulseDomain.t)
        ~rhs:(rhs.post :> PulseDomain.t)


module Stack = struct
  let is_abducible _var =
    (* TODO: need to keep only formals + return variable + globals in the pre *) true


  (** [astate] with [astate.post.stack = f astate.post.stack] *)
  let map_post_stack ~f astate =
    let new_post = Domain.update astate.post ~stack:(f (astate.post :> base_domain).stack) in
    if phys_equal new_post astate.post then astate else {astate with post= new_post}


  let materialize var astate =
    match BaseStack.find_opt var (astate.post :> base_domain).stack with
    | Some addr_loc_opt ->
        (astate, addr_loc_opt)
    | None ->
        let addr_loc_opt' = (AbstractAddress.mk_fresh (), None) in
        let post_stack = BaseStack.add var addr_loc_opt' (astate.post :> base_domain).stack in
        let pre =
          if is_abducible var then
            let foot_stack = BaseStack.add var addr_loc_opt' (astate.pre :> base_domain).stack in
            let foot_heap =
              BaseMemory.register_address (fst addr_loc_opt') (astate.pre :> base_domain).heap
            in
            InvertedDomain.make foot_stack foot_heap
          else astate.pre
        in
        ({post= Domain.update astate.post ~stack:post_stack; pre}, addr_loc_opt')


  let add var addr_loc_opt astate =
    map_post_stack astate ~f:(fun stack -> BaseStack.add var addr_loc_opt stack)


  let remove_vars vars astate =
    map_post_stack astate ~f:(fun stack ->
        BaseStack.filter (fun var _ -> not (List.mem ~equal:Var.equal vars var)) stack )


  let fold f astate accum = BaseStack.fold f (astate.post :> base_domain).stack accum

  let find_opt var astate = BaseStack.find_opt var (astate.post :> base_domain).stack
end

module Memory = struct
  open Result.Monad_infix
  module Access = BaseMemory.Access

  (** [astate] with [astate.post.heap = f astate.post.heap] *)
  let map_post_heap ~f astate =
    let new_post = Domain.update astate.post ~heap:(f (astate.post :> base_domain).heap) in
    if phys_equal new_post astate.post then astate else {astate with post= new_post}


  (** if [address] is in [pre] and it should be valid then that fact goes in the precondition *)
  let record_must_be_valid actor address (pre : InvertedDomain.t) =
    if BaseMemory.mem_edges address (pre :> base_domain).heap then
      InvertedDomain.update pre
        ~heap:
          (BaseMemory.add_attributes address
             (Attributes.singleton (MustBeValid actor))
             (pre :> base_domain).heap)
    else pre


  let check_valid actor addr ({post; pre} as astate) =
    BaseMemory.check_valid addr (post :> base_domain).heap
    >>| fun () ->
    let new_pre = record_must_be_valid actor addr pre in
    if phys_equal new_pre pre then astate else {astate with pre= new_pre}


  let add_edge addr access new_addr_trace astate =
    map_post_heap astate ~f:(fun heap -> BaseMemory.add_edge addr access new_addr_trace heap)


  let add_edge_and_back_edge addr access new_addr_trace astate =
    map_post_heap astate ~f:(fun heap ->
        BaseMemory.add_edge_and_back_edge addr access new_addr_trace heap )


  let materialize_edge addr access astate =
    match BaseMemory.find_edge_opt addr access (astate.post :> base_domain).heap with
    | Some addr_trace' ->
        (astate, addr_trace')
    | None ->
        let addr_trace' = (AbstractAddress.mk_fresh (), []) in
        let post_heap =
          BaseMemory.add_edge_and_back_edge addr access addr_trace'
            (astate.post :> base_domain).heap
        in
        let foot_heap =
          if BaseMemory.mem_edges addr (astate.pre :> base_domain).heap then
            BaseMemory.add_edge_and_back_edge addr access addr_trace'
              (astate.pre :> base_domain).heap
            |> BaseMemory.register_address (fst addr_trace')
          else (astate.pre :> base_domain).heap
        in
        ( { post= Domain.update astate.post ~heap:post_heap
          ; pre= InvertedDomain.update astate.pre ~heap:foot_heap }
        , addr_trace' )


  let invalidate address actor astate =
    map_post_heap astate ~f:(fun heap -> BaseMemory.invalidate address actor heap)


  let add_attributes address attributes astate =
    map_post_heap astate ~f:(fun heap -> BaseMemory.add_attributes address attributes heap)


  let std_vector_reserve addr astate =
    map_post_heap astate ~f:(fun heap -> BaseMemory.std_vector_reserve addr heap)


  let is_std_vector_reserved addr astate =
    BaseMemory.is_std_vector_reserved addr (astate.post :> base_domain).heap


  let find_opt address astate = BaseMemory.find_opt address (astate.post :> base_domain).heap

  let set_cell addr cell astate =
    map_post_heap astate ~f:(fun heap -> BaseMemory.set_cell addr cell heap)


  module Edges = BaseMemory.Edges
end

let empty = {post= Domain.empty; pre= InvertedDomain.empty}

let discard_unreachable ({pre; post} as astate) =
  let pre_addresses = PulseDomain.visit (pre :> PulseDomain.t) in
  let pre_old_heap = (pre :> PulseDomain.t).heap in
  let pre_new_heap =
    PulseDomain.Memory.filter
      (fun address -> PulseDomain.AbstractAddressSet.mem address pre_addresses)
      pre_old_heap
  in
  let post_addresses = PulseDomain.visit (post :> PulseDomain.t) in
  let all_addresses = PulseDomain.AbstractAddressSet.union pre_addresses post_addresses in
  let post_old_heap = (post :> PulseDomain.t).heap in
  let post_new_heap =
    PulseDomain.Memory.filter
      (fun address -> PulseDomain.AbstractAddressSet.mem address all_addresses)
      post_old_heap
  in
  if phys_equal pre_new_heap pre_old_heap && phys_equal post_new_heap post_old_heap then astate
  else
    { pre= InvertedDomain.make (pre :> PulseDomain.t).stack pre_new_heap
    ; post= Domain.make (post :> PulseDomain.t).stack post_new_heap }
