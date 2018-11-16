(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging
module Invalidation = PulseInvalidation

(* {2 Abstract domain description } *)

(** An abstract address in memory. *)
module AbstractAddress : sig
  type t = private int [@@deriving compare]

  val nullptr : t

  val equal : t -> t -> bool

  val mk_fresh : unit -> t

  val pp : F.formatter -> t -> unit

  val init : unit -> unit
end = struct
  type t = int [@@deriving compare]

  let equal = [%compare.equal: t]

  (** distinguish 0 since it's always an invalid address *)
  let nullptr = 0

  let next_fresh = ref 1

  let mk_fresh () =
    let l = !next_fresh in
    incr next_fresh ; l


  let pp = F.pp_print_int

  let init () = next_fresh := 1
end

(* {3 Heap domain } *)

module Attribute = struct
  (* OPTIM: [Invalid _] is first in the order to make queries about invalidation status more
       efficient (only need to look at the first element in the set of attributes to know if an
       address is invalid) *)
  type t = Invalid of Invalidation.t | StdVectorReserve [@@deriving compare]

  let pp f = function
    | Invalid invalidation ->
        Invalidation.pp f invalidation
    | StdVectorReserve ->
        F.pp_print_string f "std::vector::reserve()"
end

module Attributes = AbstractDomain.FiniteSet (Attribute)

module Memory : sig
  module Edges : module type of PrettyPrintable.MakePPMap (AccessExpression.Access)

  type edges = AbstractAddress.t Edges.t

  type cell = edges * Attributes.t

  type t

  val empty : t

  val find_opt : AbstractAddress.t -> t -> cell option

  val for_all : (AbstractAddress.t -> cell -> bool) -> t -> bool

  val fold : (AbstractAddress.t -> cell -> 'accum -> 'accum) -> t -> 'accum -> 'accum

  val pp : F.formatter -> t -> unit

  val add_edge : AbstractAddress.t -> AccessExpression.Access.t -> AbstractAddress.t -> t -> t

  val add_edge_and_back_edge :
    AbstractAddress.t -> AccessExpression.Access.t -> AbstractAddress.t -> t -> t

  val find_edge_opt :
    AbstractAddress.t -> AccessExpression.Access.t -> t -> AbstractAddress.t option

  val add_attributes : AbstractAddress.t -> Attributes.t -> t -> t

  val invalidate : AbstractAddress.t -> Invalidation.t -> t -> t

  val get_invalidation : AbstractAddress.t -> t -> Invalidation.t option
  (** None denotes a valid location *)

  val std_vector_reserve : AbstractAddress.t -> t -> t

  val is_std_vector_reserved : AbstractAddress.t -> t -> bool
end = struct
  module Edges = PrettyPrintable.MakePPMap (AccessExpression.Access)

  type edges = AbstractAddress.t Edges.t

  type cell = edges * Attributes.t

  module Graph = PrettyPrintable.MakePPMap (AbstractAddress)

  type t = cell Graph.t

  let pp =
    Graph.pp ~pp_value:(Pp.pair ~fst:(Edges.pp ~pp_value:AbstractAddress.pp) ~snd:Attributes.pp)


  (* {3 Helper functions to traverse the two maps at once } *)

  let add_edge addr_src access addr_end memory =
    let edges, attrs =
      match Graph.find_opt addr_src memory with
      | Some edges_attrs ->
          edges_attrs
      | None ->
          (Edges.empty, Attributes.empty)
    in
    Graph.add addr_src (Edges.add access addr_end edges, attrs) memory


  (** [Dereference] edges induce a [TakeAddress] back edge and vice-versa, because
      [*(&x) = &( *x ) = x]. *)
  let add_edge_and_back_edge addr_src (access : AccessExpression.Access.t) addr_end memory =
    let memory = add_edge addr_src access addr_end memory in
    match access with
    | ArrayAccess _ | FieldAccess _ ->
        memory
    | TakeAddress ->
        add_edge addr_end Dereference addr_src memory
    | Dereference ->
        add_edge addr_end TakeAddress addr_src memory


  let find_edge_opt addr access memory =
    let open Option.Monad_infix in
    Graph.find_opt addr memory >>| fst >>= Edges.find_opt access


  let add_attributes addr attrs memory =
    let edges, old_attrs =
      match Graph.find_opt addr memory with
      | Some edges_old_attrs ->
          edges_old_attrs
      | None ->
          (Edges.empty, Attributes.empty)
    in
    if phys_equal old_attrs attrs then memory
    else Graph.add addr (edges, Attributes.union attrs old_attrs) memory


  let add_attribute address attribute memory =
    Graph.update address
      (function
        | Some (edges, old_attributes) ->
            Some (edges, Attributes.add attribute old_attributes)
        | None ->
            Some (Edges.empty, Attributes.singleton attribute))
      memory


  let invalidate address invalidation memory =
    add_attribute address (Attribute.Invalid invalidation) memory


  let get_invalidation address memory =
    (* Since we often want to find out whether an address is invalid this case is optimised. Since
       [Invalid _] attributes are the smallest we can simply look at the first element to decide if
       an address is invalid or not. *)
    Graph.find_opt address memory |> Option.map ~f:snd
    |> Option.bind ~f:Attributes.min_elt_opt
    |> Option.bind ~f:(function Attribute.Invalid invalidation -> Some invalidation | _ -> None)


  let std_vector_reserve address memory = add_attribute address Attribute.StdVectorReserve memory

  let is_std_vector_reserved address memory =
    Graph.find_opt address memory |> Option.map ~f:snd
    |> Option.value_map ~default:false ~f:(fun attributes ->
           Attributes.mem Attribute.StdVectorReserve attributes )


  (* {3 Monomorphic {!PPMap} interface as needed } *)

  let empty = Graph.empty

  let find_opt = Graph.find_opt

  let for_all = Graph.for_all

  let fold = Graph.fold
end

(** Stacks: map variables to values.

    This is defined as an abstract domain but the domain operations are mostly meaningless on their
    own. It so happens that the join on abstract states uses the join of stacks provided by this
    functor followed by normalization wrt the unification found between abstract locations so it's
    convenient to define stacks as elements of this domain. *)
module Stack =
  AbstractDomain.Map
    (Var)
    (struct
      type astate = AbstractAddress.t

      let ( <= ) ~lhs ~rhs = AbstractAddress.equal lhs rhs

      let join l1 l2 = min l1 l2

      let widen ~prev ~next ~num_iters:_ = join prev next

      let pp = AbstractAddress.pp
    end)

(** the domain *)
type t = {heap: Memory.t; stack: Stack.astate}

let initial =
  { heap=
      Memory.empty
      (* TODO: we could record that 0 is an invalid address at this point but this makes the
         analysis go a bit overboard with the Nullptr reports. *)
  ; stack= Stack.empty }


module Domain : AbstractDomain.S with type astate = t = struct
  type astate = t

  let piecewise_lessthan lhs rhs =
    Stack.( <= ) ~lhs:lhs.stack ~rhs:rhs.stack
    && Memory.for_all
         (fun addr_src (edges_lhs, attrs_lhs) ->
           let edges_rhs_opt, attrs_rhs =
             let cell = Memory.find_opt addr_src rhs.heap in
             (Option.map ~f:fst cell, Option.value_map ~default:Attributes.empty ~f:snd cell)
           in
           Memory.Edges.for_all
             (fun access_lhs addr_dst ->
               Option.bind edges_rhs_opt ~f:(fun edges_rhs ->
                   Memory.Edges.find_opt access_lhs edges_rhs )
               |> Option.map ~f:(AbstractAddress.equal addr_dst)
               |> Option.value ~default:false )
             edges_lhs
           && Attributes.( <= ) ~lhs:attrs_lhs ~rhs:attrs_rhs )
         lhs.heap


  module JoinState = struct
    module AddressUnionSet = struct
      module Set = PrettyPrintable.MakePPSet (AbstractAddress)

      type elt = AbstractAddress.t [@@deriving compare]

      type t = Set.t ref

      let create x = ref (Set.singleton x)

      let compare_size _ _ = 0

      let merge ~from ~to_ = to_ := Set.union !from !to_

      let pp f x = Set.pp f !x
    end

    module AddressUF = ImperativeUnionFind.Make (AddressUnionSet)

    (** just to get the correct type coercion *)
    let to_canonical_address subst addr = (AddressUF.find subst addr :> AbstractAddress.t)

    type nonrec t = {subst: AddressUF.t; astate: t}

    (** adds [(src_addr, access, dst_addr)] to [union_heap] and record potential new equality that
       results from it in [subst] *)
    let union_one_edge subst src_addr access dst_addr union_heap =
      let src_addr = to_canonical_address subst src_addr in
      let dst_addr = to_canonical_address subst dst_addr in
      match
        (Memory.find_edge_opt src_addr access union_heap, (access : AccessExpression.Access.t))
      with
      | Some dst_addr', _ when AbstractAddress.equal dst_addr dst_addr' ->
          (* same edge *)
          (union_heap, `No_new_equality)
      | _, ArrayAccess _ ->
          (* do not trust array accesses for now, replace the destination of the edge by a fresh location *)
          ( Memory.add_edge src_addr access (AbstractAddress.mk_fresh ()) union_heap
          , `No_new_equality )
      | None, _ ->
          (Memory.add_edge src_addr access dst_addr union_heap, `No_new_equality)
      | Some dst_addr', _ ->
          (* new equality [dst_addr = dst_addr'] found *)
          ignore (AddressUF.union subst dst_addr dst_addr') ;
          (union_heap, `New_equality)


    module Addresses = Caml.Set.Make (AbstractAddress)

    let rec visit_address subst visited heap addr union_heap =
      if Addresses.mem addr visited then (visited, union_heap)
      else
        let visited = Addresses.add addr visited in
        let visit_edge access addr_dst (visited, union_heap) =
          union_one_edge subst addr access addr_dst union_heap
          |> fst
          |> visit_address subst visited heap addr_dst
        in
        Memory.find_opt addr heap
        |> Option.fold ~init:(visited, union_heap) ~f:(fun (visited, union_heap) (edges, attrs) ->
               let union_heap = Memory.add_attributes addr attrs union_heap in
               Memory.Edges.fold visit_edge edges (visited, union_heap) )


    let visit_stack subst heap stack union_heap =
      (* start graph exploration *)
      let visited = Addresses.empty in
      let _, union_heap =
        Stack.fold
          (fun _var addr (visited, union_heap) -> visit_address subst visited heap addr union_heap)
          stack (visited, union_heap)
      in
      union_heap


    let populate_subst_from_stacks subst stack1 stack2 =
      ignore
        ((* Use [Caml.Map.merge] to detect the variables present in both stacks. Build an empty
            result map since we don't use the result. *)
         Stack.merge
           (fun _var addr1_opt addr2_opt ->
             Option.both addr1_opt addr2_opt
             |> Option.iter ~f:(fun (addr1, addr2) ->
                    (* stack1 says [_var = addr1] and stack2 says [_var = addr2]: unify the
                       addresses since they are equal to the same variable *)
                    ignore (AddressUF.union subst addr1 addr2) ) ;
             (* empty result map *)
             None )
           stack1 stack2)


    let from_astate_union {heap= heap1; stack= stack1} {heap= heap2; stack= stack2} =
      let subst = AddressUF.create () in
      (* gather equalities from the stacks *)
      populate_subst_from_stacks subst stack1 stack2 ;
      (* union the heaps, take this opportunity to do garbage collection of unreachable values by
         only copying the addresses reachable from the variables in the stacks *)
      let heap = visit_stack subst heap1 stack1 Memory.empty |> visit_stack subst heap2 stack2 in
      (* This keeps all the variables and picks one representative address for each variable in
         common thanks to [AbstractAddressDomain_JoinIsMin] *)
      let stack = Stack.join stack1 stack2 in
      {subst; astate= {heap; stack}}


    let rec normalize state =
      let one_addr subst addr (edges, attrs) (heap, has_converged) =
        let heap = Memory.add_attributes addr attrs heap in
        Memory.Edges.fold
          (fun access addr_dest (heap, has_converged) ->
            match union_one_edge subst addr access addr_dest heap with
            | heap, `No_new_equality ->
                (heap, has_converged)
            | heap, `New_equality ->
                (heap, false) )
          edges (heap, has_converged)
      in
      let heap, has_converged =
        Memory.fold (one_addr state.subst) state.astate.heap (Memory.empty, true)
      in
      if has_converged then (
        L.d_strln "Join unified addresses:" ;
        L.d_increase_indent () ;
        Container.iter state.subst ~fold:AddressUF.fold_sets
          ~f:(fun ((repr : AddressUF.Repr.t), set) ->
            L.d_printfln "%a=%a" AbstractAddress.pp
              (repr :> AbstractAddress.t)
              AddressUnionSet.pp set ) ;
        L.d_decrease_indent () ;
        let stack = Stack.map (to_canonical_address state.subst) state.astate.stack in
        {heap; stack} )
      else normalize {state with astate= {state.astate with heap}}
  end

  (** Given

      - stacks S1, S2 : Var -> Address,

      - graphs G1, G2 : Address -> Access -> Address,

      - and invalid sets I1, I2 : 2^Address

      (all finite), the join of 2 abstract states (S1, G1, I1) and (S2, G2, I2) is (S, G, A) where
      there exists a substitution σ from addresses to addresses such that the following holds. Given
      addresses l, l', access path a, and graph G, we write l –a–> l' ∈ G if there is a path labelled
      by a from l to l' in G (in particular, if a is empty then l –a–> l' ∈ G for all l, l').

      {v
      ∀ i ∈ {1,2}, ∀ l, x, a, ∀ l' ∈ Ii, ((x, l) ∈ Si ∧ l –a–> l' ∈ Gi)
                                         => (x, σ(l)) ∈ S ∧ σ(l) –a–> σ(l') ∈ G ∧ σ(l') ∈ I
      v}

      For now the implementation gives back a larger heap than necessary, where all the previously
      reachable location are still reachable (up to the substitution) instead of only the locations
      leading to invalid ones.
  *)
  let join astate1 astate2 =
    if phys_equal astate1 astate2 then astate1
    else
      (* high-level idea: maintain some union-find data structure to identify locations in one heap
         with locations in the other heap. Build the initial join state as follows:

         - equate all locations that correspond to identical variables in both stacks, eg joining
         stacks {x=1} and {x=2} adds "1=2" to the unification.

         - add all addresses reachable from stack variables to the join state heap

         This gives us an abstract state that is the union of both abstract states, but more states
         can still be made equal. For instance, if 1 points to 3 in the first heap and 2 points to 4
         in the second heap and we deduced "1 = 2" from the stacks already (as in the example just
         above) then we can deduce "3 = 4". Proceed in this fashion until no more equalities are
         discovered, and return the abstract state where a canonical representative has been chosen
         consistently for each equivalence class (this is what the union-find data structure gives
         us). *)
      JoinState.from_astate_union astate1 astate2 |> JoinState.normalize


  (* TODO: this could be [piecewise_lessthan lhs' (join lhs rhs)] where [lhs'] is [lhs] renamed
     according to the unification discovered while joining [lhs] and [rhs]. *)
  let ( <= ) ~lhs ~rhs = phys_equal lhs rhs || piecewise_lessthan lhs rhs

  let max_widening = 5

  let widen ~prev ~next ~num_iters =
    (* widening is underapproximation for now... TODO *)
    if num_iters > max_widening then prev
    else if phys_equal prev next then prev
    else join prev next


  let pp fmt {heap; stack} =
    F.fprintf fmt "{@[<v1> heap=@[<hv>%a@];@;stack=@[<hv>%a@];@]}" Memory.pp heap Stack.pp stack
end

(* {2 Access operations on the domain} *)

type actor = {access_expr: AccessExpression.t; location: Location.t} [@@deriving compare]

module Diagnostic = struct
  type t =
    | AccessToInvalidAddress of
        { invalidated_by: Invalidation.t
        ; accessed_by: actor
        ; address: AbstractAddress.t }

  let get_location (AccessToInvalidAddress {accessed_by= {location}}) = location

  let get_message (AccessToInvalidAddress {accessed_by; invalidated_by; address}) =
    let pp_debug_address f =
      if Config.debug_mode then F.fprintf f " (debug: %a)" AbstractAddress.pp address
    in
    F.asprintf "`%a` accesses address %a past its lifetime%t" AccessExpression.pp
      accessed_by.access_expr Invalidation.pp invalidated_by pp_debug_address


  let get_trace (AccessToInvalidAddress {accessed_by; invalidated_by}) =
    let invalidated_by_trace =
      Invalidation.get_location invalidated_by
      |> Option.map ~f:(fun location ->
             Errlog.make_trace_element 0 location
               (F.asprintf "%a here" Invalidation.pp invalidated_by)
               [] )
      |> Option.to_list
    in
    invalidated_by_trace
    @ [ Errlog.make_trace_element 0 accessed_by.location
          (F.asprintf "accessed `%a` here" AccessExpression.pp accessed_by.access_expr)
          [] ]


  let get_issue_type (AccessToInvalidAddress {invalidated_by}) =
    Invalidation.issue_type_of_cause invalidated_by
end

type 'a access_result = ('a, Diagnostic.t) result

(** operations on the domain *)
module Operations = struct
  open Result.Monad_infix

  (** Check that the address is not known to be invalid *)
  let check_addr_access actor address astate =
    match Memory.get_invalidation address astate.heap with
    | Some invalidated_by ->
        Error (Diagnostic.AccessToInvalidAddress {invalidated_by; accessed_by= actor; address})
    | None ->
        Ok astate


  (** Walk the heap starting from [addr] and following [path]. Stop either at the element before last
   and return [new_addr] if [overwrite_last] is [Some new_addr], or go until the end of the path if it
   is [None]. Create more addresses into the heap as needed to follow the [path]. Check that each
   address reached is valid. *)
  let rec walk actor ~on_last addr path astate =
    match (path, on_last) with
    | [], `Access ->
        Ok (astate, addr)
    | [], `Overwrite _ ->
        L.die InternalError "Cannot overwrite last address in empty path"
    | [a], `Overwrite new_addr ->
        check_addr_access actor addr astate
        >>| fun astate ->
        let heap = Memory.add_edge_and_back_edge addr a new_addr astate.heap in
        ({astate with heap}, new_addr)
    | a :: path, _ -> (
        check_addr_access actor addr astate
        >>= fun astate ->
        match Memory.find_edge_opt addr a astate.heap with
        | None ->
            let addr' = AbstractAddress.mk_fresh () in
            let heap = Memory.add_edge_and_back_edge addr a addr' astate.heap in
            let astate = {astate with heap} in
            walk actor ~on_last addr' path astate
        | Some addr' ->
            walk actor ~on_last addr' path astate )


  let write_var var addr astate =
    let stack = Stack.add var addr astate.stack in
    {astate with stack}


  (** add addresses to the state to give a address to the destination of the given access path *)
  let walk_access_expr ~on_last astate access_expr location =
    let (access_var, _), access_list = AccessExpression.to_accesses access_expr in
    if Config.write_html then
      L.d_printfln "Accessing %a -> [%a]" Var.pp access_var
        (Pp.seq ~sep:"," AccessExpression.Access.pp)
        access_list ;
    match (on_last, access_list) with
    | `Overwrite new_addr, [] ->
        Ok (write_var access_var new_addr astate, new_addr)
    | `Access, _ | `Overwrite _, _ :: _ ->
        let astate, base_addr =
          match Stack.find_opt access_var astate.stack with
          | Some addr ->
              (astate, addr)
          | None ->
              let addr = AbstractAddress.mk_fresh () in
              let stack = Stack.add access_var addr astate.stack in
              ({astate with stack}, addr)
        in
        let actor = {access_expr; location} in
        walk actor ~on_last base_addr access_list astate


  (** Use the stack and heap to walk the access path represented by the given expression down to an
    abstract address representing what the expression points to.

    Return an error state if it traverses some known invalid address or if the end destination is
    known to be invalid. *)
  let materialize_address astate access_expr = walk_access_expr ~on_last:`Access astate access_expr

  (** Use the stack and heap to walk the access path represented by the given expression down to an
    abstract address representing what the expression points to, and replace that with the given
    address.

    Return an error state if it traverses some known invalid address. *)
  let overwrite_address astate access_expr new_addr =
    walk_access_expr ~on_last:(`Overwrite new_addr) astate access_expr


  (** Add the given address to the set of know invalid addresses. *)
  let mark_invalid actor address astate =
    {astate with heap= Memory.invalidate address actor astate.heap}


  let havoc_var var astate =
    {astate with stack= Stack.add var (AbstractAddress.mk_fresh ()) astate.stack}


  let havoc location (access_expr : AccessExpression.t) astate =
    match access_expr with
    | Base (access_var, _) ->
        havoc_var access_var astate |> Result.return
    | _ ->
        walk_access_expr
          ~on_last:(`Overwrite (AbstractAddress.mk_fresh ()))
          astate access_expr location
        >>| fst


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


  let invalidate cause location access_expr astate =
    materialize_address astate access_expr location
    >>= fun (astate, addr) ->
    check_addr_access {access_expr; location} addr astate >>| mark_invalid cause addr
end

module StdVector = struct
  open Result.Monad_infix

  let is_reserved location vector_access_expr astate =
    Operations.read location vector_access_expr astate
    >>| fun (astate, addr) -> (astate, Memory.is_std_vector_reserved addr astate.heap)


  let mark_reserved location vector_access_expr astate =
    Operations.read location vector_access_expr astate
    >>| fun (astate, addr) -> {astate with heap= Memory.std_vector_reserve addr astate.heap}
end

include Domain
include Operations
