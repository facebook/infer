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
end

(* {3 Heap domain } *)

module Memory : sig
  module Edges : module type of PrettyPrintable.MakePPMap (AccessExpression.Access)

  type edges = AbstractAddress.t Edges.t

  type t

  val empty : t

  val find_opt : AbstractAddress.t -> t -> edges option

  val for_all : (AbstractAddress.t -> edges -> bool) -> t -> bool

  val fold : (AbstractAddress.t -> edges -> 'accum -> 'accum) -> t -> 'accum -> 'accum

  val pp : F.formatter -> t -> unit

  val add_edge : AbstractAddress.t -> AccessExpression.Access.t -> AbstractAddress.t -> t -> t

  val add_edge_and_back_edge :
    AbstractAddress.t -> AccessExpression.Access.t -> AbstractAddress.t -> t -> t

  val find_edge_opt :
    AbstractAddress.t -> AccessExpression.Access.t -> t -> AbstractAddress.t option
end = struct
  module Edges = PrettyPrintable.MakePPMap (AccessExpression.Access)

  type edges = AbstractAddress.t Edges.t

  module Graph = PrettyPrintable.MakePPMap (AbstractAddress)

  (* {3 Monomorphic {!PPMap} interface as needed } *)

  type t = AbstractAddress.t Edges.t Graph.t

  let empty = Graph.empty

  let find_opt = Graph.find_opt

  let for_all = Graph.for_all

  let fold = Graph.fold

  let pp = Graph.pp ~pp_value:(Edges.pp ~pp_value:AbstractAddress.pp)

  (* {3 Helper functions to traverse the two maps at once } *)

  let add_edge addr_src access addr_end memory =
    let edges =
      match Graph.find_opt addr_src memory with Some edges -> edges | None -> Edges.empty
    in
    Graph.add addr_src (Edges.add access addr_end edges) memory


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
    Graph.find_opt addr memory >>= Edges.find_opt access
end

(* {3 Stack domain } *)

(** to be used as maps values *)
module AbstractAddressDomain_JoinIsMin : AbstractDomain.S with type astate = AbstractAddress.t =
struct
  type astate = AbstractAddress.t

  let ( <= ) ~lhs ~rhs = AbstractAddress.equal lhs rhs

  let join l1 l2 = min l1 l2

  let widen ~prev ~next ~num_iters:_ = join prev next

  let pp = AbstractAddress.pp
end

(* It so happens that the join we want on stacks is this followed by normalization wrt the
   unification found between abstract locations, so it's convenient to define stacks as elements of
   this domain. Do not use the domain operations outside of {!Domain} though as they are mostly
   meaningless on their own. *)
module AliasingDomain = AbstractDomain.Map (Var) (AbstractAddressDomain_JoinIsMin)

(* {3 Invalid addresses domain } *)

(** Locations known to be invalid for some reason *)
module InvalidAddressesDomain : sig
  include AbstractDomain.S

  val empty : astate

  val add : AbstractAddress.t -> Invalidation.t -> astate -> astate

  val get_invalidation : AbstractAddress.t -> astate -> Invalidation.t option
  (** None denotes a valid location *)

  val map : (AbstractAddress.t -> AbstractAddress.t) -> astate -> astate
  (** translate invalid addresses according to the mapping *)
end = struct
  include AbstractDomain.Map (AbstractAddress) (Invalidation)

  let map f invalids = fold (fun key value map -> add (f key) value map) invalids empty

  let get_invalidation address invalids = find_opt address invalids
end

(** the domain *)
type t = {heap: Memory.t; stack: AliasingDomain.astate; invalids: InvalidAddressesDomain.astate}

let initial =
  { heap= Memory.empty
  ; stack= AliasingDomain.empty
  ; invalids=
      InvalidAddressesDomain.empty
      (* TODO: this makes the analysis go a bit overboard with the Nullptr reports. *)
      (* (\* always recall that 0 is invalid *\)
         InvalidAddressesDomain.add AbstractAddress.nullptr Nullptr InvalidAddressesDomain.empty *)
  }


module Domain : AbstractDomain.S with type astate = t = struct
  type astate = t

  let piecewise_lessthan lhs rhs =
    InvalidAddressesDomain.( <= ) ~lhs:lhs.invalids ~rhs:rhs.invalids
    && AliasingDomain.( <= ) ~lhs:lhs.stack ~rhs:rhs.stack
    && Memory.for_all
         (fun addr_src edges ->
           Memory.Edges.for_all
             (fun edge addr_dst ->
               Memory.find_edge_opt addr_src edge rhs.heap
               |> Option.exists ~f:(fun addr -> AbstractAddress.equal addr addr_dst) )
             edges )
         lhs.heap


  module JoinState = struct
    module AddressUnionSet = struct
      module Set = PrettyPrintable.MakePPSet (AbstractAddress)

      type elt = AbstractAddress.t [@@deriving compare]

      type t = Set.t ref

      let create x = ref (Set.singleton x)

      let compare_size _ _ = 0

      let merge ~from ~to_ =
        (* building the actual set is only useful to display what equalities where discovered in the
           HTML debug output *)
        if Config.debug_mode then to_ := Set.union !from !to_


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
      match Memory.find_edge_opt src_addr access union_heap with
      | None ->
          (Memory.add_edge src_addr access dst_addr union_heap, `No_new_equality)
      | Some dst_addr' ->
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
        |> Option.fold ~init:(visited, union_heap) ~f:(fun visited_union_heap edges ->
               Memory.Edges.fold visit_edge edges visited_union_heap )


    let visit_stack subst heap stack union_heap =
      (* start graph exploration *)
      let visited = Addresses.empty in
      let _, union_heap =
        AliasingDomain.fold
          (fun _var addr (visited, union_heap) -> visit_address subst visited heap addr union_heap)
          stack (visited, union_heap)
      in
      union_heap


    let populate_subst_from_stacks subst stack1 stack2 =
      ignore
        ((* Use [Caml.Map.merge] to detect the variables present in both stacks. Build an empty
            result map since we don't use the result. *)
         AliasingDomain.merge
           (fun _var addr1_opt addr2_opt ->
             Option.both addr1_opt addr2_opt
             |> Option.iter ~f:(fun (addr1, addr2) ->
                    (* stack1 says [_var = addr1] and stack2 says [_var = addr2]: unify the
                       addresses since they are equal to the same variable *)
                    ignore (AddressUF.union subst addr1 addr2) ) ;
             (* empty result map *)
             None )
           stack1 stack2)


    let from_astate_union {heap= heap1; stack= stack1; invalids= invalids1}
        {heap= heap2; stack= stack2; invalids= invalids2} =
      let subst = AddressUF.create () in
      (* gather equalities from the stacks *)
      populate_subst_from_stacks subst stack1 stack2 ;
      (* union the heaps, take this opportunity to do garbage collection of unreachable values by
         only copying the addresses reachable from the variables in the stacks *)
      let heap = visit_stack subst heap1 stack1 Memory.empty |> visit_stack subst heap2 stack2 in
      (* This keeps all the variables and picks one representative address for each variable in
         common thanks to [AbstractAddressDomain_JoinIsMin] *)
      let stack = AliasingDomain.join stack1 stack2 in
      (* basically union *)
      let invalids = InvalidAddressesDomain.join invalids1 invalids2 in
      {subst; astate= {heap; stack; invalids}}


    let rec normalize state =
      let one_addr subst addr edges heap_has_converged =
        Memory.Edges.fold
          (fun access addr_dest (heap, has_converged) ->
            match union_one_edge subst addr access addr_dest heap with
            | heap, `No_new_equality ->
                (heap, has_converged)
            | heap, `New_equality ->
                (heap, false) )
          edges heap_has_converged
      in
      let heap, has_converged =
        Memory.fold (one_addr state.subst) state.astate.heap (Memory.empty, true)
      in
      if has_converged then (
        L.d_strln "Join unified addresses:" ;
        L.d_increase_indent 1 ;
        Container.iter state.subst ~fold:AddressUF.fold_sets
          ~f:(fun ((repr : AddressUF.Repr.t), set) ->
            L.d_strln
              (F.asprintf "%a=%a" AbstractAddress.pp
                 (repr :> AbstractAddress.t)
                 AddressUnionSet.pp set) ) ;
        L.d_decrease_indent 1 ;
        let stack = AliasingDomain.map (to_canonical_address state.subst) state.astate.stack in
        let invalids =
          InvalidAddressesDomain.map (to_canonical_address state.subst) state.astate.invalids
        in
        {heap; stack; invalids} )
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

      ∀ i ∈ {1,2}, ∀ l, x, a, ∀ l' ∈ Ii, ((x, l) ∈ Si ∧ l –a–> l' ∈ Gi)
                                         => (x, σ(l)) ∈ S ∧ σ(l) –a–> σ(l') ∈ G ∧ σ(l') ∈ I

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


  let pp fmt {heap; stack; invalids} =
    F.fprintf fmt "{@[<v1> heap=@[<hv>%a@];@;stack=@[<hv>%a@];@;invalids=@[<hv>%a@];@]}" Memory.pp
      heap AliasingDomain.pp stack InvalidAddressesDomain.pp invalids
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

(** operations on the domain *)
module Operations = struct
  open Result.Monad_infix

  type 'a access_result = ('a, Diagnostic.t) result

  (** Check that the address is not known to be invalid *)
  let check_addr_access actor address astate =
    match InvalidAddressesDomain.get_invalidation address astate.invalids with
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


  (** add addresses to the state to give a address to the destination of the given access path *)
  let walk_access_expr ~on_last astate access_expr location =
    let (access_var, _), access_list = AccessExpression.to_accesses access_expr in
    if Config.write_html then
      L.d_strln
        (F.asprintf "Accessing %a -> [%a]" Var.pp access_var
           (Pp.seq ~sep:"," AccessExpression.Access.pp)
           access_list) ;
    match (on_last, access_list) with
    | `Overwrite new_addr, [] ->
        let stack = AliasingDomain.add access_var new_addr astate.stack in
        Ok ({astate with stack}, new_addr)
    | `Access, _ | `Overwrite _, _ :: _ ->
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
    {astate with invalids= InvalidAddressesDomain.add address actor astate.invalids}


  let havoc_var var astate =
    if AliasingDomain.mem var astate.stack then
      {astate with stack= AliasingDomain.remove var astate.stack}
    else astate


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

include Domain
include Operations
