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

  val equal : t -> t -> bool

  val mk_fresh : unit -> t

  val pp : F.formatter -> t -> unit

  val init : unit -> unit
end = struct
  type t = int [@@deriving compare]

  let equal = [%compare.equal: t]

  let next_fresh = ref 1

  let mk_fresh () =
    let l = !next_fresh in
    incr next_fresh ; l


  let pp = F.pp_print_int

  let init () = next_fresh := 1
end

(* {3 Heap domain } *)

module AddrTracePair = struct
  type t = AbstractAddress.t * PulseTrace.t [@@deriving compare]

  let pp f addr_trace =
    if Config.debug_level_analysis >= 3 then
      Pp.pair ~fst:AbstractAddress.pp ~snd:PulseTrace.pp f addr_trace
    else AbstractAddress.pp f (fst addr_trace)
end

module Attribute = struct
  (* OPTIM: [Invalid _] is first in the order to make queries about invalidation status more
       efficient (only need to look at the first element in the set of attributes to know if an
       address is invalid) *)
  type t =
    (* DO NOT MOVE, see toplevel comment *)
    | Invalid of Invalidation.t
    | AddressOfCppTemporary of Var.t * Location.t option
    | Closure of Typ.Procname.t * AddrTracePair.t list
    | StdVectorReserve
  [@@deriving compare]

  let pp f = function
    | Invalid invalidation ->
        Invalidation.pp f invalidation
    | AddressOfCppTemporary (var, location_opt) ->
        F.fprintf f "&%a (%a)" Var.pp var (Pp.option Location.pp) location_opt
    | Closure (pname, captured) ->
        F.fprintf f "%a[%a]" Typ.Procname.pp pname (Pp.seq ~sep:"," AddrTracePair.pp) captured
    | StdVectorReserve ->
        F.pp_print_string f "std::vector::reserve()"
end

module Attributes = AbstractDomain.FiniteSet (Attribute)

module Memory : sig
  module Access :
    PrettyPrintable.PrintableOrderedType with type t = AbstractAddress.t HilExp.Access.t

  module Edges : PrettyPrintable.PPMap with type key = Access.t

  type edges = AddrTracePair.t Edges.t

  type cell = edges * Attributes.t

  type t [@@deriving compare]

  val empty : t

  val find_opt : AbstractAddress.t -> t -> cell option

  val for_all : (AbstractAddress.t -> cell -> bool) -> t -> bool

  val fold : (AbstractAddress.t -> cell -> 'accum -> 'accum) -> t -> 'accum -> 'accum

  val pp : F.formatter -> t -> unit

  val add_edge : AbstractAddress.t -> Access.t -> AddrTracePair.t -> t -> t

  val add_edge_and_back_edge : AbstractAddress.t -> Access.t -> AddrTracePair.t -> t -> t

  val find_edge_opt : AbstractAddress.t -> Access.t -> t -> AddrTracePair.t option

  val add_attributes : AbstractAddress.t -> Attributes.t -> t -> t

  val invalidate : AbstractAddress.t -> Invalidation.t -> t -> t

  val get_invalidation : AbstractAddress.t -> t -> Invalidation.t option
  (** None denotes a valid location *)

  val std_vector_reserve : AbstractAddress.t -> t -> t

  val is_std_vector_reserved : AbstractAddress.t -> t -> bool
end = struct
  module Access = struct
    type t = AbstractAddress.t HilExp.Access.t [@@deriving compare]

    let pp = HilExp.Access.pp AbstractAddress.pp
  end

  module Edges = PrettyPrintable.MakePPMap (Access)

  type edges = AddrTracePair.t Edges.t [@@deriving compare]

  type cell = edges * Attributes.t [@@deriving compare]

  module Graph = PrettyPrintable.MakePPMap (AbstractAddress)

  type t = cell Graph.t [@@deriving compare]

  let pp =
    Graph.pp ~pp_value:(Pp.pair ~fst:(Edges.pp ~pp_value:AddrTracePair.pp) ~snd:Attributes.pp)


  (* {3 Helper functions to traverse the two maps at once } *)

  let add_edge addr_src access value memory =
    let edges, attrs =
      match Graph.find_opt addr_src memory with
      | Some cell ->
          cell
      | None ->
          (Edges.empty, Attributes.empty)
    in
    Graph.add addr_src (Edges.add access value edges, attrs) memory


  (** [Dereference] edges induce a [TakeAddress] back edge and vice-versa, because
      [*(&x) = &( *x ) = x]. *)
  let add_edge_and_back_edge addr_src (access : Access.t) addr_trace_dst memory =
    let memory = add_edge addr_src access addr_trace_dst memory in
    match access with
    | ArrayAccess _ | FieldAccess _ ->
        memory
    | TakeAddress ->
        add_edge (fst addr_trace_dst) Dereference (addr_src, []) memory
    | Dereference ->
        add_edge (fst addr_trace_dst) TakeAddress (addr_src, []) memory


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

(** Stacks: map addresses of variables to values and initialisation location.

    This is defined as an abstract domain but the domain operations are mostly meaningless on their
    own. It so happens that the join on abstract states uses the join of stacks provided by this
    functor followed by normalization wrt the unification found between abstract locations so it's
    convenient to define stacks as elements of this domain. *)
module Stack = struct
  module VarAddress = struct
    include Var

    let pp f var =
      let pp_ampersand f = function
        | ProgramVar _ ->
            F.pp_print_string f "&"
        | LogicalVar _ ->
            ()
      in
      F.fprintf f "%a%a" pp_ampersand var Var.pp var
  end

  module ValueDomain = struct
    type t = AbstractAddress.t * Location.t option [@@deriving compare]

    let join ((addr1, _) as v1) ((addr2, _) as v2) = if addr1 <= addr2 then v1 else v2

    let ( <= ) ~lhs:(lhs_addr, _) ~rhs:(rhs_addr, _) = AbstractAddress.equal lhs_addr rhs_addr

    let widen ~prev ~next ~num_iters:_ = join prev next

    let pp = Pp.pair ~fst:AbstractAddress.pp ~snd:(Pp.option Location.pp)
  end

  include AbstractDomain.Map (VarAddress) (ValueDomain)

  let pp fmt m =
    let pp_item fmt (var_address, v) =
      F.fprintf fmt "%a=%a" VarAddress.pp var_address ValueDomain.pp v
    in
    PrettyPrintable.pp_collection ~pp_item fmt (bindings m)


  let compare = compare ValueDomain.compare
end

type t = {heap: Memory.t; stack: Stack.t} [@@deriving compare]

let initial =
  { heap=
      Memory.empty
      (* TODO: we could record that 0 is an invalid address at this point but this makes the
         analysis go a bit overboard with the Nullptr reports. *)
  ; stack= Stack.empty }


let piecewise_lessthan lhs rhs =
  Stack.( <= ) ~lhs:lhs.stack ~rhs:rhs.stack
  && Memory.for_all
       (fun addr_src (edges_lhs, attrs_lhs) ->
         let edges_rhs_opt, attrs_rhs =
           let cell = Memory.find_opt addr_src rhs.heap in
           (Option.map ~f:fst cell, Option.value_map ~default:Attributes.empty ~f:snd cell)
         in
         Memory.Edges.for_all
           (fun access_lhs (addr_dst, _) ->
             Option.bind edges_rhs_opt ~f:(fun edges_rhs ->
                 Memory.Edges.find_opt access_lhs edges_rhs )
             |> Option.exists ~f:(fun (addr_dst_rhs, _) ->
                    AbstractAddress.equal addr_dst_rhs addr_dst ) )
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

  (** adds [(src_addr, access, value)] to [union_heap] and record potential new equality that
       results from it in [subst] *)
  let union_one_edge subst src_addr access value union_heap =
    let dst_addr, _ = value in
    let src_addr = to_canonical_address subst src_addr in
    let dst_addr = to_canonical_address subst dst_addr in
    match (Memory.find_edge_opt src_addr access union_heap, (access : Memory.Access.t)) with
    | Some (dst_addr', _), _ when AbstractAddress.equal dst_addr dst_addr' ->
        (* same edge *)
        (union_heap, `No_new_equality)
    | _, ArrayAccess _ ->
        (* do not trust array accesses for now, replace the destination of the edge by a fresh location *)
        ( Memory.add_edge src_addr access (AbstractAddress.mk_fresh (), []) union_heap
        , `No_new_equality )
    | None, _ ->
        (Memory.add_edge src_addr access value union_heap, `No_new_equality)
    | Some (dst_addr', _), _ ->
        (* new equality [dst_addr = dst_addr'] found *)
        ignore (AddressUF.union subst dst_addr dst_addr') ;
        (union_heap, `New_equality)


  module Addresses = Caml.Set.Make (AbstractAddress)

  let rec visit_address subst visited heap addr union_heap =
    if Addresses.mem addr visited then (visited, union_heap)
    else
      let visited = Addresses.add addr visited in
      let visit_edge access value (visited, union_heap) =
        union_one_edge subst addr access value union_heap
        |> fst
        |> visit_address subst visited heap (fst value)
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
        (fun _var (addr, _) (visited, union_heap) ->
          visit_address subst visited heap addr union_heap )
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
           |> Option.iter ~f:(fun ((addr1, _), (addr2, _)) ->
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
      let pp_union_find_classes f subst =
        Container.iter subst ~fold:AddressUF.fold_sets ~f:(fun ((repr : AddressUF.Repr.t), set) ->
            F.fprintf f "%a=%a@;" AbstractAddress.pp
              (repr :> AbstractAddress.t)
              AddressUnionSet.pp set )
      in
      L.d_printfln "Join unified addresses:@\n@[<v2>  %a@]" pp_union_find_classes state.subst ;
      let stack =
        Stack.map
          (fun (addr, loc) -> (to_canonical_address state.subst addr, loc))
          state.astate.stack
      in
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
  if num_iters > max_widening then prev else if phys_equal prev next then prev else join prev next


let pp fmt {heap; stack} =
  F.fprintf fmt "{@[<v1> heap=@[<hv>%a@];@;stack=@[<hv>%a@];@]}" Memory.pp heap Stack.pp stack
