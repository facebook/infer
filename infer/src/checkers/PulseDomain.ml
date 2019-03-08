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

(** Purposefully declared before [AbstractAddress] to avoid attributes depending on
    addresses. Otherwise they become a pain to handle when comparing memory states. *)
module Attribute = struct
  (* OPTIM: [Invalid _] is first in the order to make queries about invalidation status more
       efficient (only need to look at the first element in the set of attributes to know if an
       address is invalid) *)
  type t =
    (* DO NOT MOVE, see toplevel comment *)
    | Invalid of Invalidation.t
    | AddressOfCppTemporary of Var.t * Location.t option
    | Closure of Typ.Procname.t
    | StdVectorReserve
  [@@deriving compare]

  let pp f = function
    | Invalid invalidation ->
        Invalidation.pp f invalidation
    | AddressOfCppTemporary (var, location_opt) ->
        F.fprintf f "&%a (%a)" Var.pp var (Pp.option Location.pp) location_opt
    | Closure pname ->
        F.fprintf f "%a" Typ.Procname.pp pname
    | StdVectorReserve ->
        F.pp_print_string f "std::vector::reserve()"
end

module Attributes = struct
  include AbstractDomain.FiniteSet (Attribute)

  let get_invalid attrs =
    (* Since we often want to find out whether an address is invalid this case is optimised. Since
       [Invalid _] attributes are the smallest we can simply look at the first element to decide if
       an address is invalid or not. *)
    match min_elt_opt attrs with Some (Invalid invalidation) -> Some invalidation | _ -> None
end

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

module Memory : sig
  module Access : sig
    include PrettyPrintable.PrintableOrderedType with type t = AbstractAddress.t HilExp.Access.t

    val equal : t -> t -> bool
  end

  module Edges : PrettyPrintable.PPMap with type key = Access.t

  type edges = AddrTracePair.t Edges.t

  type cell = edges * Attributes.t

  type t [@@deriving compare]

  val empty : t

  val filter : (AbstractAddress.t -> cell -> bool) -> t -> t

  val find_opt : AbstractAddress.t -> t -> cell option

  val set_cell : AbstractAddress.t -> cell -> t -> t

  val pp : F.formatter -> t -> unit

  val add_edge : AbstractAddress.t -> Access.t -> AddrTracePair.t -> t -> t

  val add_edge_and_back_edge : AbstractAddress.t -> Access.t -> AddrTracePair.t -> t -> t

  val find_edge_opt : AbstractAddress.t -> Access.t -> t -> AddrTracePair.t option

  val add_attributes : AbstractAddress.t -> Attributes.t -> t -> t

  val invalidate : AbstractAddress.t -> Invalidation.t -> t -> t

  val check_valid : AbstractAddress.t -> t -> (unit, Invalidation.t) result

  val std_vector_reserve : AbstractAddress.t -> t -> t

  val is_std_vector_reserved : AbstractAddress.t -> t -> bool
end = struct
  module Access = struct
    type t = AbstractAddress.t HilExp.Access.t [@@deriving compare]

    let equal = [%compare.equal: t]

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


  let check_valid address memory =
    match
      Graph.find_opt address memory |> Option.map ~f:snd |> Option.bind ~f:Attributes.get_invalid
    with
    | Some invalidation ->
        Error invalidation
    | None ->
        Ok ()


  let std_vector_reserve address memory = add_attribute address Attribute.StdVectorReserve memory

  let is_std_vector_reserved address memory =
    Graph.find_opt address memory |> Option.map ~f:snd
    |> Option.value_map ~default:false ~f:(fun attributes ->
           Attributes.mem Attribute.StdVectorReserve attributes )


  (* {3 Monomorphic {!PPMap} interface as needed } *)

  let empty = Graph.empty

  let find_opt = Graph.find_opt

  let set_cell = Graph.add

  let filter = Graph.filter
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


(** comparison between two elements of the domain to determine the [<=] relation

    Given two states [lhs] and [rhs], try to find a bijection [lhs_to_rhs]/[rhs_to_lhs] between the
    addresses of [lhs] and [rhs] such that [lhs_to_rhs(lhs)] contains (is a supergraph of) [rhs].
*)
module GraphComparison = struct
  module AddressMap = PrettyPrintable.MakePPMap (AbstractAddress)

  (** translation between the abstract values on the LHS and the ones on the RHS *)
  type mapping =
    { rhs_to_lhs: AbstractAddress.t AddressMap.t  (** map from RHS values to LHS *)
    ; lhs_to_rhs: AbstractAddress.t AddressMap.t  (** inverse map from [rhs_to_lhs] *) }

  let empty_mapping = {rhs_to_lhs= AddressMap.empty; lhs_to_rhs= AddressMap.empty}

  let pp_mapping fmt {rhs_to_lhs; lhs_to_rhs} =
    F.fprintf fmt "@[<v>{ rhs_to_lhs=@[<hv2>%a@];@,lhs_to_rhs=@[<hv2>%a@];@,}@]"
      (AddressMap.pp ~pp_value:AbstractAddress.pp)
      rhs_to_lhs
      (AddressMap.pp ~pp_value:AbstractAddress.pp)
      lhs_to_rhs


  (** try to add the fact that [addr_lhs] corresponds to [addr_rhs] to the [mapping] *)
  let record_equal ~addr_lhs ~addr_rhs mapping =
    (* have we seen [addr_lhs] before?.. *)
    match AddressMap.find_opt addr_lhs mapping.lhs_to_rhs with
    | Some addr_rhs' when not (AbstractAddress.equal addr_rhs addr_rhs') ->
        (* ...yes, but it was bound to another address *)
        L.d_printfln
          "Aliasing in LHS not in RHS: LHS address %a in current already bound to %a, not %a@\n\
           State=%a"
          AbstractAddress.pp addr_lhs AbstractAddress.pp addr_rhs' AbstractAddress.pp addr_rhs
          pp_mapping mapping ;
        `AliasingLHS
    | Some _addr_rhs (* [_addr_rhs = addr_rhs] *) ->
        `AlreadyVisited
    | None -> (
      (* ...and have we seen [addr_rhs] before?.. *)
      match AddressMap.find_opt addr_rhs mapping.rhs_to_lhs with
      | Some addr_lhs' ->
          (* ...yes, but it was bound to another address: [addr_lhs' != addr_lhs] otherwise we would
             have found [addr_lhs] in the [lhs_to_rhs] map above *)
          L.d_printfln
            "Aliasing in RHS not in LHS: RHS address %a in current already bound to %a, not %a@\n\
             State=%a"
            AbstractAddress.pp addr_rhs AbstractAddress.pp addr_lhs' AbstractAddress.pp addr_lhs
            pp_mapping mapping ;
          `AliasingRHS
      | None ->
          (* [addr_rhs] and [addr_lhs] are both new, record that they correspond to each other *)
          let mapping' =
            { rhs_to_lhs= AddressMap.add addr_rhs addr_lhs mapping.rhs_to_lhs
            ; lhs_to_rhs= AddressMap.add addr_lhs addr_rhs mapping.lhs_to_rhs }
          in
          `NotAlreadyVisited mapping' )


  type supergraph_relation =
    | NotASupergraph  (** no mapping was found that can make LHS bigger than RHS *)
    | Supergraph of mapping  (** [mapping(lhs)] is a supergraph of [rhs] *)

  (** can we extend [mapping] so that the subgraph of [lhs] rooted at [addr_lhs] is a supergraph of
     the subgraph of [rhs] rooted at [addr_rhs]? *)
  let rec supergraph_map_from_address ~lhs ~addr_lhs ~rhs ~addr_rhs mapping =
    L.d_printfln "%a<->%a@\n" AbstractAddress.pp addr_lhs AbstractAddress.pp addr_rhs ;
    match record_equal mapping ~addr_lhs ~addr_rhs with
    | `AlreadyVisited ->
        Supergraph mapping
    | `AliasingRHS | `AliasingLHS ->
        NotASupergraph
    | `NotAlreadyVisited mapping -> (
      match Memory.find_opt addr_rhs rhs.heap with
      | None ->
          (* nothing on the RHS: nothing more to check *)
          Supergraph mapping
      | Some (edges_rhs, attrs_rhs) -> (
        match Memory.find_opt addr_lhs lhs.heap with
        | None ->
            (* [RHS] is not a subgraph of [LHS] unless [addr_rhs] has no attributes and no edges
               (could happen because of [register_address] or because we don't care to delete empty
               edges when removing edges) *)
            if Memory.Edges.is_empty edges_rhs && Attributes.is_empty attrs_rhs then
              Supergraph mapping
            else NotASupergraph
        | Some (edges_lhs, attrs_lhs) ->
            (* continue the comparison recursively on all edges and attributes *)
            if Attributes.subset attrs_rhs attrs_lhs then
              let bindings_lhs = Memory.Edges.bindings edges_lhs in
              let bindings_rhs = Memory.Edges.bindings edges_rhs in
              supergraph_map_edges ~lhs ~edges_lhs:bindings_lhs ~rhs ~edges_rhs:bindings_rhs
                mapping
            else NotASupergraph ) )


  (** check that the supergraph relation can be extended for all edges *)
  and supergraph_map_edges ~lhs ~edges_lhs ~rhs ~edges_rhs mapping =
    match (edges_lhs, edges_rhs) with
    | _, [] ->
        (* more edges on the LHS => supergraph *)
        Supergraph mapping
    | (a_lhs, (addr_lhs, _trace_lhs)) :: edges_lhs, (a_rhs, (addr_rhs, _trace_rhs)) :: edges_rhs
      when Memory.Access.equal a_lhs a_rhs -> (
      (* check supergraph relation from the destination addresses *)
      match supergraph_map_from_address ~lhs ~addr_lhs ~rhs ~addr_rhs mapping with
      | Supergraph mapping ->
          (* ok: continue with the other edges *)
          supergraph_map_edges ~lhs ~edges_lhs ~rhs ~edges_rhs mapping
      | NotASupergraph ->
          NotASupergraph )
    | (a_lhs, _) :: edges_lhs, (a_rhs, _) :: _ when Memory.Access.compare a_lhs a_rhs < 0 ->
        (* [a_lhs] is not present on the RHS: that's ok: the LHS knows more than the RHS  *)
        supergraph_map_edges ~lhs ~edges_lhs ~rhs ~edges_rhs mapping
    | _, _ :: _ ->
        (* [a_rhs] is not present on the LHS: that's not ok: the RHS knows more than the LHS *)
        NotASupergraph


  (** check that the memory graph induced by the addresses in [lhs] reachable from the variables in
     [stack_lhs] is a supergraph of the same graph in [rhs] starting from [stack_rhs], up to some
     [mapping] *)
  let rec supergraph_map_from_stack ~lhs ~stack_lhs ~rhs ~stack_rhs mapping =
    match (stack_lhs, stack_rhs) with
    | _, [] ->
        Supergraph mapping
    | ( (var_lhs, (addr_lhs, _trace_lhs)) :: stack_lhs
      , (var_rhs, (addr_rhs, _trace_rhs)) :: stack_rhs )
      when Var.equal var_lhs var_rhs -> (
      match supergraph_map_from_address ~lhs ~addr_lhs ~rhs ~addr_rhs mapping with
      | Supergraph mapping ->
          supergraph_map_from_stack ~lhs ~stack_lhs ~rhs ~stack_rhs mapping
      | NotASupergraph ->
          NotASupergraph )
    | (var_lhs, _) :: stack_lhs, (var_rhs, _) :: _ when Var.compare var_lhs var_rhs < 0 ->
        (* [var_lhs] is not present on the RHS: that's ok: the LHS knows more than the RHS  *)
        supergraph_map_from_stack ~lhs ~stack_lhs ~rhs ~stack_rhs mapping
    | _, _ :: _ ->
        NotASupergraph


  let supergraph_map ~lhs ~rhs mapping =
    let stack_lhs = Stack.bindings lhs.stack in
    let stack_rhs = Stack.bindings rhs.stack in
    supergraph_map_from_stack ~lhs ~rhs ~stack_lhs ~stack_rhs mapping


  let is_supergraph ~lhs ~rhs mapping =
    match supergraph_map ~lhs ~rhs mapping with Supergraph _ -> true | NotASupergraph -> false
end

let join _ _ = (* not implemented: use disjunctive domain instead *) assert false

let ( <= ) ~lhs ~rhs =
  (* [lhs] implies [rhs] if it knows more facts than [rhs] *)
  phys_equal lhs rhs || GraphComparison.is_supergraph ~lhs ~rhs GraphComparison.empty_mapping


let widen ~prev:_ ~next:_ ~num_iters:_ =
  (* not implemented: use disjunctive domain instead *) assert false


let pp fmt {heap; stack} =
  F.fprintf fmt "{@[<v1> heap=@[<hv>%a@];@;stack=@[<hv>%a@];@]}" Memory.pp heap Stack.pp stack


module GraphGC : sig
  val minimize : t -> t
  (** compute the set of abstract addresses that are "used" in the abstract state, i.e. reachable
     from the stack variables, then removes all the unused addresses from the heap *)
end = struct
  module AddressSet = PrettyPrintable.MakePPSet (AbstractAddress)

  let visit address visited =
    if AddressSet.mem address visited then `AlreadyVisited
    else
      let visited = AddressSet.add address visited in
      `NotAlreadyVisited visited


  let rec visit_address astate address visited =
    match visit address visited with
    | `AlreadyVisited ->
        visited
    | `NotAlreadyVisited visited -> (
      match Memory.find_opt address astate.heap with
      | None ->
          visited
      | Some (edges, _) ->
          visit_edges astate ~edges visited )


  and visit_edges ~edges astate visited =
    Memory.Edges.fold
      (fun _access (address, _trace) visited -> visit_address astate address visited)
      edges visited


  let visit_stack astate visited =
    Stack.fold
      (fun _var (address, _loc) visited -> visit_address astate address visited)
      astate.stack visited


  let minimize astate =
    let visited = visit_stack astate AddressSet.empty in
    let heap = Memory.filter (fun address _ -> AddressSet.mem address visited) astate.heap in
    if phys_equal heap astate.heap then astate else {astate with heap}
end

include GraphGC
include GraphComparison
