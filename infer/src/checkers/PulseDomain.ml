(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
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

  val filter : (AbstractAddress.t -> cell -> bool) -> t -> t

  val find_opt : AbstractAddress.t -> t -> cell option

  val for_all : (AbstractAddress.t -> cell -> bool) -> t -> bool

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


let join _ _ = (* not implemented: use disjunctive domain instead *) assert false

let ( <= ) ~lhs ~rhs = phys_equal lhs rhs || piecewise_lessthan lhs rhs

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
      | Some (edges, attrs) ->
          visit_edges astate ~edges visited |> visit_attributes astate attrs )


  and visit_edges ~edges astate visited =
    Memory.Edges.fold
      (fun _access (address, _trace) visited -> visit_address astate address visited)
      edges visited


  and visit_attributes astate attrs visited =
    Attributes.fold
      (fun attr visited ->
        match attr with
        | Attribute.Invalid _ | Attribute.AddressOfCppTemporary _ | Attribute.StdVectorReserve ->
            visited
        | Attribute.Closure (_, captured) ->
            List.fold captured ~init:visited ~f:(fun visited (address, _) ->
                visit_address astate address visited ) )
      attrs visited


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
