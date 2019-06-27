(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging

(* {2 Abstract domain description } *)

module Invalidation = struct
  type std_vector_function =
    | Assign
    | Clear
    | Emplace
    | EmplaceBack
    | Insert
    | PushBack
    | Reserve
    | ShrinkToFit
  [@@deriving compare]

  let pp_std_vector_function f = function
    | Assign ->
        F.fprintf f "std::vector::assign"
    | Clear ->
        F.fprintf f "std::vector::clear"
    | Emplace ->
        F.fprintf f "std::vector::emplace"
    | EmplaceBack ->
        F.fprintf f "std::vector::emplace_back"
    | Insert ->
        F.fprintf f "std::vector::insert"
    | PushBack ->
        F.fprintf f "std::vector::push_back"
    | Reserve ->
        F.fprintf f "std::vector::reserve"
    | ShrinkToFit ->
        F.fprintf f "std::vector::shrink_to_fit"


  type t =
    | CFree
    | CppDelete
    | GoneOutOfScope of Pvar.t * Typ.t
    | Nullptr
    | StdVector of std_vector_function
  [@@deriving compare]

  let issue_type_of_cause = function
    | CFree ->
        IssueType.use_after_free
    | CppDelete ->
        IssueType.use_after_delete
    | GoneOutOfScope _ ->
        IssueType.use_after_lifetime
    | Nullptr ->
        IssueType.null_dereference
    | StdVector _ ->
        IssueType.vector_invalidation


  let describe f cause =
    match cause with
    | CFree ->
        F.pp_print_string f "was invalidated by call to `free()`"
    | CppDelete ->
        F.pp_print_string f "was invalidated by `delete`"
    | GoneOutOfScope (pvar, typ) ->
        let pp_var f pvar =
          if Pvar.is_cpp_temporary pvar then
            F.fprintf f "is the address of a C++ temporary of type `%a`" (Typ.pp_full Pp.text) typ
          else F.fprintf f "is the address of a stack variable `%a`" Pvar.pp_value pvar
        in
        F.fprintf f "%a whose lifetime has ended" pp_var pvar
    | Nullptr ->
        F.pp_print_string f "is the null pointer"
    | StdVector std_vector_f ->
        F.fprintf f "was potentially invalidated by `%a()`" pp_std_vector_function std_vector_f


  let pp f invalidation =
    match invalidation with
    | CFree ->
        F.fprintf f "CFree(%a)" describe invalidation
    | CppDelete ->
        F.fprintf f "CppDelete(%a)" describe invalidation
    | GoneOutOfScope _ ->
        describe f invalidation
    | Nullptr ->
        describe f invalidation
    | StdVector _ ->
        F.fprintf f "StdVector(%a)" describe invalidation
end

module ValueHistory = struct
  type event =
    | VariableDeclaration of Location.t
    | CppTemporaryCreated of Location.t
    | Assignment of {location: Location.t}
    | Capture of {captured_as: Pvar.t; location: Location.t}
    | Call of
        { f:
            [ `Call of Typ.Procname.t
            | `UnknownCall of Typ.Procname.t
            | `IndirectCall of Exp.t
            | `Model of string ]
        ; location: Location.t }
  [@@deriving compare]

  let pp_event_no_location fmt = function
    | VariableDeclaration _ ->
        F.pp_print_string fmt "variable declared"
    | CppTemporaryCreated _ ->
        F.pp_print_string fmt "C++ temporary created"
    | Capture {captured_as; location= _} ->
        F.fprintf fmt "value captured as `%a`" (Pvar.pp Pp.text) captured_as
    | Assignment _ ->
        F.pp_print_string fmt "assigned"
    | Call {f; location= _} ->
        let pp_f fmt = function
          | `Call proc_name ->
              F.fprintf fmt "`%a()`" Typ.Procname.pp proc_name
          | `UnknownCall proc_name ->
              F.fprintf fmt "unknown function `%a`" Typ.Procname.pp proc_name
          | `IndirectCall call_exp ->
              F.fprintf fmt "unresolved expression `%a`" Exp.pp call_exp
          | `Model model ->
              F.fprintf fmt "`%s()` (pulse model)" model
        in
        F.fprintf fmt "returned from call to %a" pp_f f


  let location_of_event = function
    | VariableDeclaration location
    | CppTemporaryCreated location
    | Assignment {location}
    | Capture {location}
    | Call {location} ->
        location


  let pp_event fmt crumb =
    F.fprintf fmt "%a at %a" pp_event_no_location crumb Location.pp_line (location_of_event crumb)


  let errlog_trace_elem_of_event ~nesting crumb =
    let location = location_of_event crumb in
    let description = F.asprintf "%a" pp_event_no_location crumb in
    let tags = [] in
    Errlog.make_trace_element nesting location description tags


  type t = event list [@@deriving compare]

  let pp f events = Pp.seq ~print_env:Pp.text_break pp_event f events

  let add_to_errlog ~nesting events errlog =
    List.rev_map_append ~f:(errlog_trace_elem_of_event ~nesting) events errlog


  let get_start_location = function [] -> None | crumb :: _ -> Some (location_of_event crumb)
end

module InterprocAction = struct
  type 'a t =
    | Immediate of {imm: 'a; location: Location.t}
    | ViaCall of {action: 'a t; proc_name: Typ.Procname.t; location: Location.t}
  [@@deriving compare]

  let rec get_immediate = function
    | Immediate {imm; _} ->
        imm
    | ViaCall {action; _} ->
        get_immediate action


  let pp pp_immediate fmt = function
    | Immediate {imm; _} ->
        pp_immediate fmt imm
    | ViaCall {proc_name; action; _} ->
        F.fprintf fmt "%a in call to `%a`" pp_immediate (get_immediate action)
          Typ.Procname.describe proc_name


  let add_to_errlog ~nesting pp_immediate action errlog =
    let rec aux ~nesting rev_errlog action =
      match action with
      | Immediate {imm; location} ->
          let rev_errlog =
            Errlog.make_trace_element nesting location (F.asprintf "%a here" pp_immediate imm) []
            :: rev_errlog
          in
          List.rev_append rev_errlog errlog
      | ViaCall {action; proc_name; location} ->
          aux ~nesting:(nesting + 1)
            ( Errlog.make_trace_element nesting location
                (F.asprintf "when calling `%a` here" Typ.Procname.describe proc_name)
                []
            :: rev_errlog )
            action
    in
    aux ~nesting [] action


  let to_outer_location = function Immediate {location} | ViaCall {location} -> location
end

module Trace = struct
  type 'a t = {action: 'a InterprocAction.t; history: ValueHistory.t} [@@deriving compare]

  let pp pp_immediate f {action; _} = InterprocAction.pp pp_immediate f action

  let add_errlog_header ~title location errlog =
    let depth = 0 in
    let tags = [] in
    Errlog.make_trace_element depth location title tags :: errlog


  let add_to_errlog ~header pp_immediate trace errlog =
    let start_location =
      match ValueHistory.get_start_location trace.history with
      | Some location ->
          location
      | None ->
          InterprocAction.to_outer_location trace.action
    in
    add_errlog_header ~title:header start_location
    @@ ValueHistory.add_to_errlog ~nesting:1 trace.history
    @@ InterprocAction.add_to_errlog ~nesting:1 pp_immediate trace.action
    @@ errlog
end

(** Purposefully declared before [AbstractAddress] to avoid attributes depending on
    addresses. Otherwise they become a pain to handle when comparing memory states. *)
module Attribute = struct
  type t =
    | Invalid of Invalidation.t Trace.t
    | MustBeValid of unit InterprocAction.t
    | AddressOfCppTemporary of Var.t * ValueHistory.t
    | Closure of Typ.Procname.t
    | StdVectorReserve
  [@@deriving compare, variants]

  let equal = [%compare.equal: t]

  let to_rank = Variants.to_rank

  let closure_rank = Variants.to_rank (Closure (Typ.Procname.from_string_c_fun ""))

  let invalid_rank =
    Variants.to_rank
      (Invalid
         {action= Immediate {imm= Invalidation.Nullptr; location= Location.dummy}; history= []})


  let must_be_valid_rank =
    Variants.to_rank (MustBeValid (Immediate {imm= (); location= Location.dummy}))


  let std_vector_reserve_rank = Variants.to_rank StdVectorReserve

  let pp f = function
    | Invalid invalidation ->
        (Trace.pp Invalidation.pp) f invalidation
    | MustBeValid action ->
        F.fprintf f "MustBeValid (read by %a @ %a)"
          (InterprocAction.pp (fun _ () -> ()))
          action Location.pp
          (InterprocAction.to_outer_location action)
    | AddressOfCppTemporary (var, history) ->
        F.fprintf f "&%a (%a)" Var.pp var ValueHistory.pp history
    | Closure pname ->
        Typ.Procname.pp f pname
    | StdVectorReserve ->
        F.pp_print_string f "std::vector::reserve()"
end

module Attributes = struct
  module Set = PrettyPrintable.MakePPUniqRankSet (Attribute)

  let get_invalid attrs =
    Set.find_rank attrs Attribute.invalid_rank
    |> Option.map ~f:(fun attr ->
           let[@warning "-8"] (Attribute.Invalid invalidation) = attr in
           invalidation )


  let get_must_be_valid attrs =
    Set.find_rank attrs Attribute.must_be_valid_rank
    |> Option.map ~f:(fun attr ->
           let[@warning "-8"] (Attribute.MustBeValid action) = attr in
           action )


  let get_closure_proc_name attrs =
    Set.find_rank attrs Attribute.closure_rank
    |> Option.map ~f:(fun attr ->
           let[@warning "-8"] (Attribute.Closure proc_name) = attr in
           proc_name )


  let is_std_vector_reserved attrs =
    Set.find_rank attrs Attribute.std_vector_reserve_rank |> Option.is_some


  include Set
end

(** An abstract address in memory. *)
module AbstractAddress : sig
  type t = private int [@@deriving compare]

  val equal : t -> t -> bool

  val mk_fresh : unit -> t

  val pp : F.formatter -> t -> unit

  val init : unit -> unit

  type state

  val get_state : unit -> state

  val set_state : state -> unit
end = struct
  type t = int [@@deriving compare]

  let equal = [%compare.equal: t]

  let next_fresh = ref 1

  let mk_fresh () =
    let l = !next_fresh in
    incr next_fresh ; l


  let pp = F.pp_print_int

  let init () = next_fresh := 1

  type state = int

  let get_state () = !next_fresh

  let set_state counter = next_fresh := counter
end

module AbstractAddressSet = PrettyPrintable.MakePPSet (AbstractAddress)
module AbstractAddressMap = PrettyPrintable.MakePPMap (AbstractAddress)

(* {3 Heap domain } *)

module AddrTracePair = struct
  type t = AbstractAddress.t * ValueHistory.t [@@deriving compare]

  let pp f addr_trace =
    if Config.debug_level_analysis >= 3 then
      Pp.pair ~fst:AbstractAddress.pp ~snd:ValueHistory.pp f addr_trace
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

  type t

  val empty : t

  val filter : (AbstractAddress.t -> bool) -> t -> t

  val find_opt : AbstractAddress.t -> t -> cell option

  val set_cell : AbstractAddress.t -> cell -> t -> t

  val find_attrs_opt : AbstractAddress.t -> t -> Attributes.t option

  val find_edges_opt : AbstractAddress.t -> t -> edges option

  val mem_edges : AbstractAddress.t -> t -> bool

  val pp : F.formatter -> t -> unit

  val register_address : AbstractAddress.t -> t -> t

  val add_edge : AbstractAddress.t -> Access.t -> AddrTracePair.t -> t -> t

  val find_edge_opt : AbstractAddress.t -> Access.t -> t -> AddrTracePair.t option

  val add_attributes : AbstractAddress.t -> Attributes.t -> t -> t

  val invalidate : AbstractAddress.t * ValueHistory.t -> Invalidation.t InterprocAction.t -> t -> t

  val check_valid : AbstractAddress.t -> t -> (unit, Invalidation.t Trace.t) result

  val get_closure_proc_name : AbstractAddress.t -> t -> Typ.Procname.t option

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

  type cell = edges * Attributes.t

  module Graph = PrettyPrintable.MakePPMap (AbstractAddress)

  type t = edges Graph.t * Attributes.t Graph.t

  let pp =
    Pp.pair
      ~fst:(Graph.pp ~pp_value:(Edges.pp ~pp_value:AddrTracePair.pp))
      ~snd:(Graph.pp ~pp_value:Attributes.pp)


  let register_address addr memory =
    if Graph.mem addr (fst memory) then memory
    else (Graph.add addr Edges.empty (fst memory), snd memory)


  (* {3 Helper functions to traverse the two maps at once } *)

  let add_edge addr_src access value memory =
    let old_edges = Graph.find_opt addr_src (fst memory) |> Option.value ~default:Edges.empty in
    let new_edges = Edges.add access value old_edges in
    if phys_equal old_edges new_edges then memory
    else (Graph.add addr_src new_edges (fst memory), snd memory)


  let find_edge_opt addr access memory =
    let open Option.Monad_infix in
    Graph.find_opt addr (fst memory) >>= Edges.find_opt access


  let add_attributes addr attrs memory =
    if Attributes.is_empty attrs then memory
    else
      let old_attrs = Graph.find_opt addr (snd memory) |> Option.value ~default:Attributes.empty in
      if phys_equal old_attrs attrs || Attributes.is_subset attrs ~of_:old_attrs then memory
      else
        let new_attrs = Attributes.union_prefer_left attrs old_attrs in
        (fst memory, Graph.add addr new_attrs (snd memory))


  let add_attribute address attribute memory =
    add_attributes address (Attributes.singleton attribute) memory


  let invalidate (address, history) invalidation memory =
    add_attribute address (Attribute.Invalid {action= invalidation; history}) memory


  let check_valid address memory =
    L.d_printfln "Checking validity of %a" AbstractAddress.pp address ;
    match Graph.find_opt address (snd memory) |> Option.bind ~f:Attributes.get_invalid with
    | Some invalidation ->
        Error invalidation
    | None ->
        Ok ()


  let get_closure_proc_name address memory =
    Graph.find_opt address (snd memory)
    |> Option.bind ~f:(fun attributes -> Attributes.get_closure_proc_name attributes)


  let std_vector_reserve address memory = add_attribute address Attribute.StdVectorReserve memory

  let is_std_vector_reserved address memory =
    Graph.find_opt address (snd memory)
    |> Option.value_map ~default:false ~f:(fun attributes ->
           Attributes.is_std_vector_reserved attributes )


  (* {3 Monomorphic {!PPMap} interface as needed } *)

  let empty = (Graph.empty, Graph.empty)

  let find_edges_opt addr memory = Graph.find_opt addr (fst memory)

  let find_attrs_opt addr memory = Graph.find_opt addr (snd memory)

  let find_opt addr memory =
    match (find_edges_opt addr memory, find_attrs_opt addr memory) with
    | None, None ->
        None
    | edges_opt, attrs_opt ->
        let edges = Option.value edges_opt ~default:Edges.empty in
        let attrs = Option.value attrs_opt ~default:Attributes.empty in
        Some (edges, attrs)


  let set_cell addr (edges, attrs) memory =
    (Graph.add addr edges (fst memory), Graph.add addr attrs (snd memory))


  let filter f memory =
    let heap = Graph.filter (fun address _ -> f address) (fst memory) in
    let attrs = Graph.filter (fun address _ -> f address) (snd memory) in
    if phys_equal heap (fst memory) && phys_equal attrs (snd memory) then memory else (heap, attrs)


  let mem_edges addr memory = Graph.mem addr (fst memory)
end

(** Stacks: map addresses of variables to values and initialisation location. *)
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

  include PrettyPrintable.MakePPMonoMap (VarAddress) (AddrTracePair)

  let pp fmt m =
    let pp_item fmt (var_address, v) =
      F.fprintf fmt "%a=%a" VarAddress.pp var_address AddrTracePair.pp v
    in
    PrettyPrintable.pp_collection ~pp_item fmt (bindings m)


  let compare = compare AddrTracePair.compare
end

type t = {heap: Memory.t; stack: Stack.t}

let empty =
  { heap=
      Memory.empty
      (* TODO: we could record that 0 is an invalid address at this point but this makes the
         analysis go a bit overboard with the Nullptr reports. *)
  ; stack= Stack.empty }


(** comparison between two elements of the domain to determine the [<=] relation

    Given two states [lhs] and [rhs], try to find a bijection [lhs_to_rhs] (with inverse
    [rhs_to_lhs]) between the addresses of [lhs] and [rhs] such that [lhs_to_rhs(reachable(lhs)) =
    reachable(rhs)] (where addresses are reachable if they are reachable from stack variables).  *)
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


  type isograph_relation =
    | NotIsomorphic  (** no mapping was found that can make LHS the same as the RHS *)
    | IsomorphicUpTo of mapping  (** [mapping(lhs)] is isomorphic to [rhs] *)

  (** can we extend [mapping] so that the subgraph of [lhs] rooted at [addr_lhs] is isomorphic to
      the subgraph of [rhs] rooted at [addr_rhs]? *)
  let rec isograph_map_from_address ~lhs ~addr_lhs ~rhs ~addr_rhs mapping =
    L.d_printfln "%a<->%a@\n" AbstractAddress.pp addr_lhs AbstractAddress.pp addr_rhs ;
    match record_equal mapping ~addr_lhs ~addr_rhs with
    | `AlreadyVisited ->
        IsomorphicUpTo mapping
    | `AliasingRHS | `AliasingLHS ->
        NotIsomorphic
    | `NotAlreadyVisited mapping -> (
        let get_non_empty_cell = function
          | None ->
              None
          | Some (edges, attrs) when Memory.Edges.is_empty edges && Attributes.is_empty attrs ->
              (* this can happen because of [register_address] or because we don't care to delete empty
               edges when removing edges *)
              None
          | Some _ as some_cell ->
              some_cell
        in
        let lhs_cell_opt = Memory.find_opt addr_lhs lhs.heap |> get_non_empty_cell in
        let rhs_cell_opt = Memory.find_opt addr_rhs rhs.heap |> get_non_empty_cell in
        match (lhs_cell_opt, rhs_cell_opt) with
        | None, None ->
            IsomorphicUpTo mapping
        | Some _, None | None, Some _ ->
            NotIsomorphic
        | Some (edges_rhs, attrs_rhs), Some (edges_lhs, attrs_lhs) ->
            (* continue the comparison recursively on all edges and attributes *)
            if Attributes.equal attrs_rhs attrs_lhs then
              let bindings_lhs = Memory.Edges.bindings edges_lhs in
              let bindings_rhs = Memory.Edges.bindings edges_rhs in
              isograph_map_edges ~lhs ~edges_lhs:bindings_lhs ~rhs ~edges_rhs:bindings_rhs mapping
            else NotIsomorphic )


  (** check that the isograph relation can be extended for all edges *)
  and isograph_map_edges ~lhs ~edges_lhs ~rhs ~edges_rhs mapping =
    match (edges_lhs, edges_rhs) with
    | [], [] ->
        IsomorphicUpTo mapping
    | (a_lhs, (addr_lhs, _trace_lhs)) :: edges_lhs, (a_rhs, (addr_rhs, _trace_rhs)) :: edges_rhs
      when Memory.Access.equal a_lhs a_rhs -> (
      (* check isograph relation from the destination addresses *)
      match isograph_map_from_address ~lhs ~addr_lhs ~rhs ~addr_rhs mapping with
      | IsomorphicUpTo mapping ->
          (* ok: continue with the other edges *)
          isograph_map_edges ~lhs ~edges_lhs ~rhs ~edges_rhs mapping
      | NotIsomorphic ->
          NotIsomorphic )
    | _ :: _, _ :: _ | [], _ :: _ | _ :: _, [] ->
        NotIsomorphic


  (** check that the memory graph induced by the addresses in [lhs] reachable from the variables in
     [stack_lhs] is a isograph of the same graph in [rhs] starting from [stack_rhs], up to some
     [mapping] *)
  let rec isograph_map_from_stack ~lhs ~stack_lhs ~rhs ~stack_rhs mapping =
    match (stack_lhs, stack_rhs) with
    | [], [] ->
        IsomorphicUpTo mapping
    | ( (var_lhs, (addr_lhs, _trace_lhs)) :: stack_lhs
      , (var_rhs, (addr_rhs, _trace_rhs)) :: stack_rhs )
      when Var.equal var_lhs var_rhs -> (
      match isograph_map_from_address ~lhs ~addr_lhs ~rhs ~addr_rhs mapping with
      | IsomorphicUpTo mapping ->
          isograph_map_from_stack ~lhs ~stack_lhs ~rhs ~stack_rhs mapping
      | NotIsomorphic ->
          NotIsomorphic )
    | _ :: _, _ :: _ | [], _ :: _ | _ :: _, [] ->
        NotIsomorphic


  let isograph_map ~lhs ~rhs mapping =
    let stack_lhs = Stack.bindings lhs.stack in
    let stack_rhs = Stack.bindings rhs.stack in
    isograph_map_from_stack ~lhs ~rhs ~stack_lhs ~stack_rhs mapping


  let is_isograph ~lhs ~rhs mapping =
    match isograph_map ~lhs ~rhs mapping with IsomorphicUpTo _ -> true | NotIsomorphic -> false
end

let ( <= ) ~lhs ~rhs =
  phys_equal lhs rhs || GraphComparison.is_isograph ~lhs ~rhs GraphComparison.empty_mapping


let pp fmt {heap; stack} =
  F.fprintf fmt "{@[<v1> heap=@[<hv>%a@];@;stack=@[<hv>%a@];@]}" Memory.pp heap Stack.pp stack


module GraphVisit : sig
  val fold :
       var_filter:(Var.t -> bool)
    -> t
    -> init:'accum
    -> f:(   'accum
          -> AbstractAddress.t
          -> Var.t
          -> Memory.Access.t list
          -> ('accum, 'final) Base.Continue_or_stop.t)
    -> finish:('accum -> 'final)
    -> AbstractAddressSet.t * 'final
  (** Generic graph traversal of the memory starting from each variable in the stack that pass
      [var_filter], in order. Returns the result of folding over every address in the graph and the
      set of addresses that have been visited before [f] returned [Stop] or all reachable addresses
      were seen. [f] is passed each address together with the variable from which the address was
      reached and the access path from that variable to the address. *)
end = struct
  open Base.Continue_or_stop

  let visit address visited =
    if AbstractAddressSet.mem address visited then `AlreadyVisited
    else
      let visited = AbstractAddressSet.add address visited in
      `NotAlreadyVisited visited


  let rec visit_address orig_var ~f rev_accesses astate address ((visited, accum) as visited_accum)
      =
    match visit address visited with
    | `AlreadyVisited ->
        Continue visited_accum
    | `NotAlreadyVisited visited -> (
      match f accum address orig_var rev_accesses with
      | Continue accum -> (
        match Memory.find_opt address astate.heap with
        | None ->
            Continue (visited, accum)
        | Some (edges, _) ->
            visit_edges orig_var ~f rev_accesses astate ~edges (visited, accum) )
      | Stop fin ->
          Stop (visited, fin) )


  and visit_edges orig_var ~f rev_accesses ~edges astate visited_accum =
    let finish visited_accum = Continue visited_accum in
    Container.fold_until edges
      ~fold:(IContainer.fold_of_pervasives_map_fold ~fold:Memory.Edges.fold)
      ~finish ~init:visited_accum ~f:(fun visited_accum (access, (address, _trace)) ->
        match visit_address orig_var ~f (access :: rev_accesses) astate address visited_accum with
        | Continue _ as cont ->
            cont
        | Stop fin ->
            Stop (Stop fin) )


  let fold ~var_filter astate ~init ~f ~finish =
    let finish (visited, accum) = (visited, finish accum) in
    let init = (AbstractAddressSet.empty, init) in
    Container.fold_until astate.stack
      ~fold:(IContainer.fold_of_pervasives_map_fold ~fold:Stack.fold) ~init ~finish
      ~f:(fun visited_accum (var, (address, _loc)) ->
        if var_filter var then visit_address var ~f [] astate address visited_accum
        else Continue visited_accum )
end

include GraphComparison

let reachable_addresses astate =
  GraphVisit.fold astate
    ~var_filter:(fun _ -> true)
    ~init:() ~finish:Fn.id
    ~f:(fun () _ _ _ -> Continue ())
  |> fst
