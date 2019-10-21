(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging
open PulseBasicInterface

(* {2 Abstract domain description } *)

module ValueHistory = struct
  type event =
    | Assignment of Location.t
    | Call of {f: CallEvent.t; location: Location.t; in_call: t}
    | Capture of {captured_as: Pvar.t; location: Location.t}
    | CppTemporaryCreated of Location.t
    | FormalDeclared of Pvar.t * Location.t
    | VariableAccessed of Pvar.t * Location.t
    | VariableDeclared of Pvar.t * Location.t

  and t = event list [@@deriving compare]

  let pp_event_no_location fmt event =
    let pp_pvar fmt pvar =
      if Pvar.is_global pvar then
        F.fprintf fmt "global variable `%a`" Pvar.pp_value_non_verbose pvar
      else F.fprintf fmt "variable `%a`" Pvar.pp_value_non_verbose pvar
    in
    match event with
    | Assignment _ ->
        F.pp_print_string fmt "assigned"
    | Call {f; location= _} ->
        F.fprintf fmt "passed as argument to %a" CallEvent.pp f
    | Capture {captured_as; location= _} ->
        F.fprintf fmt "value captured as `%a`" Pvar.pp_value_non_verbose captured_as
    | CppTemporaryCreated _ ->
        F.pp_print_string fmt "C++ temporary created"
    | FormalDeclared (pvar, _) ->
        let pp_proc fmt pvar =
          Pvar.get_declaring_function pvar
          |> Option.iter ~f:(fun proc_name -> F.fprintf fmt " of %a" Typ.Procname.pp proc_name)
        in
        F.fprintf fmt "parameter `%a`%a" Pvar.pp_value_non_verbose pvar pp_proc pvar
    | VariableAccessed (pvar, _) ->
        F.fprintf fmt "%a accessed here" pp_pvar pvar
    | VariableDeclared (pvar, _) ->
        F.fprintf fmt "%a declared here" pp_pvar pvar


  let location_of_event = function
    | Assignment location
    | Call {location}
    | Capture {location}
    | CppTemporaryCreated location
    | FormalDeclared (_, location)
    | VariableAccessed (_, location)
    | VariableDeclared (_, location) ->
        location


  let pp_event fmt event =
    F.fprintf fmt "%a at %a" pp_event_no_location event Location.pp_line (location_of_event event)


  let pp fmt history =
    let rec pp_aux fmt = function
      | [] ->
          ()
      | (Call {f; in_call} as event) :: tail ->
          F.fprintf fmt "%a@;" pp_event event ;
          F.fprintf fmt "[%a]@;" pp_aux (List.rev in_call) ;
          if not (List.is_empty tail) then F.fprintf fmt "return from call to %a@;" CallEvent.pp f ;
          pp_aux fmt tail
      | event :: tail ->
          F.fprintf fmt "%a@;" pp_event event ;
          pp_aux fmt tail
    in
    F.fprintf fmt "@[%a@]" pp_aux (List.rev history)


  let add_event_to_errlog ~nesting event errlog =
    let location = location_of_event event in
    let description = F.asprintf "%a" pp_event_no_location event in
    let tags = [] in
    Errlog.make_trace_element nesting location description tags :: errlog


  let add_returned_from_call_to_errlog ~nesting f location errlog =
    let description = F.asprintf "return from call to %a" CallEvent.pp f in
    let tags = [] in
    Errlog.make_trace_element nesting location description tags :: errlog


  let add_to_errlog ~nesting history errlog =
    let rec add_to_errlog_aux ~nesting history errlog =
      match history with
      | [] ->
          errlog
      | (Call {f; location; in_call} as event) :: tail ->
          add_to_errlog_aux ~nesting tail
          @@ add_event_to_errlog ~nesting event
          @@ add_to_errlog_aux ~nesting:(nesting + 1) in_call
          @@ add_returned_from_call_to_errlog ~nesting f location
          @@ errlog
      | event :: tail ->
          add_to_errlog_aux ~nesting tail @@ add_event_to_errlog ~nesting event @@ errlog
    in
    add_to_errlog_aux ~nesting history errlog
end

module Trace = struct
  type 'a t =
    | Immediate of {imm: 'a; location: Location.t; history: ValueHistory.t}
    | ViaCall of {f: CallEvent.t; location: Location.t; history: ValueHistory.t; in_call: 'a t}
  [@@deriving compare]

  (** only for use in the attributes' [*_rank] functions *)
  let mk_dummy imm = Immediate {imm; location= Location.dummy; history= []}

  let rec get_immediate = function
    | Immediate {imm; _} ->
        imm
    | ViaCall {in_call; _} ->
        get_immediate in_call


  let get_outer_location = function Immediate {location; _} | ViaCall {location; _} -> location

  let get_history = function Immediate {history; _} | ViaCall {history; _} -> history

  let get_start_location trace =
    match get_history trace |> List.last with
    | Some event ->
        ValueHistory.location_of_event event
    | None ->
        get_outer_location trace


  let rec pp pp_immediate fmt = function
    | Immediate {imm; location= _; history} ->
        if Config.debug_level_analysis < 3 then pp_immediate fmt imm
        else F.fprintf fmt "%a::%a" ValueHistory.pp history pp_immediate imm
    | ViaCall {f; location= _; history; in_call} ->
        if Config.debug_level_analysis < 3 then pp pp_immediate fmt in_call
        else
          F.fprintf fmt "%a::%a[%a]" ValueHistory.pp history CallEvent.pp f (pp pp_immediate)
            in_call


  let rec add_to_errlog ~nesting pp_immediate trace errlog =
    match trace with
    | Immediate {imm; location; history} ->
        ValueHistory.add_to_errlog ~nesting history
        @@ Errlog.make_trace_element nesting location (F.asprintf "%a" pp_immediate imm) []
           :: errlog
    | ViaCall {f; location; in_call; history} ->
        ValueHistory.add_to_errlog ~nesting history
        @@ (fun errlog ->
             Errlog.make_trace_element nesting location
               (F.asprintf "when calling %a here" CallEvent.pp f)
               []
             :: errlog )
        @@ add_to_errlog ~nesting:(nesting + 1) pp_immediate in_call
        @@ errlog
end

module Attribute = struct
  (** Make sure we don't depend on [AbstractAddress] to avoid attributes depending on
      addresses. Otherwise they become a pain to handle when comparing memory states. *)
  include struct
    [@@@warning "-60"]

    module AbstractAddress = struct end
  end

  type t =
    | AddressOfCppTemporary of Var.t * ValueHistory.t
    | AddressOfStackVariable of Var.t * Location.t * ValueHistory.t
    | Closure of Typ.Procname.t
    | Constant of Const.t
    | Invalid of Invalidation.t Trace.t
    | MustBeValid of unit Trace.t
    | StdVectorReserve
    | WrittenTo of unit Trace.t
  [@@deriving compare, variants]

  let equal = [%compare.equal: t]

  let to_rank = Variants.to_rank

  let closure_rank = Variants.to_rank (Closure (Typ.Procname.from_string_c_fun ""))

  let written_to_rank = Variants.to_rank (WrittenTo (Trace.mk_dummy ()))

  let address_of_stack_variable_rank =
    let pname = Typ.Procname.from_string_c_fun "" in
    let var = Var.of_pvar (Pvar.mk (Mangled.from_string "") pname) in
    let location = Location.dummy in
    Variants.to_rank (AddressOfStackVariable (var, location, []))


  let invalid_rank = Variants.to_rank (Invalid (Trace.mk_dummy Invalidation.Nullptr))

  let must_be_valid_rank = Variants.to_rank (MustBeValid (Trace.mk_dummy ()))

  let std_vector_reserve_rank = Variants.to_rank StdVectorReserve

  let const_rank = Variants.to_rank (Constant (Const.Cint IntLit.zero))

  let pp f attribute =
    let pp_string_if_debug string fmt () =
      if Config.debug_level_analysis >= 3 then F.pp_print_string fmt string
    in
    match attribute with
    | AddressOfCppTemporary (var, history) ->
        F.fprintf f "t&%a (%a)" Var.pp var ValueHistory.pp history
    | AddressOfStackVariable (var, location, history) ->
        F.fprintf f "s&%a (%a) at %a" Var.pp var ValueHistory.pp history Location.pp location
    | Closure pname ->
        Typ.Procname.pp f pname
    | Constant c ->
        F.fprintf f "=%a" (Const.pp Pp.text) c
    | Invalid invalidation ->
        F.fprintf f "Invalid %a" (Trace.pp Invalidation.pp) invalidation
    | MustBeValid action ->
        F.fprintf f "MustBeValid %a" (Trace.pp (pp_string_if_debug "access")) action
    | StdVectorReserve ->
        F.pp_print_string f "std::vector::reserve()"
    | WrittenTo action ->
        F.fprintf f "WrittenTo %a" (Trace.pp (pp_string_if_debug "mutation")) action
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


  let get_written_to attrs =
    Set.find_rank attrs Attribute.written_to_rank
    |> Option.map ~f:(fun attr ->
           let[@warning "-8"] (Attribute.WrittenTo action) = attr in
           action )


  let get_closure_proc_name attrs =
    Set.find_rank attrs Attribute.closure_rank
    |> Option.map ~f:(fun attr ->
           let[@warning "-8"] (Attribute.Closure proc_name) = attr in
           proc_name )


  let get_address_of_stack_variable attrs =
    Set.find_rank attrs Attribute.address_of_stack_variable_rank
    |> Option.map ~f:(fun attr ->
           let[@warning "-8"] (Attribute.AddressOfStackVariable (var, loc, history)) = attr in
           (var, loc, history) )


  let is_std_vector_reserved attrs =
    Set.find_rank attrs Attribute.std_vector_reserve_rank |> Option.is_some


  let is_modified attrs =
    Option.is_some (Set.find_rank attrs Attribute.written_to_rank)
    || Option.is_some (Set.find_rank attrs Attribute.invalid_rank)


  let get_constant attrs =
    Set.find_rank attrs Attribute.const_rank
    |> Option.map ~f:(fun attr ->
           let[@warning "-8"] (Attribute.Constant c) = attr in
           c )


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


  let pp f l = F.fprintf f "v%d" l

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

  val pp_edges : F.formatter -> edges -> unit

  type cell = edges * Attributes.t

  type t

  val empty : t

  val filter : (AbstractAddress.t -> bool) -> t -> t

  val filter_heap : (AbstractAddress.t -> edges -> bool) -> t -> t

  val find_opt : AbstractAddress.t -> t -> cell option

  val fold_attrs : (AbstractAddress.t -> Attributes.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc

  val set_attrs : AbstractAddress.t -> Attributes.t -> t -> t

  val set_edges : AbstractAddress.t -> edges -> t -> t

  val set_cell : AbstractAddress.t -> cell -> t -> t

  val find_edges_opt : AbstractAddress.t -> t -> edges option

  val mem_edges : AbstractAddress.t -> t -> bool

  val pp_heap : F.formatter -> t -> unit

  val pp_attributes : F.formatter -> t -> unit

  val register_address : AbstractAddress.t -> t -> t

  val add_edge : AbstractAddress.t -> Access.t -> AddrTracePair.t -> t -> t

  val find_edge_opt : AbstractAddress.t -> Access.t -> t -> AddrTracePair.t option

  val add_attribute : AbstractAddress.t -> Attribute.t -> t -> t

  val invalidate : AbstractAddress.t * ValueHistory.t -> Invalidation.t -> Location.t -> t -> t

  val check_valid : AbstractAddress.t -> t -> (unit, Invalidation.t Trace.t) result

  val get_closure_proc_name : AbstractAddress.t -> t -> Typ.Procname.t option

  val get_constant : AbstractAddress.t -> t -> Const.t option

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

  let pp_edges = Edges.pp ~pp_value:AddrTracePair.pp

  type cell = edges * Attributes.t

  module Graph = PrettyPrintable.MakePPMap (AbstractAddress)

  type t = edges Graph.t * Attributes.t Graph.t

  let pp_heap fmt (heap, _) = Graph.pp ~pp_value:pp_edges fmt heap

  let pp_attributes fmt (_, attrs) = Graph.pp ~pp_value:Attributes.pp fmt attrs

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


  let add_attribute addr attribute memory =
    match Graph.find_opt addr (snd memory) with
    | None ->
        (fst memory, Graph.add addr (Attributes.singleton attribute) (snd memory))
    | Some old_attrs ->
        let new_attrs = Attributes.add old_attrs attribute in
        (fst memory, Graph.add addr new_attrs (snd memory))


  let invalidate (address, history) invalid location memory =
    let invalidation = Trace.Immediate {imm= invalid; location; history} in
    add_attribute address (Attribute.Invalid invalidation) memory


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


  let get_constant address memory =
    Graph.find_opt address (snd memory)
    |> Option.bind ~f:(fun attributes -> Attributes.get_constant attributes)


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


  let fold_attrs f memory init = Graph.fold f (snd memory) init

  let set_attrs addr attrs memory = (fst memory, Graph.add addr attrs (snd memory))

  let set_edges addr edges memory = (Graph.add addr edges (fst memory), snd memory)

  let set_cell addr (edges, attrs) memory =
    (Graph.add addr edges (fst memory), Graph.add addr attrs (snd memory))


  let filter f memory =
    let heap = Graph.filter (fun address _ -> f address) (fst memory) in
    let attrs = Graph.filter (fun address _ -> f address) (snd memory) in
    if phys_equal heap (fst memory) && phys_equal attrs (snd memory) then memory else (heap, attrs)


  let filter_heap f memory =
    let heap = Graph.filter f (fst memory) in
    if phys_equal heap (fst memory) then memory else (heap, snd memory)


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
  F.fprintf fmt "{@[<v1> roots=@[<hv>%a@];@;mem  =@[<hv>%a@];@;attrs=@[<hv>%a@];@]}" Stack.pp stack
    Memory.pp_heap heap Memory.pp_attributes heap


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
