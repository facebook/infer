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

(** the domain *)
type astate = {heap: Memory.t; stack: Stack.t} [@@deriving compare]

let initial =
  { heap=
      Memory.empty
      (* TODO: we could record that 0 is an invalid address at this point but this makes the
         analysis go a bit overboard with the Nullptr reports. *)
  ; stack= Stack.empty }


module Domain : AbstractDomain.S with type t = astate = struct
  type t = astate

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
          Container.iter subst ~fold:AddressUF.fold_sets
            ~f:(fun ((repr : AddressUF.Repr.t), set) ->
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
    if num_iters > max_widening then prev
    else if phys_equal prev next then prev
    else join prev next


  let pp fmt {heap; stack} =
    F.fprintf fmt "{@[<v1> heap=@[<hv>%a@];@;stack=@[<hv>%a@];@]}" Memory.pp heap Stack.pp stack
end

(* {2 Access operations on the domain} *)

type actor = {access_expr: HilExp.AccessExpression.t; location: Location.t} [@@deriving compare]

module Diagnostic = struct
  type t =
    | AccessToInvalidAddress of
        { invalidated_by: Invalidation.t
        ; accessed_by: actor
        ; trace: PulseTrace.t
        ; address: AbstractAddress.t }
    | StackVariableAddressEscape of {variable: Var.t; location: Location.t}

  let get_location = function
    | AccessToInvalidAddress {accessed_by= {location}} | StackVariableAddressEscape {location} ->
        location


  let get_message = function
    | AccessToInvalidAddress {accessed_by; invalidated_by; address; trace} ->
        let pp_debug_address f =
          if Config.debug_mode then F.fprintf f " (debug: %a)" AbstractAddress.pp address
        in
        F.asprintf "`%a` accesses address %a%a past its lifetime%t" HilExp.AccessExpression.pp
          accessed_by.access_expr PulseTrace.pp_interesting_events trace Invalidation.pp
          invalidated_by pp_debug_address
    | StackVariableAddressEscape {variable} ->
        let pp_var f var =
          if Var.is_cpp_temporary var then F.pp_print_string f "C++ temporary"
          else F.fprintf f "stack variable `%a`" Var.pp var
        in
        F.asprintf "address of %a is returned by the function" pp_var variable


  let get_trace = function
    | AccessToInvalidAddress {accessed_by; invalidated_by; trace} ->
        let invalidated_by_trace =
          Invalidation.get_location invalidated_by
          |> Option.map ~f:(fun location ->
                 Errlog.make_trace_element 0 location
                   (F.asprintf "%a here" Invalidation.pp invalidated_by)
                   [] )
          |> Option.to_list
        in
        PulseTrace.make_errlog_trace ~depth:0 trace
        @ invalidated_by_trace
        @ [ Errlog.make_trace_element 0 accessed_by.location
              (F.asprintf "accessed `%a` here" HilExp.AccessExpression.pp accessed_by.access_expr)
              [] ]
    | StackVariableAddressEscape _ ->
        []


  let get_issue_type = function
    | AccessToInvalidAddress {invalidated_by} ->
        Invalidation.issue_type_of_cause invalidated_by
    | StackVariableAddressEscape _ ->
        IssueType.stack_variable_address_escape
end

type 'a access_result = ('a, Diagnostic.t) result

(** operations on the domain *)
module Operations = struct
  open Result.Monad_infix

  (** Check that the address is not known to be invalid *)
  let check_addr_access actor (address, trace) astate =
    match Memory.get_invalidation address astate.heap with
    | Some invalidated_by ->
        Error
          (Diagnostic.AccessToInvalidAddress {invalidated_by; accessed_by= actor; address; trace})
    | None ->
        Ok astate


  (** Walk the heap starting from [addr] and following [path]. Stop either at the element before last
   and return [new_addr] if [overwrite_last] is [Some new_addr], or go until the end of the path if it
   is [None]. Create more addresses into the heap as needed to follow the [path]. Check that each
   address reached is valid. *)
  let rec walk ~dereference_to_ignore actor ~on_last addr_trace path astate =
    let check_addr_access_optional actor addr_trace astate =
      match dereference_to_ignore with
      | Some 0 ->
          Ok astate
      | _ ->
          check_addr_access actor addr_trace astate
    in
    match (path, on_last) with
    | [], `Access ->
        Ok (astate, addr_trace)
    | [], `Overwrite _ ->
        L.die InternalError "Cannot overwrite last address in empty path"
    | [a], `Overwrite new_addr_trace ->
        check_addr_access_optional actor addr_trace astate
        >>| fun astate ->
        let heap = Memory.add_edge_and_back_edge (fst addr_trace) a new_addr_trace astate.heap in
        ({astate with heap}, new_addr_trace)
    | a :: path, _ -> (
        check_addr_access_optional actor addr_trace astate
        >>= fun astate ->
        let dereference_to_ignore =
          Option.map ~f:(fun index -> max 0 (index - 1)) dereference_to_ignore
        in
        let addr = fst addr_trace in
        match Memory.find_edge_opt addr a astate.heap with
        | None ->
            let addr_trace' = (AbstractAddress.mk_fresh (), []) in
            let heap = Memory.add_edge_and_back_edge addr a addr_trace' astate.heap in
            let astate = {astate with heap} in
            walk ~dereference_to_ignore actor ~on_last addr_trace' path astate
        | Some addr_trace' ->
            walk ~dereference_to_ignore actor ~on_last addr_trace' path astate )


  let write_var var new_addr_trace astate =
    let astate, var_address_of =
      match Stack.find_opt var astate.stack with
      | Some (addr, _) ->
          (astate, addr)
      | None ->
          let addr = AbstractAddress.mk_fresh () in
          let stack = Stack.add var (addr, None) astate.stack in
          ({astate with stack}, addr)
    in
    (* Update heap with var_address_of -*-> new_addr *)
    let heap =
      Memory.add_edge var_address_of HilExp.Access.Dereference new_addr_trace astate.heap
    in
    {astate with heap}


  let ends_with_addressof = function HilExp.AccessExpression.AddressOf _ -> true | _ -> false

  let last_dereference access_list =
    let rec last_dereference_inner access_list index result =
      match access_list with
      | [] ->
          result
      | HilExp.Access.Dereference :: rest ->
          last_dereference_inner rest (index + 1) (Some index)
      | _ :: rest ->
          last_dereference_inner rest (index + 1) result
    in
    last_dereference_inner access_list 0 None


  let rec to_accesses location access_expr astate =
    let exception Failed_fold of Diagnostic.t in
    try
      HilExp.AccessExpression.to_accesses_fold access_expr ~init:astate
        ~f_array_offset:(fun astate hil_exp_opt ->
          match hil_exp_opt with
          | None ->
              (astate, AbstractAddress.mk_fresh ())
          | Some hil_exp -> (
            match eval_hil_exp location hil_exp astate with
            | Ok result ->
                result
            | Error diag ->
                raise (Failed_fold diag) ) )
      |> Result.return
    with Failed_fold diag -> Error diag


  (** add addresses to the state to give an address to the destination of the given access path *)
  and walk_access_expr ~on_last astate access_expr location =
    to_accesses location access_expr astate
    >>= fun (astate, (access_var, _), access_list) ->
    let dereference_to_ignore =
      if ends_with_addressof access_expr then last_dereference access_list else None
    in
    if Config.write_html then
      L.d_printfln "Accessing %a -> [%a]" Var.pp access_var
        (Pp.seq ~sep:"," Memory.Access.pp)
        access_list ;
    match (on_last, access_list) with
    | `Overwrite new_addr_trace, [] ->
        Ok (write_var access_var new_addr_trace astate, new_addr_trace)
    | `Access, _ | `Overwrite _, _ :: _ -> (
        let astate, base_addr_trace =
          match Stack.find_opt access_var astate.stack with
          | Some (addr, init_loc_opt) ->
              let trace =
                Option.value_map init_loc_opt ~default:[] ~f:(fun init_loc ->
                    [PulseTrace.VariableDeclaration init_loc] )
              in
              (astate, (addr, trace))
          | None ->
              let addr = AbstractAddress.mk_fresh () in
              let stack = Stack.add access_var (addr, None) astate.stack in
              ({astate with stack}, (addr, []))
        in
        match access_list with
        | [HilExp.Access.TakeAddress] ->
            Ok (astate, base_addr_trace)
        | _ ->
            let actor = {access_expr; location} in
            walk ~dereference_to_ignore actor ~on_last base_addr_trace
              (HilExp.Access.Dereference :: access_list)
              astate )


  (** Use the stack and heap to walk the access path represented by the given expression down to an
    abstract address representing what the expression points to.

    Return an error state if it traverses some known invalid address or if the end destination is
    known to be invalid. *)
  and materialize_address astate access_expr = walk_access_expr ~on_last:`Access astate access_expr

  and read location access_expr astate =
    materialize_address astate access_expr location
    >>= fun (astate, addr_trace) ->
    let actor = {access_expr; location} in
    check_addr_access actor addr_trace astate >>| fun astate -> (astate, addr_trace)


  and read_all location access_exprs astate =
    List.fold_result access_exprs ~init:astate ~f:(fun astate access_expr ->
        read location access_expr astate >>| fst )


  and eval_hil_exp location (hil_exp : HilExp.t) astate =
    match hil_exp with
    | AccessExpression access_expr ->
        read location access_expr astate >>| fun (astate, (addr, _)) -> (astate, addr)
    | _ ->
        read_all location (HilExp.get_access_exprs hil_exp) astate
        >>| fun astate -> (astate, AbstractAddress.mk_fresh ())


  (** Use the stack and heap to walk the access path represented by the given expression down to an
    abstract address representing what the expression points to, and replace that with the given
    address.

    Return an error state if it traverses some known invalid address. *)
  let overwrite_address astate access_expr new_addr_trace =
    walk_access_expr ~on_last:(`Overwrite new_addr_trace) astate access_expr


  (** Add the given address to the set of know invalid addresses. *)
  let mark_invalid actor address astate =
    {astate with heap= Memory.invalidate address actor astate.heap}


  let havoc_var trace var astate = write_var var (AbstractAddress.mk_fresh (), trace) astate

  let havoc trace location (access_expr : HilExp.AccessExpression.t) astate =
    overwrite_address astate access_expr (AbstractAddress.mk_fresh (), trace) location >>| fst


  let write location access_expr addr astate =
    overwrite_address astate access_expr addr location >>| fun (astate, _) -> astate


  let invalidate cause location access_expr astate =
    materialize_address astate access_expr location
    >>= fun (astate, addr_trace) ->
    check_addr_access {access_expr; location} addr_trace astate
    >>| mark_invalid cause (fst addr_trace)


  let invalidate_array_elements cause location access_expr astate =
    materialize_address astate access_expr location
    >>= fun (astate, addr_trace) ->
    check_addr_access {access_expr; location} addr_trace astate
    >>| fun astate ->
    match Memory.find_opt (fst addr_trace) astate.heap with
    | None ->
        astate
    | Some (edges, _) ->
        Memory.Edges.fold
          (fun access (dest_addr, _) astate ->
            match (access : Memory.Access.t) with
            | ArrayAccess _ ->
                mark_invalid cause dest_addr astate
            | _ ->
                astate )
          edges astate


  let check_address_of_local_variable proc_desc address astate =
    let proc_location = Procdesc.get_loc proc_desc in
    let proc_name = Procdesc.get_proc_name proc_desc in
    let check_address_of_cpp_temporary () =
      Memory.find_opt address astate.heap
      |> Option.fold_result ~init:() ~f:(fun () (_, attrs) ->
             Container.fold_result ~fold:(IContainer.fold_of_pervasives_fold ~fold:Attributes.fold)
               attrs ~init:() ~f:(fun () attr ->
                 match attr with
                 | Attribute.AddressOfCppTemporary (variable, location_opt) ->
                     let location = Option.value ~default:proc_location location_opt in
                     Error (Diagnostic.StackVariableAddressEscape {variable; location})
                 | _ ->
                     Ok () ) )
    in
    let check_address_of_stack_variable () =
      Container.fold_result ~fold:(IContainer.fold_of_pervasives_map_fold ~fold:Stack.fold)
        astate.stack ~init:() ~f:(fun () (variable, (var_address, init_location)) ->
          if
            AbstractAddress.equal var_address address
            && ( Var.is_cpp_temporary variable
               || Var.is_local_to_procedure proc_name variable
                  && not (Procdesc.is_captured_var proc_desc variable) )
          then
            let location = Option.value ~default:proc_location init_location in
            Error (Diagnostic.StackVariableAddressEscape {variable; location})
          else Ok () )
    in
    check_address_of_cpp_temporary () >>= check_address_of_stack_variable >>| fun () -> astate


  let mark_address_of_cpp_temporary location variable address heap =
    Memory.add_attributes address
      (Attributes.singleton (AddressOfCppTemporary (variable, location)))
      heap


  let remove_vars vars astate =
    let heap =
      List.fold vars ~init:astate.heap ~f:(fun heap var ->
          match Stack.find_opt var astate.stack with
          | Some (address, location) when Var.is_cpp_temporary var ->
              (* TODO: it would be good to record the location of the temporary creation in the
                 stack and save it here in the attribute for reporting *)
              mark_address_of_cpp_temporary location var address heap
          | _ ->
              heap )
    in
    let stack = List.fold ~f:(fun stack var -> Stack.remove var stack) ~init:astate.stack vars in
    if phys_equal stack astate.stack && phys_equal heap astate.heap then astate else {stack; heap}


  let record_var_decl_location location var astate =
    let addr =
      match Stack.find_opt var astate.stack with
      | Some (addr, _) ->
          addr
      | None ->
          AbstractAddress.mk_fresh ()
    in
    let stack = Stack.add var (addr, Some location) astate.stack in
    {astate with stack}
end

module Closures = struct
  open Result.Monad_infix

  let check_captured_addresses location lambda addr astate =
    match Memory.find_opt addr astate.heap with
    | None ->
        Ok astate
    | Some (_, attributes) ->
        IContainer.iter_result ~fold:(IContainer.fold_of_pervasives_fold ~fold:Attributes.fold)
          attributes ~f:(function
          | Attribute.Closure (_, captured) ->
              IContainer.iter_result ~fold:List.fold captured ~f:(fun addr_trace ->
                  Operations.check_addr_access {access_expr= lambda; location} addr_trace astate
                  >>| fun _ -> () )
          | _ ->
              Ok () )
        >>| fun () -> astate


  let write location access_expr pname captured astate =
    let closure_addr = AbstractAddress.mk_fresh () in
    Operations.write location access_expr
      (closure_addr, [PulseTrace.Assignment {lhs= access_expr; location}])
      astate
    >>| fun astate ->
    { astate with
      heap=
        Memory.add_attributes closure_addr
          (Attributes.singleton (Closure (pname, captured)))
          astate.heap }


  let record location access_expr pname captured astate =
    List.fold_result captured ~init:(astate, [])
      ~f:(fun ((astate, captured) as result) (captured_as, captured_exp) ->
        match captured_exp with
        | HilExp.AccessExpression (AddressOf access_expr as captured_access_expr) ->
            Operations.read location access_expr astate
            >>= fun (astate, (address, trace)) ->
            let new_trace =
              PulseTrace.Capture {captured_as; captured= captured_access_expr; location} :: trace
            in
            Ok (astate, (address, new_trace) :: captured)
        | _ ->
            Ok result )
    >>= fun (astate, captured_addresses) ->
    write location access_expr pname captured_addresses astate
end

module StdVector = struct
  open Result.Monad_infix

  let is_reserved location vector_access_expr astate =
    Operations.read location vector_access_expr astate
    >>| fun (astate, (addr, _)) -> (astate, Memory.is_std_vector_reserved addr astate.heap)


  let mark_reserved location vector_access_expr astate =
    Operations.read location vector_access_expr astate
    >>| fun (astate, (addr, _)) -> {astate with heap= Memory.std_vector_reserve addr astate.heap}
end

include Domain

let compare = compare_astate

include Operations
