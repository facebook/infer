(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module IRAttributes = Attributes
open PulseBasicInterface
module BaseMemory = PulseBaseMemory
module DecompilerExpr = PulseDecompilerExpr

(** Unnecessary copies are tracked in two places:

    - In non-disjunctive domain (here), we keep track of a map from (copy var, source address
      (optional)) -> Modified/ Copied heap snapshot
    - In attributes ({!PulseBaseAddressAttributes}) of each disjunct where we keep track of
    - source address -> copy var ({!CopiedVar})
    - copy address -> source address ({!SourceOriginOfCopy})

    In order to determine if a source/copy variable is modified,

    - When a source variable goes out of scope, we first lookup the CopiedVar from the attributes of
      source address and then lookup the snapshot from the non-disjunctive domain
    - When a copy variable goes out of scope, we first lookup the corresponding source address which
      we can then use to loopkup the snapshot from the non-disjunctive domain

    Then we compare the snapshot heap with the current heap (see {!PulseNonDisjunctiveOperations}.) *)
open AbstractDomain.Types

module MakeDomainFromTotalOrder (M : AbstractDomain.Comparable) = struct
  type t = M.t

  let leq ~lhs ~rhs = M.leq ~lhs ~rhs

  (* NOTE: This [join] definition is incorrect when given arguments do not have a total order. Make sure
     that the function is always applied to the arguments that have a total order. *)
  let join x y = if leq ~lhs:x ~rhs:y then y else x

  let widen ~prev ~next ~num_iters:_ = join prev next

  let pp fmt = M.pp fmt
end

type copy_spec_t =
  | Copied of
      { source_typ: Typ.t option
      ; source_opt: DecompilerExpr.source_expr option
      ; location: Location.t
      ; copied_location: (Procname.t * Location.t) option
      ; heap: BaseMemory.t
      ; from: Attribute.CopyOrigin.t
      ; timestamp: Timestamp.t }
  | Modified of
      { source_typ: Typ.t option
      ; source_opt: DecompilerExpr.source_expr option
      ; location: Location.t
      ; copied_location: (Procname.t * Location.t) option
      ; from: Attribute.CopyOrigin.t
      ; copied_timestamp: Timestamp.t }
[@@deriving equal]

module CopySpec = MakeDomainFromTotalOrder (struct
  type t = copy_spec_t [@@deriving equal]

  let leq ~lhs ~rhs =
    match (lhs, rhs) with
    | Copied _, _ ->
        true
    | Modified _, Copied _ ->
        false
    | Modified _, Modified _ ->
        true


  let pp fmt = function
    | Copied {source_typ; heap; location; from; timestamp} ->
        Format.fprintf fmt "@[%a (value of type %a) at %a@ with heap= %a@ (timestamp: %d)@]"
          Attribute.CopyOrigin.pp from
          (Pp.option (Typ.pp Pp.text))
          source_typ Location.pp location BaseMemory.pp heap
          (timestamp :> int)
    | Modified _ ->
        Format.fprintf fmt "modified"
end)

module CopyVar = struct
  type t = {copied_into: Attribute.CopiedInto.t; source_addr_opt: AbstractValue.t option}
  [@@deriving compare]

  let pp fmt {copied_into; source_addr_opt} =
    match source_addr_opt with
    | Some source_addr ->
        Format.fprintf fmt "%a copied with source_addr %a" Attribute.CopiedInto.pp copied_into
          AbstractValue.pp source_addr
    | None ->
        Format.fprintf fmt "%a copied" Attribute.CopiedInto.pp copied_into
end

type parameter_spec_t =
  | Unmodified of {typ: Typ.t; location: Location.t; heap: BaseMemory.t}
  | Modified
[@@deriving equal]

module ParameterSpec = MakeDomainFromTotalOrder (struct
  type t = parameter_spec_t [@@deriving equal]

  let leq ~lhs ~rhs =
    match (lhs, rhs) with
    | Unmodified _, _ ->
        true
    | Modified, Unmodified _ ->
        false
    | Modified, Modified ->
        true


  let pp fmt = function
    | Unmodified {typ; heap; location} ->
        Format.fprintf fmt "@[const refable (value of type %a) at %a@ with heap= %a@]"
          (Typ.pp Pp.text) typ Location.pp location BaseMemory.pp heap
    | Modified ->
        Format.fprintf fmt "modified"
end)

module ParameterVar = struct
  type t = Var.t [@@deriving compare]

  let pp fmt = Var.pp fmt
end

module DestructorChecked = AbstractDomain.FiniteSet (Var)

module Captured = struct
  include
    AbstractDomain.FiniteMultiMap
      (struct
        include Pvar

        let pp = Pvar.pp Pp.text
      end)
      (struct
        type t = CapturedVar.capture_mode [@@deriving compare]

        let pp f x = F.pp_print_string f (CapturedVar.string_of_capture_mode x)
      end)

  let mem_var var x =
    match (var : Var.t) with ProgramVar pvar -> mem pvar x | LogicalVar _ -> false


  let is_captured_by_ref var x =
    match (var : Var.t) with
    | ProgramVar pvar ->
        get_all pvar x
        |> List.exists ~f:(fun mode -> CapturedVar.equal_capture_mode mode ByReference)
    | LogicalVar _ ->
        false
end

module CopyMap = struct
  include AbstractDomain.Map (CopyVar) (CopySpec)

  let remove_var var x =
    filter
      (fun {copied_into} _ ->
        match copied_into with
        | IntoVar {copied_var} | IntoIntermediate {copied_var} ->
            not (Var.equal var copied_var)
        | IntoField _ ->
            true )
      x
end

module ParameterMap = AbstractDomain.Map (ParameterVar) (ParameterSpec)
module Locked = AbstractDomain.BooleanOr
module TrackedLoc = AbstractDomain.FiniteMultiMap (Location) (Timestamp)

(** The value domain [Val] is conceptually a collection, i.e. a set or a map, that has a bottom
    value and an element can be added to it, e.g. using [f_add_v] in the [add] function. *)
module MakeMapToCollection
    (Key : PrettyPrintable.PrintableOrderedType)
    (Val : AbstractDomain.WithBottom) =
struct
  include AbstractDomain.Map (Key) (Val)

  let value_with_bottom = Option.value ~default:Val.bottom

  let find k x = find_opt k x |> value_with_bottom

  let add k f_add_v x = update k (fun v_opt -> Some (f_add_v (value_with_bottom v_opt))) x
end

module MakeSetWithTrackedLoc (Key : PrettyPrintable.PrintableOrderedType) = struct
  include MakeMapToCollection (Key) (TrackedLoc)

  let add k loc timestamp map = add k (TrackedLoc.add loc timestamp) map
end

module Loads = struct
  module IdentToVars = AbstractDomain.FiniteMultiMap (Ident) (Var)
  module LoadedVars = MakeSetWithTrackedLoc (Var)
  include AbstractDomain.PairWithBottom (IdentToVars) (LoadedVars)

  let add loc timestamp ident var (ident_to_vars, loaded_vars) =
    (IdentToVars.add ident var ident_to_vars, LoadedVars.add var loc timestamp loaded_vars)


  let get_all ident (ident_to_vars, _) = IdentToVars.get_all ident ident_to_vars

  let is_loaded var (_, loaded_vars) = LoadedVars.mem var loaded_vars

  let get_loaded_locations var (_, loaded_vars) = LoadedVars.find var loaded_vars
end

module PVar = struct
  type t = Pvar.t [@@deriving compare]

  let pp = Pvar.pp Pp.text
end

module Stores = MakeSetWithTrackedLoc (PVar)

module CalleeWithUnknown = struct
  type t = V of {copy_tgt: Exp.t option; callee: Procname.t} | Unknown [@@deriving compare]

  let pp_copy_tgt f copy_tgt = Option.iter copy_tgt ~f:(fun tgt -> F.fprintf f "(%a)" Exp.pp tgt)

  let pp f = function
    | V {copy_tgt; callee} ->
        F.fprintf f "%a%a" Procname.pp callee pp_copy_tgt copy_tgt
    | Unknown ->
        F.pp_print_string f "unknown"


  let is_copy_to_field_or_global = function
    | V {copy_tgt= Some (Lindex _ | Lfield _); callee} ->
        Option.exists (IRAttributes.load callee) ~f:(fun attrs ->
            attrs.ProcAttributes.is_cpp_copy_assignment || attrs.ProcAttributes.is_cpp_copy_ctor )
    | V {copy_tgt= Some (Lvar pvar); callee} ->
        Pvar.is_global pvar
        && Option.exists (IRAttributes.load callee) ~f:(fun attrs ->
               attrs.ProcAttributes.is_cpp_copy_assignment )
    | V _ ->
        false
    | Unknown ->
        true


  let is_moved = function
    | V {callee} ->
        Option.exists (IRAttributes.load callee) ~f:(fun attrs ->
            attrs.ProcAttributes.is_cpp_move_ctor )
    | Unknown ->
        true
end

module CalleesWithLoc = struct
  include MakeSetWithTrackedLoc (CalleeWithUnknown)

  let is_copy_to_field_or_global x =
    exists (fun callee _ -> CalleeWithUnknown.is_copy_to_field_or_global callee) x


  let is_moved x = exists (fun callee _ -> CalleeWithUnknown.is_moved callee) x
end

module PassedTo = struct
  include MakeMapToCollection (Var) (CalleesWithLoc)

  let add var callee loc timestamp x = add var (CalleesWithLoc.add callee loc timestamp) x

  let is_copied_to_field_or_global var x =
    find_opt var x |> Option.exists ~f:CalleesWithLoc.is_copy_to_field_or_global


  let is_moved var x = find_opt var x |> Option.exists ~f:CalleesWithLoc.is_moved
end

module DroppedTransitiveAccesses = AbstractDomain.FiniteSetOfPPSet (Trace.Set)

type elt =
  { copy_map: CopyMap.t
  ; parameter_map: ParameterMap.t
  ; destructor_checked: DestructorChecked.t
  ; dropped_transitive_accesses: DroppedTransitiveAccesses.t
  ; captured: Captured.t
  ; locked: Locked.t
  ; loads: Loads.t
  ; stores: Stores.t
  ; passed_to: PassedTo.t }
[@@deriving abstract_domain]

module Elt = struct
  type t = elt [@@deriving abstract_domain]

  let pp fmt {copy_map; parameter_map; destructor_checked; captured; locked; loads; passed_to} =
    F.fprintf fmt
      "@[@[copy map: %a@],@ @[parameter map: %a@],@ @[destructor checked: %a@],@ @[captured: \
       %a@],@ @[locked: %a@],@ @[loads: %a@],@ @[passed to: %a@]@]"
      CopyMap.pp copy_map ParameterMap.pp parameter_map DestructorChecked.pp destructor_checked
      Captured.pp captured Locked.pp locked Loads.pp loads PassedTo.pp passed_to
end

include AbstractDomain.TopLifted (Elt)

let bottom =
  NonTop
    { copy_map= CopyMap.empty
    ; parameter_map= ParameterMap.empty
    ; destructor_checked= DestructorChecked.empty
    ; dropped_transitive_accesses= DroppedTransitiveAccesses.empty
    ; captured= Captured.bottom
    ; locked= Locked.bottom
    ; loads= Loads.bottom
    ; stores= Stores.bottom
    ; passed_to= PassedTo.bottom }


let is_bottom = function
  | Top ->
      false
  | NonTop {copy_map; parameter_map; destructor_checked; captured; locked; loads; passed_to} ->
      CopyMap.is_bottom copy_map
      && ParameterMap.is_bottom parameter_map
      && DestructorChecked.is_bottom destructor_checked
      && Captured.is_bottom captured && Locked.is_bottom locked && Loads.is_bottom loads
      && PassedTo.is_bottom passed_to


let join lhs rhs =
  if phys_equal lhs bottom then rhs else if phys_equal rhs bottom then lhs else join lhs rhs


let mark_copy_as_modified_elt ~is_modified ~copied_into ~source_addr_opt ({copy_map} as astate_n) =
  let copy_var = CopyVar.{copied_into; source_addr_opt} in
  let copy_map =
    match CopyMap.find_opt copy_var copy_map with
    | Some
        (Copied
          { source_typ
          ; source_opt
          ; from
          ; copied_location
          ; location
          ; heap= copy_heap
          ; timestamp= copied_timestamp } )
      when is_modified copy_heap copied_timestamp ->
        Logging.d_printfln_escaped "Copy/source modified!" ;
        let modified : copy_spec_t =
          Modified {source_typ; source_opt; location; copied_location; from; copied_timestamp}
        in
        CopyMap.add copy_var modified copy_map
    | _ ->
        copy_map
  in
  {astate_n with copy_map}


let mark_copy_as_modified ~is_modified ~copied_into ~source_addr_opt =
  map (mark_copy_as_modified_elt ~is_modified ~copied_into ~source_addr_opt)


let mark_parameter_as_modified_elt ~is_modified ~var ({parameter_map} as astate_n) =
  let parameter_map =
    match ParameterMap.find_opt var parameter_map with
    | Some (Unmodified {heap= copy_heap}) when is_modified copy_heap Timestamp.t0 ->
        Logging.d_printfln_escaped "Parameter %a modified!" Var.pp var ;
        ParameterMap.add var Modified parameter_map
    | _ ->
        parameter_map
  in
  {astate_n with parameter_map}


let mark_parameter_as_modified ~is_modified ~var =
  map (mark_parameter_as_modified_elt ~is_modified ~var)


let checked_via_dtor_elt var astate_n =
  {astate_n with destructor_checked= DestructorChecked.add var astate_n.destructor_checked}


let checked_via_dtor var = map (checked_via_dtor_elt var)

module CopiedSet = PrettyPrintable.MakePPSet (Attribute.CopiedInto)

let is_never_used_after_copy_into_intermediate_or_field pvar (copied_timestamp : Timestamp.t)
    astate_n =
  match astate_n with
  | Top ->
      false
  | NonTop {passed_to; loads; stores} ->
      let is_after_copy =
        TrackedLoc.exists (fun _ timestamp -> (copied_timestamp :> int) < (timestamp :> int))
      in
      let source_var = Var.of_pvar pvar in
      let is_passed_to_non_destructor_after_copy =
        PassedTo.find source_var passed_to
        |> CalleesWithLoc.exists (fun callee tracked_loc ->
               match callee with
               | V {callee} ->
                   is_after_copy tracked_loc && not (Procname.is_destructor callee)
               | Unknown ->
                   false )
      in
      let is_loaded_after_copy = Loads.get_loaded_locations source_var loads |> is_after_copy in
      let is_stored_after_copy = Stores.find pvar stores |> is_after_copy in
      not (is_loaded_after_copy || is_stored_after_copy || is_passed_to_non_destructor_after_copy)


let is_lvalue_ref_param ~ref_formals pvar =
  Option.exists (List.Assoc.find ref_formals ~equal:Pvar.equal pvar) ~f:(fun typ ->
      not (Typ.is_rvalue_reference typ) )


let get_copied ~ref_formals ~ptr_formals astate_n =
  match astate_n with
  | Top ->
      []
  | NonTop {copy_map; captured} ->
      let modified =
        CopyMap.fold
          (fun CopyVar.{copied_into} (copy_spec : CopySpec.t) acc ->
            match copy_spec with Modified _ -> CopiedSet.add copied_into acc | Copied _ -> acc )
          copy_map CopiedSet.empty
      in
      let is_captured copy_into =
        match (copy_into : Attribute.CopiedInto.t) with
        | IntoVar {copied_var= ProgramVar pvar} ->
            Captured.mem pvar captured
        | _ ->
            false
      in
      CopyMap.fold
        (fun CopyVar.{copied_into} (copy_spec : CopySpec.t) acc ->
          match (copied_into, copy_spec) with
          | _, Copied _ when CopiedSet.mem copied_into modified || is_captured copied_into ->
              acc
          | ( (IntoField _ | IntoIntermediate _)
            , ( Copied
                  { location
                  ; copied_location
                  ; source_typ
                  ; source_opt= Some (PVar pvar, _) as source_opt
                  ; from
                  ; timestamp= copied_timestamp }
              | Modified
                  { location
                  ; copied_location
                  ; source_typ
                  ; source_opt= Some (PVar pvar, _) as source_opt
                  ; from
                  ; copied_timestamp } ) ) ->
              if
                (not (Pvar.is_global pvar))
                && (not (is_lvalue_ref_param ~ref_formals pvar))
                && (not (List.Assoc.mem ptr_formals ~equal:Pvar.equal pvar))
                && is_never_used_after_copy_into_intermediate_or_field pvar copied_timestamp
                     astate_n
              then
                (* if source var is never used later on, we can still suggest removing the copy even though the copy is modified *)
                (copied_into, source_typ, source_opt, location, copied_location, from) :: acc
              else acc
          | _, Copied {location; copied_location; source_typ; source_opt; from} ->
              (copied_into, source_typ, source_opt, location, copied_location, from) :: acc
          | _, Modified _ ->
              acc )
        copy_map []


let get_const_refable_parameters = function
  | Top ->
      []
  | NonTop {parameter_map; captured; loads; passed_to} ->
      ParameterMap.fold
        (fun var (parameter_spec_t : ParameterSpec.t) acc ->
          if Captured.is_captured_by_ref var captured then acc
          else if
            (Loads.is_loaded var loads || Captured.mem_var var captured)
            && (not (PassedTo.is_copied_to_field_or_global var passed_to))
            && not (PassedTo.is_moved var passed_to)
          then
            match parameter_spec_t with
            | Modified ->
                acc
            | Unmodified {location; typ= copied_source_typ} ->
                (var, copied_source_typ, location) :: acc
          else acc )
        parameter_map []


let add_var_elt copied_into ~source_addr_opt (res : copy_spec_t) astate_n =
  {astate_n with copy_map= CopyMap.add {copied_into; source_addr_opt} res astate_n.copy_map}


let remove_var_elt var astate_n = {astate_n with copy_map= CopyMap.remove_var var astate_n.copy_map}

let add_var copied_into ~source_addr_opt res = map (add_var_elt copied_into ~source_addr_opt res)

let remove_var var = map (remove_var_elt var)

let add_field_elt copied_field ~source_addr_opt (res : copy_spec_t) astate_n =
  { astate_n with
    copy_map=
      CopyMap.add
        {copied_into= IntoField {field= copied_field}; source_addr_opt}
        res astate_n.copy_map }


let add_field copied_field ~source_addr_opt res =
  map (add_field_elt copied_field ~source_addr_opt res)


let add_parameter_elt parameter_var (res : parameter_spec_t) astate_n =
  {astate_n with parameter_map= ParameterMap.add parameter_var res astate_n.parameter_map}


let add_parameter parameter_var res = map (add_parameter_elt parameter_var res)

let is_checked_via_dtor var = function
  | Top ->
      true
  | NonTop {destructor_checked} ->
      DestructorChecked.mem var destructor_checked


let set_captured_variables_elt exp astate_n =
  match exp with
  | Exp.Closure {captured_vars} ->
      List.fold captured_vars ~init:astate_n ~f:(fun astate_n (_, pvar, _, mode) ->
          {astate_n with captured= Captured.add pvar mode astate_n.captured} )
  | _ ->
      astate_n


let set_captured_variables exp = map (set_captured_variables_elt exp)

let set_locked_elt astate_n = {astate_n with locked= true}

let set_locked = map set_locked_elt

let is_locked = function Top -> true | NonTop {locked} -> locked

let set_load_elt loc tstamp ident var astate_n =
  {astate_n with loads= Loads.add loc tstamp ident var astate_n.loads}


let set_load loc tstamp ident var astate_n =
  if Ident.is_none ident then astate_n else map (set_load_elt loc tstamp ident var) astate_n


let set_store_elt loc timestamp var astate_n =
  {astate_n with stores= Stores.add var loc timestamp astate_n.stores}


let set_store loc tstamp var astate_n = map (set_store_elt loc tstamp var) astate_n

let get_loaded_locations var = function
  | Top ->
      []
  | NonTop {loads} ->
      Loads.get_loaded_locations var loads |> TrackedLoc.get_all_keys


let is_captured var astate_n =
  match ((var : Var.t), astate_n) with
  | LogicalVar _, _ ->
      false
  | ProgramVar x, NonTop {captured} ->
      Captured.mem x captured
  | ProgramVar _, Top ->
      true


let set_passed_to_elt loc timestamp call_exp actuals ({loads; passed_to} as astate_n) =
  let new_callee =
    match (call_exp : Exp.t) with
    | Const (Cfun callee) | Closure {name= callee} ->
        let copy_tgt =
          match actuals with
          | (tgt, _) :: _
            when Option.exists (IRAttributes.load callee) ~f:(fun attrs ->
                     attrs.ProcAttributes.is_cpp_copy_ctor
                     || attrs.ProcAttributes.is_cpp_copy_assignment
                     || attrs.ProcAttributes.is_cpp_move_ctor ) ->
              Some tgt
          | _ ->
              None
        in
        CalleeWithUnknown.V {copy_tgt; callee}
    | _ ->
        CalleeWithUnknown.Unknown
  in
  let vars =
    List.fold actuals ~init:Var.Set.empty ~f:(fun acc (actual, _) ->
        match (actual : Exp.t) with
        | Lvar pvar when not (Pvar.is_frontend_tmp pvar) ->
            Var.Set.add (Var.of_pvar pvar) acc
        | Var ident ->
            List.fold (Loads.get_all ident loads) ~init:acc ~f:(fun acc var -> Var.Set.add var acc)
        | _ ->
            acc )
  in
  let passed_to =
    Var.Set.fold (fun var acc -> PassedTo.add var new_callee loc timestamp acc) vars passed_to
  in
  {astate_n with passed_to}


let set_passed_to loc timestamp call_exp actuals =
  map (set_passed_to_elt loc timestamp call_exp actuals)


let get_passed_to var ~f = function
  | Top ->
      `Top
  | NonTop {passed_to} ->
      let callees =
        PassedTo.find var passed_to |> CalleesWithLoc.filter (fun callee _ -> f callee)
      in
      `PassedTo callees


let is_lifetime_extended var astate_n =
  is_captured var astate_n
  ||
  match
    get_passed_to var astate_n ~f:(function
      | CalleeWithUnknown.V {callee} ->
          not (Procname.is_shared_ptr_observer callee)
      | CalleeWithUnknown.Unknown ->
          true )
  with
  | `Top ->
      true
  | `PassedTo callees ->
      not (CalleesWithLoc.is_empty callees)


let bind (execs, non_disj) ~f =
  List.rev execs
  |> List.fold ~init:([], bottom) ~f:(fun (acc, joined_non_disj) elt ->
         let l, new_non_disj = f elt non_disj in
         (l @ acc, join joined_non_disj new_non_disj) )


let remember_dropped_transitive_accesses accesses (non_disj : t) =
  match non_disj with
  | Top ->
      Top
  | NonTop ({dropped_transitive_accesses} as non_disj) ->
      let dropped_transitive_accesses =
        DroppedTransitiveAccesses.union accesses dropped_transitive_accesses
      in
      NonTop {non_disj with dropped_transitive_accesses}


type summary = DroppedTransitiveAccesses.t top_lifted

let make_summary (non_disj : t) =
  match non_disj with
  | Top ->
      Top
  | NonTop {dropped_transitive_accesses} ->
      NonTop dropped_transitive_accesses


let add_transitive_accesses_from_callee procname call_loc (non_disj : t) (summary : summary) =
  match (non_disj, summary) with
  | Top, _ | _, Top ->
      Top
  | NonTop ({dropped_transitive_accesses} as non_dis), NonTop summary ->
      let dropped_transitive_accesses =
        Trace.Set.map_callee (CallEvent.Call procname) call_loc summary
        |> Trace.Set.union dropped_transitive_accesses
      in
      NonTop {non_dis with dropped_transitive_accesses}


module Summary = struct
  include AbstractDomain.TopLifted (DroppedTransitiveAccesses)

  let bottom = NonTop DroppedTransitiveAccesses.empty

  let is_bottom (x : t) =
    match x with Top -> false | NonTop set -> DroppedTransitiveAccesses.is_empty set
end
