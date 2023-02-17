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
      ; location: Location.t
      ; copied_location: (Procname.t * Location.t) option
      ; heap: BaseMemory.t
      ; from: Attribute.CopyOrigin.t
      ; timestamp: Timestamp.t }
  | Modified of
      { source_typ: Typ.t option
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
  include AbstractDomain.FiniteSet (struct
    include Pvar

    let pp = Pvar.pp Pp.text
  end)

  let mem_var var x =
    match (var : Var.t) with ProgramVar pvar -> mem pvar x | LogicalVar _ -> false
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

module TrackedLoc = struct
  type t = {loc: Location.t; timestamp: Timestamp.t} [@@deriving compare, equal]

  let pp fmt {loc} = Location.pp fmt loc
end

module Loads = struct
  module IdentToVars = AbstractDomain.FiniteMultiMap (Ident) (Var)
  module LoadedVars = AbstractDomain.FiniteMultiMap (Var) (TrackedLoc)
  include AbstractDomain.PairWithBottom (IdentToVars) (LoadedVars)

  let add loc timestamp ident var (ident_to_vars, loaded_vars) =
    (IdentToVars.add ident var ident_to_vars, LoadedVars.add var {loc; timestamp} loaded_vars)


  let get_all ident (ident_to_vars, _) = IdentToVars.get_all ident ident_to_vars

  let is_loaded var (_, loaded_vars) = LoadedVars.mem var loaded_vars

  let get_loaded_locations var (_, loaded_vars) = LoadedVars.get_all var loaded_vars
end

module PVar = struct
  type t = Pvar.t [@@deriving compare]

  let pp = Pvar.pp Pp.text
end

module Stores = AbstractDomain.FiniteMultiMap (PVar) (TrackedLoc)

module CalleeWithUnknown = struct
  type t = V of {copy_tgt: Exp.t option; callee: Procname.t; timestamp: Timestamp.t} | Unknown
  [@@deriving compare]

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
end

module CalleeWithLoc = struct
  type t = {callee: CalleeWithUnknown.t; loc: Location.t} [@@deriving compare]

  let pp f {callee; loc} = F.fprintf f "%a at %a" CalleeWithUnknown.pp callee Location.pp loc

  let is_copy_to_field_or_global {callee} = CalleeWithUnknown.is_copy_to_field_or_global callee
end

module PassedTo = struct
  include AbstractDomain.FiniteMultiMap (Var) (CalleeWithLoc)

  let is_copied_to_field_or_global var x =
    List.exists (get_all var x) ~f:CalleeWithLoc.is_copy_to_field_or_global
end

type elt =
  { copy_map: CopyMap.t
  ; parameter_map: ParameterMap.t
  ; destructor_checked: DestructorChecked.t
  ; captured: Captured.t
  ; locked: Locked.t
  ; loads: Loads.t
  ; stores: Stores.t
  ; passed_to: PassedTo.t }

type t = V of elt | Top

let pp f = function
  | V {copy_map; parameter_map; destructor_checked; captured; locked; loads; passed_to} ->
      F.fprintf f
        "@[@[copy map: %a@],@ @[parameter map: %a@],@ @[destructor checked: %a@],@ @[captured: \
         %a@],@ @[locked: %a@],@ @[loads: %a@],@ @[passed to: %a@]@]"
        CopyMap.pp copy_map ParameterMap.pp parameter_map DestructorChecked.pp destructor_checked
        Captured.pp captured Locked.pp locked Loads.pp loads PassedTo.pp passed_to
  | Top ->
      AbstractDomain.TopLiftedUtils.pp_top f


let leq ~lhs ~rhs =
  match (lhs, rhs) with
  | _, Top ->
      true
  | Top, _ ->
      false
  | V lhs, V rhs ->
      CopyMap.leq ~lhs:lhs.copy_map ~rhs:rhs.copy_map
      && ParameterMap.leq ~lhs:lhs.parameter_map ~rhs:rhs.parameter_map
      && DestructorChecked.leq ~lhs:lhs.destructor_checked ~rhs:rhs.destructor_checked
      && Captured.leq ~lhs:lhs.captured ~rhs:rhs.captured
      && Locked.leq ~lhs:lhs.locked ~rhs:rhs.locked
      && Loads.leq ~lhs:lhs.loads ~rhs:rhs.loads
      && PassedTo.leq ~lhs:lhs.passed_to ~rhs:rhs.passed_to


let join x y =
  match (x, y) with
  | _, Top | Top, _ ->
      Top
  | V x, V y ->
      V
        { copy_map= CopyMap.join x.copy_map y.copy_map
        ; parameter_map= ParameterMap.join x.parameter_map y.parameter_map
        ; destructor_checked= DestructorChecked.join x.destructor_checked y.destructor_checked
        ; captured= Captured.join x.captured y.captured
        ; locked= Locked.join x.locked y.locked
        ; loads= Loads.join x.loads y.loads
        ; stores= Stores.join x.stores y.stores
        ; passed_to= PassedTo.join x.passed_to y.passed_to }


let widen ~prev ~next ~num_iters =
  match (prev, next) with
  | _, Top | Top, _ ->
      Top
  | V prev, V next ->
      V
        { copy_map= CopyMap.widen ~prev:prev.copy_map ~next:next.copy_map ~num_iters
        ; parameter_map=
            ParameterMap.widen ~prev:prev.parameter_map ~next:next.parameter_map ~num_iters
        ; destructor_checked=
            DestructorChecked.widen ~prev:prev.destructor_checked ~next:next.destructor_checked
              ~num_iters
        ; captured= Captured.widen ~prev:prev.captured ~next:next.captured ~num_iters
        ; locked= Locked.widen ~prev:prev.locked ~next:next.locked ~num_iters
        ; loads= Loads.widen ~prev:prev.loads ~next:next.loads ~num_iters
        ; stores= Stores.widen ~prev:prev.stores ~next:next.stores ~num_iters
        ; passed_to= PassedTo.widen ~prev:prev.passed_to ~next:next.passed_to ~num_iters }


let bottom =
  V
    { copy_map= CopyMap.empty
    ; parameter_map= ParameterMap.empty
    ; destructor_checked= DestructorChecked.empty
    ; captured= Captured.empty
    ; locked= Locked.bottom
    ; loads= Loads.bottom
    ; stores= Stores.bottom
    ; passed_to= PassedTo.bottom }


let is_bottom = function
  | Top ->
      false
  | V {copy_map; parameter_map; destructor_checked; captured; locked; loads; passed_to} ->
      CopyMap.is_bottom copy_map
      && ParameterMap.is_bottom parameter_map
      && DestructorChecked.is_bottom destructor_checked
      && Captured.is_bottom captured && Locked.is_bottom locked && Loads.is_bottom loads
      && PassedTo.is_bottom passed_to


let top = Top

let is_top = function Top -> true | V _ -> false

let map f = function Top -> Top | V astate_n -> V (f astate_n)

let mark_copy_as_modified_elt ~is_modified ~copied_into ~source_addr_opt ({copy_map} as astate_n) =
  let copy_var = CopyVar.{copied_into; source_addr_opt} in
  let copy_map =
    match CopyMap.find_opt copy_var copy_map with
    | Some
        (Copied
          {source_typ; from; copied_location; location; heap= copy_heap; timestamp= copied_timestamp}
          )
      when is_modified copy_heap copied_timestamp ->
        Logging.d_printfln_escaped "Copy/source modified!" ;
        let modified : copy_spec_t =
          Modified {source_typ; location; copied_location; from; copied_timestamp}
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
  | V {passed_to; loads; stores} ->
      let is_after_copy =
        List.exists ~f:(fun TrackedLoc.{timestamp} ->
            (copied_timestamp :> int) < (timestamp :> int) )
      in
      let source_var = Var.of_pvar pvar in
      let is_passed_to_non_destructor_after_copy =
        PassedTo.get_all source_var passed_to
        |> List.exists ~f:(fun {CalleeWithLoc.callee; _} ->
               match callee with
               | V {callee; timestamp} when (copied_timestamp :> int) < (timestamp :> int) ->
                   not (Procname.is_destructor callee)
               | _ ->
                   false )
      in
      let is_loaded_after_copy = Loads.get_loaded_locations source_var loads |> is_after_copy in
      let is_stored_after_copy = Stores.get_all pvar stores |> is_after_copy in
      not (is_loaded_after_copy || is_stored_after_copy || is_passed_to_non_destructor_after_copy)


let get_copied astate_n =
  match astate_n with
  | Top ->
      []
  | V {copy_map; captured} ->
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
          | ( ( IntoField {source_opt= Some (SourceExpr ((PVar pvar, _), _))}
              | IntoIntermediate {source_opt= Some (PVar pvar, _)} )
            , ( Copied {location; copied_location; source_typ; from; timestamp= copied_timestamp}
              | Modified {location; copied_location; source_typ; from; copied_timestamp} ) ) ->
              if is_never_used_after_copy_into_intermediate_or_field pvar copied_timestamp astate_n
              then
                (* if source var is never used later on, we can still suggest removing the copy even though the copy is modified *)
                (copied_into, source_typ, location, copied_location, from) :: acc
              else acc
          | _, Copied {location; copied_location; source_typ; from} ->
              (copied_into, source_typ, location, copied_location, from) :: acc
          | _, Modified _ ->
              acc )
        copy_map []


let get_const_refable_parameters = function
  | Top ->
      []
  | V {parameter_map; captured; loads; passed_to} ->
      ParameterMap.fold
        (fun var (parameter_spec_t : ParameterSpec.t) acc ->
          if
            (Loads.is_loaded var loads || Captured.mem_var var captured)
            && not (PassedTo.is_copied_to_field_or_global var passed_to)
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

let add_field_elt copied_field ~source_opt (res : copy_spec_t) astate_n =
  { astate_n with
    copy_map=
      CopyMap.add
        { copied_into= IntoField {field= copied_field; source_opt}
        ; source_addr_opt= Option.bind source_opt ~f:DecompilerExpr.abstract_value_of_expr }
        res astate_n.copy_map }


let add_field copied_field ~source_opt res = map (add_field_elt copied_field ~source_opt res)

let add_parameter_elt parameter_var (res : parameter_spec_t) astate_n =
  {astate_n with parameter_map= ParameterMap.add parameter_var res astate_n.parameter_map}


let add_parameter parameter_var res = map (add_parameter_elt parameter_var res)

let is_checked_via_dtor var = function
  | Top ->
      true
  | V {destructor_checked} ->
      DestructorChecked.mem var destructor_checked


let set_captured_variables_elt exp astate_n =
  match exp with
  | Exp.Closure {captured_vars} ->
      List.fold captured_vars ~init:astate_n ~f:(fun astate_n (_, pvar, _, _) ->
          {astate_n with captured= Captured.add pvar astate_n.captured} )
  | _ ->
      astate_n


let set_captured_variables exp = map (set_captured_variables_elt exp)

let set_locked_elt astate_n = {astate_n with locked= true}

let set_locked = map set_locked_elt

let is_locked = function Top -> true | V {locked} -> locked

let set_load_elt loc tstamp ident var astate_n =
  {astate_n with loads= Loads.add loc tstamp ident var astate_n.loads}


let set_load loc tstamp ident var astate_n =
  if Ident.is_none ident then astate_n else map (set_load_elt loc tstamp ident var) astate_n


let set_store_elt loc timestamp var astate_n =
  {astate_n with stores= Stores.add var {loc; timestamp} astate_n.stores}


let set_store loc tstamp var astate_n = map (set_store_elt loc tstamp var) astate_n

let get_loaded_locations var = function
  | Top ->
      []
  | V {loads} ->
      Loads.get_loaded_locations var loads |> List.map ~f:(fun TrackedLoc.{loc} -> loc)


let is_captured var astate_n =
  match ((var : Var.t), astate_n) with
  | LogicalVar _, _ ->
      false
  | ProgramVar x, V {captured} ->
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
                     || attrs.ProcAttributes.is_cpp_copy_assignment ) ->
              Some tgt
          | _ ->
              None
        in
        CalleeWithUnknown.V {copy_tgt; callee; timestamp}
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
    Var.Set.fold (fun var acc -> PassedTo.add var {callee= new_callee; loc} acc) vars passed_to
  in
  {astate_n with passed_to}


let set_passed_to loc timestamp call_exp actuals =
  map (set_passed_to_elt loc timestamp call_exp actuals)


let get_passed_to var ~f = function
  | Top ->
      `Top
  | V {passed_to} ->
      let callees =
        PassedTo.get_all var passed_to |> List.filter ~f:(fun {CalleeWithLoc.callee; _} -> f callee)
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
      not (List.is_empty callees)
