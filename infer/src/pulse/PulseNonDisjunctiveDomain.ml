(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open PulseBasicInterface
module BaseMemory = PulseBaseMemory

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
      { typ: Typ.t
      ; location: Location.t
      ; copied_location: (Procname.t * Location.t) option
      ; heap: BaseMemory.t
      ; from: Attribute.CopyOrigin.t }
  | Modified
[@@deriving equal]

module CopySpec = MakeDomainFromTotalOrder (struct
  type t = copy_spec_t [@@deriving equal]

  let leq ~lhs ~rhs =
    match (lhs, rhs) with
    | Copied _, _ ->
        true
    | Modified, Copied _ ->
        false
    | Modified, Modified ->
        true


  let pp fmt = function
    | Copied {typ; heap; location; from} ->
        Format.fprintf fmt "%a (value of type %a) at %a with heap= %a" Attribute.CopyOrigin.pp from
          (Typ.pp Pp.text) typ Location.pp location BaseMemory.pp heap
    | Modified ->
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
        Format.fprintf fmt "const refable (value of type %a) at %a with heap= %a" (Typ.pp Pp.text)
          typ Location.pp location BaseMemory.pp heap
    | Modified ->
        Format.fprintf fmt "modified"
end)

module ParameterVar = struct
  type t = Var.t [@@deriving compare]

  let pp fmt = Var.pp fmt
end

module DestructorChecked = AbstractDomain.FiniteSet (Var)

module Captured = AbstractDomain.FiniteSet (struct
  include Pvar

  let pp = Pvar.pp Pp.text
end)

module CopyMap = AbstractDomain.Map (CopyVar) (CopySpec)
module ParameterMap = AbstractDomain.Map (ParameterVar) (ParameterSpec)
module Locked = AbstractDomain.BooleanOr

type elt =
  { copy_map: CopyMap.t
  ; parameter_map: ParameterMap.t
  ; destructor_checked: DestructorChecked.t
  ; captured: Captured.t
  ; locked: Locked.t }

type t = V of elt | Top

let pp f = function
  | V {copy_map; parameter_map; destructor_checked; captured; locked} ->
      F.fprintf f
        "@[@[copy map: %a@],@ @[parameter map: %a@],@ @[destructor checked: %a@],@ @[captured: \
         %a@],@ @[locked: %a@]@]"
        CopyMap.pp copy_map ParameterMap.pp parameter_map DestructorChecked.pp destructor_checked
        Captured.pp captured Locked.pp locked
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
        ; locked= Locked.join x.locked y.locked }


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
        ; locked= Locked.widen ~prev:prev.locked ~next:next.locked ~num_iters }


let bottom =
  V
    { copy_map= CopyMap.empty
    ; parameter_map= ParameterMap.empty
    ; destructor_checked= DestructorChecked.empty
    ; captured= Captured.empty
    ; locked= Locked.bottom }


let is_bottom = function
  | Top ->
      false
  | V {copy_map; parameter_map; destructor_checked; captured; locked} ->
      CopyMap.is_bottom copy_map
      && ParameterMap.is_bottom parameter_map
      && DestructorChecked.is_bottom destructor_checked
      && Captured.is_bottom captured && Locked.is_bottom locked


let top = Top

let is_top = function Top -> true | V _ -> false

let map f = function Top -> Top | V astate -> V (f astate)

let mark_copy_as_modified_elt ~is_modified ~copied_into ~source_addr_opt ({copy_map} as astate) =
  let copy_var = CopyVar.{copied_into; source_addr_opt} in
  let copy_map =
    match CopyMap.find_opt copy_var copy_map with
    | Some (Copied {heap= copy_heap}) when is_modified copy_heap ->
        Logging.d_printfln_escaped "Copy/source modified!" ;
        CopyMap.add copy_var Modified copy_map
    | _ ->
        copy_map
  in
  {astate with copy_map}


let mark_copy_as_modified ~is_modified ~copied_into ~source_addr_opt =
  map (mark_copy_as_modified_elt ~is_modified ~copied_into ~source_addr_opt)


let mark_parameter_as_modified_elt ~is_modified ~var ({parameter_map} as astate) =
  let parameter_map =
    match ParameterMap.find_opt var parameter_map with
    | Some (Unmodified {heap= copy_heap}) when is_modified copy_heap ->
        Logging.d_printfln_escaped "Parameter %a modified!" Var.pp var ;
        ParameterMap.add var Modified parameter_map
    | _ ->
        parameter_map
  in
  {astate with parameter_map}


let mark_parameter_as_modified ~is_modified ~var =
  map (mark_parameter_as_modified_elt ~is_modified ~var)


let checked_via_dtor_elt var astate =
  {astate with destructor_checked= DestructorChecked.add var astate.destructor_checked}


let checked_via_dtor var = map (checked_via_dtor_elt var)

module CopiedSet = PrettyPrintable.MakePPSet (Attribute.CopiedInto)

let get_copied = function
  | Top ->
      []
  | V {copy_map; captured} ->
      let modified =
        CopyMap.fold
          (fun CopyVar.{copied_into} (copy_spec : CopySpec.t) acc ->
            match copy_spec with Modified -> CopiedSet.add copied_into acc | Copied _ -> acc )
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
          match copy_spec with
          | Modified ->
              acc
          | Copied {location; copied_location; typ= copied_typ; from} ->
              if CopiedSet.mem copied_into modified || is_captured copied_into then acc
              else (copied_into, copied_typ, location, copied_location, from) :: acc )
        copy_map []


let get_const_refable_parameters = function
  | Top ->
      []
  | V {parameter_map} ->
      ParameterMap.fold
        (fun var (parameter_spec_t : ParameterSpec.t) acc ->
          match parameter_spec_t with
          | Modified ->
              acc
          | Unmodified {location; typ= copied_typ} ->
              (var, copied_typ, location) :: acc )
        parameter_map []


let add_var_elt copied_var ~source_addr_opt ~source_opt (res : copy_spec_t) astate =
  { astate with
    copy_map=
      CopyMap.add
        {copied_into= IntoVar {copied_var; source_opt}; source_addr_opt}
        res astate.copy_map }


let add_var copied_var ~source_addr_opt ~source_opt res =
  map (add_var_elt copied_var ~source_addr_opt ~source_opt res)


let add_field_elt copied_field ~source_opt (res : copy_spec_t) astate =
  { astate with
    copy_map=
      CopyMap.add
        { copied_into= IntoField {field= copied_field; source_opt}
        ; source_addr_opt= Option.bind source_opt ~f:PulseDecompilerExpr.abstract_value_of_expr }
        res astate.copy_map }


let add_field copied_field ~source_opt res = map (add_field_elt copied_field ~source_opt res)

let add_parameter_elt parameter_var (res : parameter_spec_t) astate =
  {astate with parameter_map= ParameterMap.add parameter_var res astate.parameter_map}


let add_parameter parameter_var res = map (add_parameter_elt parameter_var res)

let is_checked_via_dtor var = function
  | Top ->
      true
  | V {destructor_checked} ->
      DestructorChecked.mem var destructor_checked


let set_captured_variables_elt exp astate =
  match exp with
  | Exp.Closure {captured_vars} ->
      List.fold captured_vars ~init:astate ~f:(fun astate (_, pvar, _, _) ->
          {astate with captured= Captured.add pvar astate.captured} )
  | _ ->
      astate


let set_captured_variables exp = map (set_captured_variables_elt exp)

let set_locked_elt astate = {astate with locked= true}

let set_locked = map set_locked_elt

let is_locked = function Top -> true | V {locked} -> locked
