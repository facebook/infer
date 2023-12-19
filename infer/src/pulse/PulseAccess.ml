(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module AbstractValue = PulseAbstractValue

module type S = sig
  type key

  include PrettyPrintable.PrintableEquatableOrderedType with type t = key MemoryAccess.t

  val is_strong_access : Tenv.t -> t -> bool

  val canonicalize : get_var_repr:(PulseAbstractValue.t -> PulseAbstractValue.t) -> t -> t

  val yojson_of_t : t -> Yojson.Safe.t

  module Set : Caml.Set.S with type elt = t
end

module T = struct
  type t = AbstractValue.t MemoryAccess.t [@@deriving yojson_of]

  let compare = MemoryAccess.loose_compare AbstractValue.compare

  let equal = [%compare.equal: t]

  let pp = MemoryAccess.pp AbstractValue.pp

  let canonicalize ~get_var_repr (access : t) =
    match access with
    | ArrayAccess (typ, addr) ->
        let addr' = get_var_repr addr in
        if AbstractValue.equal addr addr' then access else MemoryAccess.ArrayAccess (typ, addr')
    | FieldAccess _ | TakeAddress | Dereference ->
        access


  let is_strong_access tenv (access : t) =
    let has_weak_or_unretained_or_assign annotations =
      List.exists annotations ~f:(fun (ann : Annot.t) ->
          ( String.equal ann.class_name Config.property_attributes
          || String.equal ann.class_name Config.ivar_attributes )
          && List.exists
               ~f:(fun Annot.{value} ->
                 Annot.has_matching_str_value value ~pred:(fun att ->
                     String.equal Config.unsafe_unret att
                     || String.equal Config.weak att || String.equal Config.assign att ) )
               ann.parameters )
    in
    match access with
    | FieldAccess fieldname -> (
        let classname = Fieldname.get_class_name fieldname in
        let is_capture_field_strong fieldname =
          (* a strongly referencing capture field is a capture field that is not weak *)
          Fieldname.is_capture_field_in_closure fieldname
          && not (Fieldname.is_weak_capture_field_in_closure fieldname)
        in
        match Tenv.lookup tenv classname with
        | None when is_capture_field_strong fieldname ->
            (* Strongly referencing captures *)
            true
        | None ->
            (* Can't tell if we have a strong reference. To avoid FP on retain cycles,
               assume weak reference by default *)
            false
        | Some {fields} -> (
          match List.find fields ~f:(fun (name, _, _) -> Fieldname.equal name fieldname) with
          | None ->
              (* Can't tell if we have a strong reference. To avoid FP on retain cycles,
                 assume weak reference by default *)
              false
          | Some (_, typ, anns) -> (
            match typ.Typ.desc with
            | Tptr (_, (Pk_objc_weak | Pk_objc_unsafe_unretained)) ->
                false
            | _ ->
                not (has_weak_or_unretained_or_assign anns) ) ) )
    | _ ->
        true
end

include T
module Set = Caml.Set.Make (T)
