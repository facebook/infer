(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type objc_property_attribute = Copy | Strong | Weak [@@deriving compare, equal, hash, normalize]

type field =
  { name: Fieldname.t
  ; typ: Typ.t
  ; annot: Annot.Item.t
  ; objc_property_attributes: objc_property_attribute list }
[@@deriving compare, equal, hash, normalize]

let mk_field ?(annot = Annot.Item.empty) ?(objc_property_attributes = []) name typ =
  {name; typ; annot; objc_property_attributes}


let field_has_weak field =
  List.exists field.objc_property_attributes ~f:(fun attr ->
      equal_objc_property_attribute attr Weak )


type java_class_kind = Interface | AbstractClass | NormalClass
[@@deriving equal, compare, hash, show {with_path= false}, normalize]

type hack_class_kind = Class | AbstractClass | Interface | Trait | Alias
[@@deriving equal, hash, show {with_path= false}, normalize]

module ClassInfo = struct
  type t =
    | NoInfo
    | CppClassInfo of {is_trivially_copyable: bool}
    | JavaClassInfo of
        { kind: java_class_kind  (** class kind in Java *)
        ; loc: Location.t option
              (** None should correspond to rare cases when it was impossible to fetch the location
                  in source file *) }
    | HackClassInfo of hack_class_kind
  [@@deriving equal, hash, show {with_path= false}, normalize]
end

(** Type for a structured value. *)
type t =
  { fields: field list  (** non-static fields *)
  ; statics: field list  (** static fields *)
  ; supers: Typ.Name.t list  (** superclasses *)
  ; objc_protocols: Typ.Name.t list  (** ObjC protocols *)
  ; methods: Procname.t list  (** methods defined *)
  ; exported_objc_methods: Procname.t list  (** methods in ObjC interface, subset of [methods] *)
  ; annots: Annot.Item.t  (** annotations *)
  ; class_info: ClassInfo.t  (** present if and only if the class is C++, Java or Hack *)
  ; dummy: bool  (** dummy struct for class including static method *)
  ; source_file: SourceFile.t option  (** source file containing this struct's declaration *) }
[@@deriving equal, hash, normalize]

type lookup = Typ.Name.t -> t option

let pp_objc_property_attribute f attributes =
  let s = match attributes with Copy -> "copy" | Strong -> "strong" | Weak -> "weak" in
  F.fprintf f "%s" s


let pp_objc_property_attributes f attrs =
  if List.is_empty attrs then ()
  else F.fprintf f "(%a)" (Pp.comma_seq pp_objc_property_attribute) attrs


let pp_field pe f {name= field_name; typ; annot; objc_property_attributes} =
  F.fprintf f "@\n\t\t%a %a %a %a" (Typ.pp_full pe) typ Fieldname.pp field_name Annot.Item.pp annot
    pp_objc_property_attributes objc_property_attributes


let pp pe name f
    ({ fields
     ; statics
     ; supers
     ; objc_protocols
     ; methods
     ; exported_objc_methods
     ; annots
     ; class_info
     ; dummy
     ; source_file } [@warning "+missing-record-field-pattern"] ) =
  let pp_field pe f {name; typ; annot; objc_property_attributes} =
    F.fprintf f "@;<0 2>%a %a %a %a" (Typ.pp_full pe) typ Fieldname.pp name Annot.Item.pp annot
      pp_objc_property_attributes objc_property_attributes
  in
  let seq pp fmt = function
    | [] ->
        ()
    | lst ->
        Pp.seq pp fmt lst ;
        F.pp_print_break fmt 0 0
  in
  F.fprintf f
    "%a@,\
     @[<v>fields: {@[<v>%a@]}@,\
     statics: {@[<v>%a@]}@,\
     supers: {@[<v>%a@]}@,\
     objc_protocols: {@[<v>%a@]}@,\
     methods: {@[<v>%a@]}@,\
     exported_obj_methods: {@[<v>%a@]}@,\
     annots: {@[<v>%a@]}@,\
     class_info: {@[<v>%a@]}@,\
     dummy: %b@,\
     %a@]"
    Typ.Name.pp name
    (seq (pp_field pe))
    fields
    (seq (pp_field pe))
    statics
    (seq (fun f n -> F.fprintf f "@;<0 2>%a" Typ.Name.pp n))
    supers
    (seq (fun f n -> F.fprintf f "@;<0 2>%a" Typ.Name.pp n))
    objc_protocols
    (seq (fun f m -> F.fprintf f "@;<0 2>%a" Procname.pp_verbose m))
    methods
    (seq (fun f m -> F.fprintf f "@;<0 2>%a" Procname.pp_verbose m))
    exported_objc_methods Annot.Item.pp annots ClassInfo.pp class_info dummy
    (fun fs -> Option.iter ~f:(Format.fprintf fs "source_file: %a@," SourceFile.pp))
    source_file


let compare_custom_field {name= fld} {name= fld'} = Fieldname.compare fld fld'

let make_sorted_struct fields' statics' methods' supers' annots' class_info ?source_file dummy =
  let fields = List.dedup_and_sort ~compare:compare_custom_field fields' in
  let statics = List.dedup_and_sort ~compare:compare_custom_field statics' in
  let methods = List.dedup_and_sort ~compare:Procname.compare methods' in
  let supers = List.dedup_and_sort ~compare:Typ.Name.compare supers' in
  let annots = List.dedup_and_sort ~compare:Annot.compare annots' in
  { fields
  ; statics
  ; methods
  ; exported_objc_methods= []
  ; supers
  ; objc_protocols= []
  ; annots
  ; class_info
  ; dummy
  ; source_file }


let internal_mk_struct ?default ?fields ?statics ?methods ?exported_objc_methods ?supers
    ?objc_protocols ?annots ?class_info ?dummy ?source_file typename =
  let default_ =
    { fields= []
    ; statics= []
    ; methods= []
    ; exported_objc_methods= []
    ; supers= []
    ; objc_protocols= []
    ; annots= Annot.Item.empty
    ; class_info= NoInfo
    ; dummy= false
    ; source_file= None }
  in
  let mk_struct_ ?(default = default_) ?(fields = default.fields) ?(statics = default.statics)
      ?(methods = default.methods) ?(exported_objc_methods = default.exported_objc_methods)
      ?(supers = default.supers) ?(objc_protocols = default.objc_protocols)
      ?(annots = default.annots) ?(dummy = default.dummy) ?source_file typename =
    let class_info = Option.value class_info ~default:ClassInfo.NoInfo in
    match typename with
    | Typ.JavaClass _ | Typ.HackClass _ | Typ.CSharpClass _ | Typ.PythonClass _ ->
        make_sorted_struct fields statics methods supers annots class_info ?source_file dummy
    | _ ->
        { fields
        ; statics
        ; methods
        ; exported_objc_methods
        ; supers
        ; objc_protocols
        ; annots
        ; class_info
        ; dummy
        ; source_file }
  in
  mk_struct_ ?default ?fields ?statics ?methods ?exported_objc_methods ?supers ?objc_protocols
    ?annots ?dummy ?source_file typename


(** the element typ of the final extensible array in the given typ, if any *)
let rec get_extensible_array_element_typ ~lookup (typ : Typ.t) =
  match typ.desc with
  | Tarray {elt} ->
      Some elt
  | Tstruct name -> (
    match lookup name with
    | Some {fields} -> (
      match List.last fields with
      | Some {typ= fld_typ} ->
          get_extensible_array_element_typ ~lookup fld_typ
      | None ->
          None )
    | None ->
        None )
  | _ ->
      None


(** If a struct type with field f, return Some (the type of f). If not, return None. *)
let fld_typ_opt ~lookup fn (typ : Typ.t) =
  (* Note: would be nice migrate it to get_field_info
     (for that one needs to ensure adding Tptr to pattern match does not break thing) *)
  match typ.desc with
  | Tstruct name -> (
    match lookup name with
    | Some {fields} ->
        List.find ~f:(fun {name= f} -> Fieldname.equal f fn) fields
        |> Option.map ~f:(fun {typ} -> typ)
    | None ->
        None )
  | _ ->
      None


(** If a struct type with field f, return the type of f. If not, return the default *)
let fld_typ ~lookup ~default fn (typ : Typ.t) = Option.value (fld_typ_opt ~lookup fn typ) ~default

type field_info = {typ: Typ.t; annotations: Annot.Item.t; is_static: bool}

let find_field field_list field_name_to_lookup =
  List.find_map
    ~f:(fun {name; typ; annot} ->
      if Fieldname.equal name field_name_to_lookup then Some (typ, annot) else None )
    field_list


let get_field_info ~lookup field_name_to_lookup (typ : Typ.t) =
  let find_field_info field_list ~is_static =
    find_field field_list field_name_to_lookup
    |> Option.map ~f:(fun (typ, annot) -> {typ; annotations= annot; is_static})
  in
  match typ.desc with
  | Tstruct name | Tptr ({desc= Tstruct name}, _) -> (
    match lookup name with
    | Some {fields= non_statics; statics} ->
        (* Search in both lists and return the first found *)
        find_field_info statics ~is_static:true
        |> IOption.if_none_evalopt ~f:(fun () -> find_field_info non_statics ~is_static:false)
    | None ->
        None )
  | _ ->
      None


let get_field_type_and_annotation ~lookup field_name_to_lookup typ =
  get_field_info ~lookup field_name_to_lookup typ
  |> Option.map ~f:(fun {typ; annotations} -> (typ, annotations))


let is_dummy {dummy} = dummy

let get_source_file {source_file} = source_file

(* is [lhs] included in [rhs] when both are sorted and deduped *)
let rec is_subsumed ~compare lhs rhs =
  match (lhs, rhs) with
  | [], _ ->
      true
  | _, [] ->
      false
  | x :: xs, y :: ys ->
      let r = compare x y in
      if Int.equal 0 r then (* [x] is in both lists so skip *) is_subsumed ~compare xs ys
      else if r < 0 then (* [x < y] but lists are sorted so [x] is not in [rhs] *) false
      else (* [x > y] so it could be that [x] is in [ys] *) is_subsumed ~compare lhs ys


let merge_dedup_sorted_lists ~compare ~newer ~current =
  let equal a b = Int.equal 0 (compare a b) in
  (* equal elements of [newer] will be first *)
  List.merge ~compare newer current
  |> (* we keep the last duplicate, thus from [current] *)
  List.remove_consecutive_duplicates ~which_to_keep:`Last ~equal


let merge_lists ~compare ~newer ~current =
  match (newer, current) with
  | [], _ ->
      current
  | _, [] ->
      newer
  | _, _ when is_subsumed ~compare newer current ->
      current
  | _, _ ->
      merge_dedup_sorted_lists ~compare ~newer ~current


let merge_fields ~newer ~current = merge_lists ~compare:compare_custom_field ~newer ~current

let merge_supers ~newer ~current = merge_lists ~compare:Typ.Name.compare ~newer ~current

let merge_methods ~newer ~current = merge_lists ~compare:Procname.compare ~newer ~current

let merge_annots ~newer ~current = merge_lists ~compare:Annot.compare ~newer ~current

let merge_kind ~newer ~current =
  (* choose the maximal, ie most concrete *)
  if compare_java_class_kind newer current <= 0 then current else newer


(* choose [Some] option if possible, [newer] if both [None] else [merge] *)
let merge_opt ~merge ~newer ~current =
  match (newer, current) with
  | None, _ ->
      current
  | _, None ->
      newer
  | Some newer_val, Some current_val ->
      let merged_val = merge ~newer:newer_val ~current:current_val in
      if phys_equal merged_val current_val then current else Some merged_val


let merge_loc ~newer ~current =
  if Location.equal Location.dummy newer then current
  else if Location.equal Location.dummy current then newer
  else if (* arbitrarily but deterministically choose one *) Location.compare newer current <= 0
  then newer
  else current


let merge_loc_opt ~newer ~current = merge_opt ~merge:merge_loc ~newer ~current

let merge_java_class_info ~newer ~current =
  match ((newer : ClassInfo.t), (current : ClassInfo.t)) with
  | ( JavaClassInfo {kind= newer_kind; loc= newer_loc}
    , JavaClassInfo {kind= current_kind; loc= current_loc} ) ->
      let kind = merge_kind ~newer:newer_kind ~current:current_kind in
      let loc = merge_loc_opt ~newer:newer_loc ~current:current_loc in
      if phys_equal kind current_kind && phys_equal loc current_loc then current
      else ClassInfo.JavaClassInfo {kind; loc}
  | _, _ ->
      assert false


let merge_class_info ~newer ~current =
  match ((newer : ClassInfo.t), (current : ClassInfo.t)) with
  | NoInfo, c | c, NoInfo ->
      c
  | JavaClassInfo _, JavaClassInfo _ ->
      merge_java_class_info ~newer ~current
  | HackClassInfo _, HackClassInfo _ ->
      (* no merge currently for Hack *) current
  | _, _ ->
      Logging.die InternalError "Tried to merge a JavaClassInfo with a HackClassInfo value.@\n"


let merge_source_file ~newer ~current =
  merge_opt ~newer ~current ~merge:(fun ~newer ~current ->
      (* return [current] if equal, else lexicographically least *)
      if SourceFile.compare newer current >= 0 then current else newer )


(* must only be used on structs made with [make_sorted_struct] *)
let full_merge ~newer ~current =
  let fields = merge_fields ~newer:newer.fields ~current:current.fields in
  let statics = merge_fields ~newer:newer.statics ~current:current.statics in
  let supers = merge_supers ~newer:newer.supers ~current:current.supers in
  let methods = merge_methods ~newer:newer.methods ~current:current.methods in
  let annots = merge_annots ~newer:newer.annots ~current:current.annots in
  let class_info = merge_class_info ~newer:newer.class_info ~current:current.class_info in
  let source_file = merge_source_file ~newer:newer.source_file ~current:current.source_file in
  if
    phys_equal fields current.fields
    && phys_equal statics current.statics
    && phys_equal supers current.supers
    && phys_equal methods current.methods
    && phys_equal annots current.annots
    && phys_equal class_info current.class_info
    && phys_equal source_file current.source_file
  then current
  else
    (* not using [with] syntax to force handling new fields added to [t] *)
    { fields
    ; statics
    ; supers
    ; methods
    ; annots
    ; class_info
    ; source_file
    ; objc_protocols= current.objc_protocols
    ; exported_objc_methods= current.exported_objc_methods
    ; dummy= current.dummy }


let merge typename ~newer ~current =
  match (typename : Typ.Name.t) with
  | CStruct _
  | CUnion _
  | ErlangType _
  | ObjcClass _
  | ObjcProtocol _
  | CppClass _
  | ObjcBlock _
  | CFunction _ ->
      if not (is_dummy newer) then newer else current
  | JavaClass _ when is_dummy newer ->
      current
  | JavaClass _ when is_dummy current ->
      newer
  | JavaClass _ ->
      full_merge ~newer ~current
  | CSharpClass _ when is_dummy newer ->
      current
  | CSharpClass _ when is_dummy current ->
      newer
  | CSharpClass _ ->
      full_merge ~newer ~current
  | HackClass _ when is_dummy newer ->
      current
  | HackClass _ when is_dummy current ->
      newer
  | HackClass _ ->
      (* NOTE: when we translate Hack we have 3 sources of Structs:

          1. classes defined inside the file that is being translated,
          2. classes that are created from ProcDecls used in the file but defined inside a different
             file,
          3. classes that are used but not defined in any way and are marked as dummy.

          We should never need to merge two structs from Group 1, but Group 1 + Group 2 or Group 2 +
          Group 2 are valid cases. There's not much benefit in differentiating between all these
          cases, hence when we see two non-dummy Hack structs we just do a full merge. *)
      full_merge ~newer ~current
  | PythonClass _ when is_dummy newer ->
      current
  | PythonClass _ when is_dummy current ->
      newer
  | PythonClass _ ->
      (* NOTE: I don't know 'yet' what I'm doing. Two python files should only have builtin
         declarations in common, and they are generated in an identical way, so we should be
         good. *)
      full_merge ~newer ~current


let is_not_java_interface = function
  | {class_info= JavaClassInfo {kind= Interface}} ->
      false
  | _ ->
      true


let is_hack_class {class_info} =
  match (class_info : ClassInfo.t) with
  | HackClassInfo Class | HackClassInfo AbstractClass ->
      true
  | _ ->
      false


let is_hack_abstract_class {class_info} =
  match (class_info : ClassInfo.t) with HackClassInfo AbstractClass -> true | _ -> false


let is_hack_interface {class_info} =
  match (class_info : ClassInfo.t) with HackClassInfo Interface -> true | _ -> false


let is_hack_alias {class_info} =
  match (class_info : ClassInfo.t) with HackClassInfo Alias -> true | _ -> false


let is_hack_trait {class_info} =
  match (class_info : ClassInfo.t) with HackClassInfo Trait -> true | _ -> false
