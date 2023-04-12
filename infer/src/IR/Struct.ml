(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type field = Fieldname.t * Typ.t * Annot.Item.t [@@deriving compare, equal]

type fields = field list [@@deriving equal]

type java_class_kind = Interface | AbstractClass | NormalClass
[@@deriving equal, compare, show {with_path= false}]

type java_class_info =
  { kind: java_class_kind  (** class kind in Java *)
  ; loc: Location.t option
        (** None should correspond to rare cases when it was impossible to fetch the location in
            source file *) }
[@@deriving equal, show {with_path= false}]

let pp_java_class_info_opt fmt jopt = Pp.option pp_java_class_info fmt jopt

(** Type for a structured value. *)
type t =
  { fields: fields  (** non-static fields *)
  ; statics: fields  (** static fields *)
  ; supers: Typ.Name.t list  (** superclasses *)
  ; objc_protocols: Typ.Name.t list  (** ObjC protocols *)
  ; methods: Procname.t list  (** methods defined *)
  ; exported_objc_methods: Procname.t list  (** methods in ObjC interface, subset of [methods] *)
  ; annots: Annot.Item.t  (** annotations *)
  ; java_class_info: java_class_info option  (** present if and only if the class is Java *)
  ; dummy: bool  (** dummy struct for class including static method *)
  ; source_file: SourceFile.t option  (** source file containing this struct's declaration *) }
[@@deriving equal]

type lookup = Typ.Name.t -> t option

let pp_field pe f (field_name, typ, ann) =
  F.fprintf f "@\n\t\t%a %a %a" (Typ.pp_full pe) typ Fieldname.pp field_name Annot.Item.pp ann


let pp pe name f
    ({ fields
     ; statics
     ; supers
     ; objc_protocols
     ; methods
     ; exported_objc_methods
     ; annots
     ; java_class_info
     ; dummy
     ; source_file } [@warning "+missing-record-field-pattern"] ) =
  let pp_field pe f (field_name, typ, ann) =
    F.fprintf f "@;<0 2>%a %a %a" (Typ.pp_full pe) typ Fieldname.pp field_name Annot.Item.pp ann
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
     java_class_info: {@[<v>%a@]}@,\
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
    (seq (fun f m -> F.fprintf f "@;<0 2>%a" Procname.pp m))
    methods
    (seq (fun f m -> F.fprintf f "@;<0 2>%a" Procname.pp m))
    exported_objc_methods Annot.Item.pp annots pp_java_class_info_opt java_class_info dummy
    (fun fs -> Option.iter ~f:(Format.fprintf fs "source_file: %a@," SourceFile.pp))
    source_file


let compare_custom_field (fld, _, _) (fld', _, _) = Fieldname.compare fld fld'

let make_java_struct fields' statics' methods' supers' annots' java_class_info ?source_file dummy =
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
  ; java_class_info
  ; dummy
  ; source_file }


let internal_mk_struct ?default ?fields ?statics ?methods ?exported_objc_methods ?supers
    ?objc_protocols ?annots ?java_class_info ?dummy ?source_file typename =
  let default_ =
    { fields= []
    ; statics= []
    ; methods= []
    ; exported_objc_methods= []
    ; supers= []
    ; objc_protocols= []
    ; annots= Annot.Item.empty
    ; java_class_info= None
    ; dummy= false
    ; source_file= None }
  in
  let mk_struct_ ?(default = default_) ?(fields = default.fields) ?(statics = default.statics)
      ?(methods = default.methods) ?(exported_objc_methods = default.exported_objc_methods)
      ?(supers = default.supers) ?(objc_protocols = default.objc_protocols)
      ?(annots = default.annots) ?(dummy = default.dummy) ?source_file typename =
    match typename with
    | Typ.JavaClass _jclass ->
        make_java_struct fields statics methods supers annots java_class_info ?source_file dummy
    | _ ->
        { fields
        ; statics
        ; methods
        ; exported_objc_methods
        ; supers
        ; objc_protocols
        ; annots
        ; java_class_info
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
      | Some (_, fld_typ, _) ->
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
        List.find ~f:(fun (f, _, _) -> Fieldname.equal f fn) fields |> Option.map ~f:snd3
    | None ->
        None )
  | _ ->
      None


(** If a struct type with field f, return the type of f. If not, return the default *)
let fld_typ ~lookup ~default fn (typ : Typ.t) = Option.value (fld_typ_opt ~lookup fn typ) ~default

type field_info = {typ: Typ.t; annotations: Annot.Item.t; is_static: bool}

let find_field field_list field_name_to_lookup =
  List.find_map
    ~f:(fun (field_name, typ, annotations) ->
      if Fieldname.equal field_name field_name_to_lookup then Some (typ, annotations) else None )
    field_list


let get_field_info ~lookup field_name_to_lookup (typ : Typ.t) =
  let find_field_info field_list ~is_static =
    find_field field_list field_name_to_lookup
    |> Option.map ~f:(fun (typ, annotations) -> {typ; annotations; is_static})
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


let merge_dedup_sorted_lists ~compare lhs rhs =
  let rec merge_dedup_sorted_lists_inner ~compare prev_opt lhs rhs =
    match (lhs, rhs, prev_opt) with
    | [], ys, _ ->
        ys
    | xs, [], _ ->
        xs
    | x :: xs, ys, Some last when Int.equal 0 (compare last x) ->
        merge_dedup_sorted_lists_inner ~compare prev_opt xs ys
    | xs, y :: ys, Some last when Int.equal 0 (compare last y) ->
        merge_dedup_sorted_lists_inner ~compare prev_opt xs ys
    | x :: xs, y :: ys, prev_opt -> (
      match compare x y with
      | 0 ->
          (* first elements equal, drop lhs first and continue, keeping same [prev_opt] *)
          merge_dedup_sorted_lists_inner ~compare prev_opt xs rhs
      | r when r < 0 ->
          (* first of lhs < first of rhs, keep [x] *)
          x :: merge_dedup_sorted_lists_inner ~compare (Some x) xs rhs
      | _ ->
          (* first of lhs > first of rhs, keep [y] *)
          y :: merge_dedup_sorted_lists_inner ~compare (Some y) lhs ys )
  in
  merge_dedup_sorted_lists_inner ~compare None lhs rhs


let merge_lists ~compare ~newer ~current =
  match (newer, current) with
  | [], _ ->
      current
  | _, [] ->
      newer
  | _, _ when is_subsumed ~compare newer current ->
      current
  | _, _ ->
      merge_dedup_sorted_lists ~compare newer current


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
  let kind = merge_kind ~newer:newer.kind ~current:current.kind in
  let loc = merge_loc_opt ~newer:newer.loc ~current:current.loc in
  if phys_equal kind current.kind && phys_equal loc current.loc then current else {kind; loc}


let merge_java_class_info_opt ~newer ~current =
  merge_opt ~merge:merge_java_class_info ~newer ~current


let full_merge ~newer ~current =
  let fields = merge_fields ~newer:newer.fields ~current:current.fields in
  let statics = merge_fields ~newer:newer.statics ~current:current.statics in
  let supers = merge_supers ~newer:newer.supers ~current:current.supers in
  let methods = merge_methods ~newer:newer.methods ~current:current.methods in
  (* we are merging only Java and Hack classes, so [exported_obj_methods] should be empty, so no
     merge *)
  let annots = merge_annots ~newer:newer.annots ~current:current.annots in
  let java_class_info =
    merge_java_class_info_opt ~newer:newer.java_class_info ~current:current.java_class_info
  in
  if
    phys_equal fields current.fields
    && phys_equal statics current.statics
    && phys_equal supers current.supers
    && phys_equal methods current.methods
    && phys_equal annots current.annots
    && phys_equal java_class_info current.java_class_info
  then current
  else {current with fields; statics; supers; methods; annots; java_class_info}


let merge typename ~newer ~current =
  match (typename : Typ.Name.t) with
  | CStruct _ | CUnion _ | ErlangType _ | ObjcClass _ | ObjcProtocol _ | CppClass _ ->
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


let is_not_java_interface = function
  | {java_class_info= Some {kind= Interface}} ->
      false
  | _ ->
      true


module FieldNormalizer = HashNormalizer.Make (struct
  type t = field [@@deriving equal]

  let hash = Hashtbl.hash

  let normalize f =
    let field_name, typ, annot = f in
    let field_name' = Fieldname.Normalizer.normalize field_name in
    let typ' = Typ.Normalizer.normalize typ in
    let annot' = Annot.Item.Normalizer.normalize annot in
    if phys_equal field_name field_name' && phys_equal typ typ' && phys_equal annot annot' then f
    else (field_name', typ', annot')
end)

module JavaClassInfoOptNormalizer = HashNormalizer.Make (struct
  type t = java_class_info option [@@deriving equal]

  let hash = Hashtbl.hash

  let normalize_location_opt loc_opt =
    IOption.map_changed loc_opt ~equal:phys_equal ~f:Location.Normalizer.normalize


  let normalize_java_class_info java_class_info =
    let loc = normalize_location_opt java_class_info.loc in
    if phys_equal loc java_class_info.loc then java_class_info else {java_class_info with loc}


  let normalize java_class_info_opt =
    IOption.map_changed java_class_info_opt ~equal:phys_equal ~f:normalize_java_class_info
end)

module Normalizer = HashNormalizer.Make (struct
  type nonrec t = t [@@deriving equal]

  let hash = Hashtbl.hash

  let normalize t =
    let fields = IList.map_changed ~equal:phys_equal ~f:FieldNormalizer.normalize t.fields in
    let statics = IList.map_changed ~equal:phys_equal ~f:FieldNormalizer.normalize t.statics in
    let supers = IList.map_changed ~equal:phys_equal ~f:Typ.NameNormalizer.normalize t.supers in
    let objc_protocols =
      IList.map_changed ~equal:phys_equal ~f:Typ.NameNormalizer.normalize t.objc_protocols
    in
    let methods = IList.map_changed ~equal:phys_equal ~f:Procname.Normalizer.normalize t.methods in
    let exported_objc_methods =
      IList.map_changed ~equal:phys_equal ~f:Procname.Normalizer.normalize t.exported_objc_methods
    in
    let annots = Annot.Item.Normalizer.normalize t.annots in
    let java_class_info = JavaClassInfoOptNormalizer.normalize t.java_class_info in
    let source_file =
      IOption.map_changed ~equal:phys_equal ~f:SourceFile.Normalizer.normalize t.source_file
    in
    if
      phys_equal fields t.fields && phys_equal statics t.statics && phys_equal supers t.supers
      && phys_equal objc_protocols t.objc_protocols
      && phys_equal methods t.methods
      && phys_equal exported_objc_methods t.exported_objc_methods
      && phys_equal annots t.annots
      && phys_equal java_class_info t.java_class_info
      && phys_equal source_file t.source_file
    then t
    else
      { fields
      ; statics
      ; supers
      ; objc_protocols
      ; methods
      ; exported_objc_methods
      ; annots
      ; java_class_info
      ; dummy= t.dummy
      ; source_file }
end)
