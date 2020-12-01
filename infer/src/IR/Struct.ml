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

type java_class_kind = Interface | AbstractClass | NormalClass [@@deriving equal, compare]

let pp_java_class_kind fmt kind =
  F.pp_print_string fmt
    ( match kind with
    | Interface ->
        "Interface"
    | AbstractClass ->
        "AbstractClass"
    | NormalClass ->
        "NormalClass" )


type java_class_info =
  { kind: java_class_kind  (** class kind in Java *)
  ; loc: Location.t option
        (** None should correspond to rare cases when it was impossible to fetch the location in
            source file *) }
[@@deriving equal]

let pp_java_class_info fmt {kind; loc} =
  F.fprintf fmt "{kind= %a; loc= %a}" pp_java_class_kind kind (Pp.option Location.pp) loc


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
  ; dummy: bool  (** dummy struct for class including static method *) }
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
     ; dummy }[@warning "+9"]) =
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
     dummy: %b@]@,"
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


let internal_mk_struct ?default ?fields ?statics ?methods ?exported_objc_methods ?supers
    ?objc_protocols ?annots ?java_class_info ?dummy () =
  let default_ =
    { fields= []
    ; statics= []
    ; methods= []
    ; exported_objc_methods= []
    ; supers= []
    ; objc_protocols= []
    ; annots= Annot.Item.empty
    ; java_class_info= None
    ; dummy= false }
  in
  let mk_struct_ ?(default = default_) ?(fields = default.fields) ?(statics = default.statics)
      ?(methods = default.methods) ?(exported_objc_methods = default.exported_objc_methods)
      ?(supers = default.supers) ?(objc_protocols = default.objc_protocols)
      ?(annots = default.annots) ?(dummy = default.dummy) () =
    { fields
    ; statics
    ; methods
    ; exported_objc_methods
    ; supers
    ; objc_protocols
    ; annots
    ; java_class_info
    ; dummy }
  in
  mk_struct_ ?default ?fields ?statics ?methods ?exported_objc_methods ?supers ?objc_protocols
    ?annots ?dummy ()


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


(** If a struct type with field f, return the type of f. If not, return the default *)
let fld_typ ~lookup ~default fn (typ : Typ.t) =
  (* Note: would be nice migrate it to get_field_info
     (for that one needs to ensure adding Tptr to pattern match does not break thing) *)
  match typ.desc with
  | Tstruct name -> (
    match lookup name with
    | Some {fields} ->
        List.find ~f:(fun (f, _, _) -> Fieldname.equal f fn) fields
        |> Option.value_map ~f:snd3 ~default
    | None ->
        default )
  | _ ->
      default


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

let merge_lists ~compare ~newer ~current =
  let equal x y = Int.equal 0 (compare x y) in
  match (newer, current) with
  | [], _ ->
      current
  | _, [] ->
      newer
  | _, _ when List.equal equal newer current ->
      newer
  | _, _ ->
      List.dedup_and_sort ~compare (newer @ current)


let merge_fields ~newer ~current = merge_lists ~compare:compare_field ~newer ~current

let merge_supers ~newer ~current = merge_lists ~compare:Typ.Name.compare ~newer ~current

let merge_methods ~newer ~current = merge_lists ~compare:Procname.compare ~newer ~current

let merge_annots ~newer ~current = merge_lists ~compare:[%compare: Annot.t * bool] ~newer ~current

let merge_kind ~newer ~current =
  (* choose the maximal, ie most concrete *)
  if compare_java_class_kind newer current < 0 then current else newer


(* choose [Some] option if possible, [newer] if both [None] else [merge] *)
let merge_opt ~merge ~newer ~current =
  match (newer, current) with
  | _, None ->
      newer
  | None, _ ->
      current
  | Some newer, Some current ->
      Some (merge ~newer ~current)


let merge_loc ~newer ~current =
  if Location.equal Location.dummy newer then current
  else if Location.equal Location.dummy current then newer
  else if (* arbitrarily but deterministically choose one *) Location.compare newer current <= 0
  then newer
  else current


let merge_loc_opt ~newer ~current = merge_opt ~merge:merge_loc ~newer ~current

let merge_java_class_info ~newer ~current =
  { kind= merge_kind ~newer:newer.kind ~current:current.kind
  ; loc= merge_loc_opt ~newer:newer.loc ~current:current.loc }


let merge_java_class_info_opt ~newer ~current =
  merge_opt ~merge:merge_java_class_info ~newer ~current


let full_merge ~newer ~current =
  let fields = merge_fields ~newer:newer.fields ~current:current.fields in
  let statics = merge_fields ~newer:newer.statics ~current:current.statics in
  let supers = merge_supers ~newer:newer.supers ~current:current.supers in
  (* the semantics of [subs] is such that no merging is attempted *)
  let methods = merge_methods ~newer:newer.methods ~current:current.methods in
  (* we are merging only Java classes, so [exported_obj_methods] should be empty, so no merge *)
  let annots = merge_annots ~newer:newer.annots ~current:current.annots in
  let java_class_info =
    merge_java_class_info_opt ~newer:newer.java_class_info ~current:current.java_class_info
  in
  {newer with fields; statics; supers; methods; annots; java_class_info}


let merge typename ~newer ~current =
  match (typename : Typ.Name.t) with
  | CStruct _ | CUnion _ | ObjcClass _ | ObjcProtocol _ | CppClass _ ->
      if not (is_dummy newer) then newer else current
  | JavaClass _ when is_dummy newer ->
      current
  | JavaClass _ when is_dummy current ->
      newer
  | JavaClass _ when equal newer current ->
      newer
  | JavaClass _ ->
      full_merge ~newer ~current
  | CSharpClass _ when is_dummy newer ->
      current
  | CSharpClass _ when is_dummy current ->
      newer
  | CSharpClass _ when equal newer current ->
      newer
  | CSharpClass _ ->
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

module Normalizer = struct
  include HashNormalizer.Make (struct
    type nonrec t = t [@@deriving equal]

    let hash = Hashtbl.hash

    let normalize t =
      let fields = IList.map_changed ~equal:phys_equal ~f:FieldNormalizer.normalize t.fields in
      let statics = IList.map_changed ~equal:phys_equal ~f:FieldNormalizer.normalize t.statics in
      let supers = IList.map_changed ~equal:phys_equal ~f:Typ.Name.Normalizer.normalize t.supers in
      let objc_protocols =
        IList.map_changed ~equal:phys_equal ~f:Typ.Name.Normalizer.normalize t.objc_protocols
      in
      let methods =
        IList.map_changed ~equal:phys_equal ~f:Procname.Normalizer.normalize t.methods
      in
      let exported_objc_methods =
        IList.map_changed ~equal:phys_equal ~f:Procname.Normalizer.normalize t.exported_objc_methods
      in
      let annots = Annot.Item.Normalizer.normalize t.annots in
      let java_class_info = JavaClassInfoOptNormalizer.normalize t.java_class_info in
      if
        phys_equal fields t.fields && phys_equal statics t.statics && phys_equal supers t.supers
        && phys_equal objc_protocols t.objc_protocols
        && phys_equal methods t.methods
        && phys_equal exported_objc_methods t.exported_objc_methods
        && phys_equal annots t.annots
        && phys_equal java_class_info t.java_class_info
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
        ; dummy= t.dummy }
  end)

  let reset () =
    reset () ;
    FieldNormalizer.reset () ;
    JavaClassInfoOptNormalizer.reset ()
end
