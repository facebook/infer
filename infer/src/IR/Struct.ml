(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type field = Fieldname.t * Typ.t * Annot.Item.t [@@deriving compare]

type fields = field list

type java_class_kind = Interface | AbstractClass | NormalClass [@@deriving equal]

type java_class_info =
  { kind: java_class_kind  (** class kind in Java *)
  ; loc: Location.t option
        (** None should correspond to rare cases when it was impossible to fetch the location in
            source file *) }

(** Type for a structured value. *)
type t =
  { fields: fields  (** non-static fields *)
  ; statics: fields  (** static fields *)
  ; supers: Typ.Name.t list  (** superclasses *)
  ; subs: Typ.Name.Set.t  (** subclasses, initialized after merging type environments *)
  ; methods: Procname.t list  (** methods defined *)
  ; exported_objc_methods: Procname.t list  (** methods in ObjC interface, subset of [methods] *)
  ; annots: Annot.Item.t  (** annotations *)
  ; java_class_info: java_class_info option  (** present if and only if the class is Java *)
  ; dummy: bool  (** dummy struct for class including static method *) }

type lookup = Typ.Name.t -> t option

let pp_field pe f (field_name, typ, ann) =
  F.fprintf f "@\n\t\t%a %a %a" (Typ.pp_full pe) typ Fieldname.pp field_name Annot.Item.pp ann


let pp pe name f {fields; supers; methods; exported_objc_methods; annots} =
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
     supers: {@[<v>%a@]}@,\
     methods: {@[<v>%a@]}@,\
     exported_obj_methods: {@[<v>%a@]}@,\
     annots: {%a}@]@,"
    Typ.Name.pp name
    (seq (pp_field pe))
    fields
    (seq (fun f n -> F.fprintf f "@;<0 2>%a" Typ.Name.pp n))
    supers
    (seq (fun f m -> F.fprintf f "@;<0 2>%a" Procname.pp m))
    methods
    (seq (fun f m -> F.fprintf f "@;<0 2>%a" Procname.pp m))
    exported_objc_methods Annot.Item.pp annots


let internal_mk_struct ?default ?fields ?statics ?methods ?exported_objc_methods ?supers ?annots
    ?java_class_info ?dummy () =
  let default_ =
    { fields= []
    ; statics= []
    ; methods= []
    ; exported_objc_methods= []
    ; supers= []
    ; subs= Typ.Name.Set.empty
    ; annots= Annot.Item.empty
    ; java_class_info= None
    ; dummy= false }
  in
  let mk_struct_ ?(default = default_) ?(fields = default.fields) ?(statics = default.statics)
      ?(methods = default.methods) ?(exported_objc_methods = default.exported_objc_methods)
      ?(supers = default.supers) ?(subs = default.subs) ?(annots = default.annots)
      ?(dummy = default.dummy) () =
    {fields; statics; methods; exported_objc_methods; supers; subs; annots; java_class_info; dummy}
  in
  mk_struct_ ?default ?fields ?statics ?methods ?exported_objc_methods ?supers ?annots ?dummy ()


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

let add_sub sub x = {x with subs= Typ.Name.Set.add sub x.subs}
