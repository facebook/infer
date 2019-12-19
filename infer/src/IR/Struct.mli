(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type field = Fieldname.t * Typ.t * Annot.Item.t [@@deriving compare]

type fields = field list

(** Type for a structured value. *)
type t = private
  { fields: fields  (** non-static fields *)
  ; statics: fields  (** static fields *)
  ; supers: Typ.Name.t list  (** supers *)
  ; methods: Typ.Procname.t list  (** methods defined *)
  ; exported_objc_methods: Typ.Procname.t list
        (** methods in ObjC interface, subset of [methods] *)
  ; annots: Annot.Item.t  (** annotations *)
  ; dummy: bool  (** dummy struct for class including static method *) }

type lookup = Typ.Name.t -> t option

val pp_field : Pp.env -> F.formatter -> field -> unit

val pp : Pp.env -> Typ.Name.t -> F.formatter -> t -> unit
(** Pretty print a struct type. *)

val internal_mk_struct :
     ?default:t
  -> ?fields:fields
  -> ?statics:fields
  -> ?methods:Typ.Procname.t list
  -> ?exported_objc_methods:Typ.Procname.t list
  -> ?supers:Typ.Name.t list
  -> ?annots:Annot.Item.t
  -> ?dummy:bool
  -> unit
  -> t
(** Construct a struct_typ, normalizing field types *)

val get_extensible_array_element_typ : lookup:lookup -> Typ.t -> Typ.t option
(** the element typ of the final extensible array in the given typ, if any *)

type field_info = {typ: Typ.t; annotations: Annot.Item.t; is_static: bool}

val get_field_info : lookup:lookup -> Fieldname.t -> Typ.t -> field_info option
(** Lookup for info associated with the field [fn]. None if [typ] has no field named [fn] *)

val fld_typ : lookup:lookup -> default:Typ.t -> Fieldname.t -> Typ.t -> Typ.t
(** If a struct type with field f, return the type of f. If not, return the default type if given,
    otherwise raise an exception *)

val get_field_type_and_annotation :
  lookup:lookup -> Fieldname.t -> Typ.t -> (Typ.t * Annot.Item.t) option
(** Return the type of the field [fn] and its annotation, None if [typ] has no field named [fn] *)

val is_dummy : t -> bool
