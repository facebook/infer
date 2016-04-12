(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Module for Type Environments. *)

(** Hash tables on strings. *)
module TypenameHash =
  Hashtbl.Make(struct
    type t = Typename.t
    let equal tn1 tn2 = Typename.equal tn1 tn2
    let hash = Hashtbl.hash
  end)

(** Type for type environment. *)
type t = Sil.struct_typ TypenameHash.t

(** Create a new type environment. *)
let create () = TypenameHash.create 1000

(** Check if typename is found in tenv *)
let mem tenv name =
  TypenameHash.mem tenv name

(** Look up a name in the global type environment. *)
let lookup tenv name =
  try Some (TypenameHash.find tenv name)
  with Not_found -> None

(** Lookup Java types by name *)
let lookup_java_typ_from_string tenv typ_str =
  let rec loop = function
    | "" | "void" ->
        Some Sil.Tvoid
    | "int" ->
        Some (Sil.Tint Sil.IInt)
    | "byte" ->
        Some (Sil.Tint Sil.IShort)
    | "short" ->
        Some (Sil.Tint Sil.IShort)
    | "boolean" ->
        Some (Sil.Tint Sil.IBool)
    | "char" ->
        Some (Sil.Tint Sil.IChar)
    | "long" ->
        Some (Sil.Tint Sil.ILong)
    | "float" ->
        Some (Sil.Tfloat Sil.FFloat)
    | "double" ->
        Some (Sil.Tfloat Sil.FDouble)
    | typ_str when String.contains typ_str '[' ->
        let stripped_typ = String.sub typ_str 0 ((String.length typ_str) - 2) in
        let array_typ_size = Sil.exp_get_undefined false in
        begin
          match loop stripped_typ with
          | Some typ -> Some (Sil.Tptr (Sil.Tarray (typ, array_typ_size), Sil.Pk_pointer))
          | None -> None
        end
    | typ_str ->
        (* non-primitive/non-array type--resolve it in the tenv *)
        let typename = Typename.Java.from_string typ_str in
        begin
          match lookup tenv typename with
          | Some struct_typ -> Some (Sil.Tstruct struct_typ)
          | None -> None
        end in
  loop typ_str

(** resolve a type string to a Java *class* type. For strings that may represent primitive or array
    typs, use [lookup_java_typ_from_string] *)
let lookup_java_class_from_string tenv typ_str =
  match lookup_java_typ_from_string tenv typ_str with
  | Some (Sil.Tstruct struct_typ) -> Some struct_typ
  | _ -> None

(** Add a (name,type) pair to the global type environment. *)
let add tenv name struct_typ =
  TypenameHash.replace tenv name struct_typ

(** Return the declaring class type of [pname_java] *)
let proc_extract_declaring_class_typ tenv pname_java =
  lookup_java_class_from_string tenv (Procname.java_get_class_name pname_java)

(** Return the return type of [pname_java]. *)
let proc_extract_return_typ tenv pname_java =
  lookup_java_typ_from_string tenv (Procname.java_get_return_type pname_java)

(** expand a type if it is a typename by looking it up in the type environment *)
let expand_type tenv typ =
  match typ with
  | Sil.Tvar tname ->
      begin
        match lookup tenv tname with
        | None ->
            assert false
        | Some struct_typ ->
            Sil.Tstruct struct_typ
      end
  | _ -> typ

(** Serializer for type environments *)
let tenv_serializer : t Serialization.serializer =
  Serialization.create_serializer Serialization.tenv_key

let global_tenv: (t option) Lazy.t =
  lazy (Serialization.from_file tenv_serializer (DB.global_tenv_fname ()))

(** Load a type environment from a file *)
let load_from_file (filename : DB.filename) : t option =
  if filename = DB.global_tenv_fname () then
    Lazy.force global_tenv
  else
    Serialization.from_file tenv_serializer filename

(** Save a type environment into a file *)
let store_to_file (filename : DB.filename) (tenv : t) =
  Serialization.to_file tenv_serializer filename tenv

let iter f tenv =
  TypenameHash.iter f tenv

let fold f tenv =
  TypenameHash.fold f tenv

let pp fmt (tenv : t) =
  TypenameHash.iter
    (fun name typ ->
       Format.fprintf fmt "@[<6>NAME: %s@." (Typename.to_string name);
       Format.fprintf fmt "@[<6>TYPE: %a@." (Sil.pp_struct_typ pe_text (fun _ () -> ())) typ)
    tenv
