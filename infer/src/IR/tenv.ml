(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

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

exception Cannot_convert_string_to_typ of string

(** Lookup Java types by name *)
let lookup_java_typ_from_string tenv typ_str =
  let rec loop = function
    | "" | "void" -> Sil.Tvoid
    | "int" -> Sil.Tint Sil.IInt
    | "byte" -> Sil.Tint Sil.IShort
    | "short" -> Sil.Tint Sil.IShort
    | "boolean" -> Sil.Tint Sil.IBool
    | "char" -> Sil.Tint Sil.IChar
    | "long" -> Sil.Tint Sil.ILong
    | "float" -> Sil.Tfloat Sil.FFloat
    | "double" -> Sil.Tfloat Sil.FDouble
    | typ_str when String.contains typ_str '[' ->
        let stripped_typ = String.sub typ_str 0 ((String.length typ_str) - 2) in
        let array_typ_size = Sil.exp_get_undefined false in
        Sil.Tptr (Sil.Tarray (loop stripped_typ, array_typ_size), Sil.Pk_pointer)
    | typ_str ->
        (* non-primitive/non-array type--resolve it in the tenv *)
        let typename = Typename.TN_csu (Csu.Class Csu.Java, (Mangled.from_string typ_str)) in
        match lookup tenv typename with
        | Some struct_typ -> Sil.Tstruct struct_typ
        | _ -> raise (Cannot_convert_string_to_typ typ_str) in
  loop typ_str

(** Add a (name,type) pair to the global type environment. *)
let add tenv name struct_typ =
  TypenameHash.replace tenv name struct_typ

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
