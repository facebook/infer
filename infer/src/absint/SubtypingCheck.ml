(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let rec is_java_class tenv (typ : Typ.t) =
  match typ.desc with
  | Tstruct name ->
      Typ.Name.Java.is_class name
  | Tarray {elt= inner_typ} | Tptr (inner_typ, _) ->
      is_java_class tenv inner_typ
  | _ ->
      false


(** check that t1 and t2 are the same primitive type *)
let check_subtype_basic_type t1 t2 =
  match t2.Typ.desc with
  | Typ.Tint Typ.IInt
  | Typ.Tint Typ.IBool
  | Typ.Tint Typ.IChar
  | Typ.Tfloat Typ.FDouble
  | Typ.Tfloat Typ.FFloat
  | Typ.Tint Typ.ILong
  | Typ.Tint Typ.IShort ->
      Typ.equal t1 t2
  | _ ->
      false


(** check if t1 is a subtype of t2, in Java *)
let rec check_subtype_java tenv (t1 : Typ.t) (t2 : Typ.t) =
  match (t1.Typ.desc, t2.Typ.desc) with
  | Tstruct (JavaClass _ as cn1), Tstruct (JavaClass _ as cn2) ->
      Subtype.is_known_subtype tenv cn1 cn2
  | Tarray {elt= dom_type1}, Tarray {elt= dom_type2} ->
      check_subtype_java tenv dom_type1 dom_type2
  | Tptr (dom_type1, _), Tptr (dom_type2, _) ->
      check_subtype_java tenv dom_type1 dom_type2
  | Tarray _, Tstruct (JavaClass _ as cn2) ->
      Typ.Name.equal cn2 StdTyp.Name.Java.java_io_serializable
      || Typ.Name.equal cn2 StdTyp.Name.Java.java_lang_cloneable
      || Typ.Name.equal cn2 StdTyp.Name.Java.java_lang_object
  | _ ->
      check_subtype_basic_type t1 t2


(** check if t1 is a subtype of t2 *)
let check_subtype tenv t1 t2 =
  if is_java_class tenv t1 then check_subtype_java tenv t1 t2
  else
    match (Typ.name t1, Typ.name t2) with
    | Some cn1, Some cn2 ->
        Subtype.is_known_subtype tenv cn1 cn2
    | _ ->
        false
