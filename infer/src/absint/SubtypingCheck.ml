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


let rec case_analysis_type tenv ((t1 : Typ.t), st1) ((t2 : Typ.t), st2) =
  match (t1.desc, t2.desc) with
  | Tstruct (JavaClass _ as cn1), Tstruct (JavaClass _ as cn2) ->
      Subtype.case_analysis tenv (cn1, st1) (cn2, st2)
  | Tstruct (JavaClass _ as cn1), Tarray _
    when ( Typ.Name.equal cn1 StdTyp.Name.Java.java_io_serializable
         || Typ.Name.equal cn1 StdTyp.Name.Java.java_lang_cloneable
         || Typ.Name.equal cn1 StdTyp.Name.Java.java_lang_object )
         && not (Subtype.equal st1 Subtype.exact) ->
      (Some st1, None)
  | Tstruct cn1, Tstruct cn2
  (* cn1 <: cn2 or cn2 <: cn1 is implied in Java when we get two types compared *)
  (* that get through the type system, but not in C++ because of multiple inheritance, *)
  (* and not in ObjC because of being weakly typed, *)
  (* and the algorithm will only work correctly if this is the case *)
    when Subtype.is_known_subtype tenv cn1 cn2 || Subtype.is_known_subtype tenv cn2 cn1 ->
      Subtype.case_analysis tenv (cn1, st1) (cn2, st2)
  | Tarray {elt= dom_type1}, Tarray {elt= dom_type2} ->
      case_analysis_type tenv (dom_type1, st1) (dom_type2, st2)
  | Tptr (dom_type1, _), Tptr (dom_type2, _) ->
      case_analysis_type tenv (dom_type1, st1) (dom_type2, st2)
  | _ when check_subtype_basic_type t1 t2 ->
      (Some st1, None)
  | _ ->
      (* The case analysis did not succeed *)
      (None, Some st1)


(** perform case analysis on [texp1 <: texp2], and return the updated types in the true and false
    case, if they are possible *)
let subtype_case_analysis tenv texp1 texp2 =
  match (texp1, texp2) with
  | Exp.Sizeof sizeof1, Exp.Sizeof sizeof2 ->
      let pos_opt, neg_opt =
        case_analysis_type tenv (sizeof1.typ, sizeof1.subtype) (sizeof2.typ, sizeof2.subtype)
      in
      let pos_type_opt =
        match pos_opt with
        | None ->
            None
        | Some subtype ->
            if check_subtype tenv sizeof1.typ sizeof2.typ then
              Some (Exp.Sizeof {sizeof1 with subtype})
            else Some (Exp.Sizeof {sizeof2 with subtype})
      in
      let neg_type_opt =
        match neg_opt with None -> None | Some subtype -> Some (Exp.Sizeof {sizeof1 with subtype})
      in
      (pos_type_opt, neg_type_opt)
  | _ ->
      (* don't know, consider both possibilities *)
      (Some texp1, Some texp1)
