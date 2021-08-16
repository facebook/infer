(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type typ_model =
  | CArray of {element_typ: Typ.t; deref_kind: Symb.SymbolPath.deref_kind; length: IntLit.t}
  | CppStdVector
  | JavaCollection
  | JavaInteger

let std_array element_typ length =
  CArray {element_typ; deref_kind= Symb.SymbolPath.Deref_ArrayIndex; length= IntLit.of_int64 length}


let std_vector = CppStdVector

(* Java's Collections are represented by their size. We don't care about the elements.
   - when they are constructed, we set the size to 0
   - each time we add an element, we increase the length of the array
   - each time we delete an element, we decrease the length of the array *)

module Java = struct
  let collection = JavaCollection

  let integer = JavaInteger
end

let dispatch : (Tenv.t, typ_model, unit) ProcnameDispatcher.TypName.dispatcher =
  let open ProcnameDispatcher.TypName in
  make_dispatcher
    [ -"std" &:: "array" < capt_typ &+ capt_int >--> std_array
    ; -"std" &:: "vector" < any_typ &+ any_typ >--> std_vector
    ; +PatternMatch.Java.implements_collection &::.*--> Java.collection
    ; +PatternMatch.Java.implements_iterator &::.*--> Java.collection
    ; +PatternMatch.Java.implements_map &::.*--> Java.collection
    ; +PatternMatch.Java.implements_pseudo_collection &::.*--> Java.collection
    ; +PatternMatch.Java.implements_nio "Buffer" &::.*--> Java.collection
    ; +PatternMatch.Java.implements_lang "Integer" &::.*--> Java.integer
    ; +PatternMatch.Java.implements_org_json "JSONArray" &::.*--> Java.collection ]
