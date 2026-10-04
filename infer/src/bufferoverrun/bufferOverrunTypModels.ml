(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type typ_model =
  | CArray of
      { element_typ: Typ.t
      ; deref_kind: Symb.SymbolPath.deref_kind
      ; length: IntLit.t
      ; stride: int option }
  | CppStdVector
  | JavaCollection
  | JavaInteger

let std_array element_typ length =
  CArray
    { element_typ
    ; deref_kind= Symb.SymbolPath.Deref_ArrayIndex
    ; length= IntLit.of_int64 length
    ; stride= None }


let std_vector = CppStdVector

(* Java's Collections are represented by their size. We don't care about the elements.
   - when they are constructed, we set the size to 0
   - each time we add an element, we increase the length of the array
   - each time we delete an element, we decrease the length of the array *)

module Java = struct
  let collection = JavaCollection

  let integer = JavaInteger
end

let dispatch_name : (Tenv.t, typ_model, unit) ProcnameDispatcher.TypName.dispatcher =
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


(* The element size of [std::array<T, N>] is not part of its name, but it is the stride of its
   array field of length [N] holding the elements ([__elems_] in libc++, [_M_elems] in
   libstdc++). *)
let get_elements_stride tenv typname length =
  let open IOption.Let_syntax in
  let* {Struct.fields} = Tenv.lookup tenv typname in
  List.find_map fields ~f:(fun ({typ} : Struct.field) ->
      match typ.Typ.desc with
      | Tarray {length= Some n; stride= Some stride}
        when IntLit.eq n length && not (IntLit.iszero stride) ->
          Some (IntLit.to_int_exn stride)
      | _ ->
          None )


let dispatch tenv typname =
  match dispatch_name tenv typname with
  | Some (CArray ({length} as carray)) ->
      Some (CArray {carray with stride= get_elements_stride tenv typname length})
  | model ->
      model
