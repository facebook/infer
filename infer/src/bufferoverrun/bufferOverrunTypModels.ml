(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type typ_model =
  | CArray of {element_typ: Typ.t; deref_kind: Symb.SymbolPath.deref_kind; length: IntLit.t}
  | JavaCollection

let std_array element_typ length =
  CArray {element_typ; deref_kind= Symb.SymbolPath.Deref_ArrayIndex; length= IntLit.of_int64 length}


(* Java's Collections are represented by their size. We don't care about the elements.
- when they are constructed, we set the size to 0
- each time we add an element, we increase the length of the array
- each time we delete an element, we decrease the length of the array *)

let collection = JavaCollection

let dispatch : (Tenv.t, typ_model) ProcnameDispatcher.TypName.dispatcher =
  let open ProcnameDispatcher.TypName in
  make_dispatcher
    [ -"std" &:: "array" < capt_typ `T &+ capt_int >--> std_array
    ; +PatternMatch.implements_collection &::.*--> collection
    ; +PatternMatch.implements_iterator &::.*--> collection ]
