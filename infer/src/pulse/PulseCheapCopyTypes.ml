(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let is_small_type typ = Typ.is_trivially_copyable typ.Typ.quals

let is_pair_small_type typ1 typ2 = is_small_type typ1 && is_small_type typ2

let dispatch : (unit, bool, unit) ProcnameDispatcher.TypName.dispatcher =
  let open ProcnameDispatcher.TypName in
  make_dispatcher
    [ -"folly" &:: "Optional" < capt_typ >--> is_small_type
    ; -"std" &:: "filesystem" &:: "directory_iterator" <>--> true
    ; -"std" &:: "optional" < capt_typ >--> is_small_type
    ; -"std" &:: "pair" < capt_typ &+ capt_typ >--> is_pair_small_type ]


let is_known_cheap_copy typ_name = dispatch () typ_name |> Option.value ~default:false
