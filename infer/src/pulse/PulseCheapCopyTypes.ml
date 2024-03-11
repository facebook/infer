(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let is_small_type typ tenv = Tenv.is_trivially_copyable tenv typ

let is_pair_small_type typ1 typ2 tenv = is_small_type typ1 tenv && is_small_type typ2 tenv

let dispatch : (unit, Tenv.t -> bool, unit) ProcnameDispatcher.TypName.dispatcher =
  let open ProcnameDispatcher.TypName in
  make_dispatcher
    [ (-"folly" &:: "FunctionRef" &--> fun _ -> true)
    ; -"folly" &:: "Optional" < capt_typ >--> is_small_type
    ; (-"std" &:: "filesystem" &:: "directory_iterator" <>--> fun _ -> true)
    ; (-"std" &:: "filesystem" &:: "recursive_directory_iterator" <>--> fun _ -> true)
    ; -"std" &:: "optional" < capt_typ >--> is_small_type
    ; -"std" &:: "pair" < capt_typ &+ capt_typ >--> is_pair_small_type ]


let is_known_cheap_copy tenv typ_name =
  Option.value_map (dispatch () typ_name) ~default:false ~f:(fun f -> f tenv)
