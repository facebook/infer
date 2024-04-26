(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let dispatch : (unit, unit, unit) ProcnameDispatcher.TypName.dispatcher =
  let open ProcnameDispatcher.TypName in
  make_dispatcher
    [ -"folly" &:: "Optional" &::.*--> ()
    ; -"folly" &:: "small_vector" &::.*--> ()
    ; -"std" &:: "__wrap_iter" &::.*--> ()
    ; -"std" &:: "atomic" &::.*--> ()
    ; -"std" &:: "function" &::.*--> ()
    ; -"std" &:: "optional" &::.*--> ()
    ; -"std" &:: "vector" &::.*--> () ]


let is_blocklisted_struct typ_name = dispatch () typ_name |> Option.is_some
