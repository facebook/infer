(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let no_return = true

let dispatch : (Tenv.t, bool, unit) ProcnameDispatcher.ProcName.dispatcher =
  let open ProcnameDispatcher.ProcName in
  make_dispatcher
    [ +PatternMatch.Java.implements_lang "System" &:: "exit" <>--> no_return
    ; +PatternMatch.Java.implements_lang "Runtime" &:: "exit" <>--> no_return ]
