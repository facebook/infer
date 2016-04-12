(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils


let rec log fmt =
  if !JConfig.debug_mode then
    Logging.stdout fmt
  else
    Obj.magic log

(*
open Javalib_pack

let get_native_methods program =
  let select_native m l =
    let is_native cm = (cm.Javalib.cm_implementation = Javalib.Native) in
    match m with
    | Javalib.ConcreteMethod cm when is_native cm ->
        let (cn, ms) = JBasics.cms_split cm.Javalib.cm_class_method_signature in
        ((JBasics.cn_name cn)^"."^(JBasics.ms_name ms)):: l
    | _ -> l in
  let collect _ node l =
    (Javalib.m_fold select_native node l) in
  JBasics.ClassMap.fold collect program []
*)
