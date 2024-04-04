(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! Core
module F = Format
include Caml.Set.Make (Int)

let pp fmt set =
  let is_first = ref true in
  F.fprintf fmt "@[<h>{%a}@]"
    (fun fmt set ->
      iter
        (fun elt ->
          if not !is_first then F.pp_print_char fmt ',' ;
          is_first := false ;
          F.pp_print_int fmt elt )
        set )
    set
