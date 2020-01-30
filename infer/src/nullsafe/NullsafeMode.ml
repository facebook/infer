(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t = Default | Strict [@@deriving compare, equal]

let of_class tenv typ_name =
  match PatternMatch.type_name_get_annotation tenv typ_name with
  | Some annots ->
      if Annotations.ia_is_nullsafe_strict annots then Strict else Default
  | None ->
      Default


let severity = function Strict -> Exceptions.Error | Default -> Exceptions.Warning

let to_string = function Default -> " Def" | Strict -> "Strict"

let pp fmt t = F.fprintf fmt "%s" (to_string t)
