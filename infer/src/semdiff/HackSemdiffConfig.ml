(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open SemdiffDirectEngine

let hack_type_annotations_config : Rules.t =
  let open Pattern in
  { ignore= []
  ; rewrite= []
  ; accept=
      [ { lhs= var "T1"
        ; rhs= var "T2"
        ; condition= None
        ; key=
            [ Name.of_string "function_type"
            ; Name.of_string "parameter_type"
            ; Name.of_string "property_type" ] } ] }
