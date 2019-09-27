(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type t = {nullability: AnnotatedNullability.t; typ: Typ.t} [@@deriving compare]

let pp fmt {nullability; typ} =
  Format.fprintf fmt "%a %a" AnnotatedNullability.pp nullability (Typ.pp_full Pp.text) typ
