(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type source = ReturnValue of Procname.t [@@deriving compare, equal]

let pp_source fmt (ReturnValue proc_name) =
  F.fprintf fmt "value returned from %a marked as sensitive" Procname.pp proc_name


type sink = PassedAsArgumentTo of Procname.t
(* TODO: name of the formal / actual *) [@@deriving compare, equal]

let pp_sink fmt (PassedAsArgumentTo proc_name) =
  F.fprintf fmt "passed as argument to sensitive function %a" Procname.pp proc_name


type sanitizer = SanitizedBy of Procname.t [@@deriving compare, equal]

let pp_sanitizer fmt (SanitizedBy proc_name) =
  F.fprintf fmt "passed as argument to sanitizer function %a" Procname.pp proc_name
