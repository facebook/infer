(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t = Reachable of Procname.t | Extends of Typ.Name.t * Typ.Name.t

let pp fmt = function
  | Reachable proc_name ->
      F.fprintf fmt "Reachable %s" (Procname.to_unique_id proc_name)
  | Extends (cl, cl_super) ->
      F.fprintf fmt "Extends %s %s" (Typ.Name.name cl) (Typ.Name.name cl_super)


let to_string fact = F.asprintf "%a" pp fact
