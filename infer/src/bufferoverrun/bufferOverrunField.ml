(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

(**
  If fn is empty, prints [pp_lhs_alone lhs]
  Otherwise prints [pp_lhs lhs ^ sep ^ fn]
*)
let pp ~pp_lhs ~pp_lhs_alone ~sep f lhs fn =
  let fieldname = Typ.Fieldname.to_flat_string fn in
  if String.is_empty fieldname then pp_lhs_alone f lhs
  else F.fprintf f "%a%s%s" pp_lhs lhs sep fieldname
