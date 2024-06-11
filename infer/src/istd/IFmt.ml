(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let colon_sp ppf _ =
  Format.pp_print_string ppf ":" ;
  Fmt.sp ppf ()


let if' bool pp = if bool then pp else Fmt.nop

module Labelled = struct
  let iter ?sep iter pp_elt = Fmt.iter ?sep (fun f x -> iter x ~f) pp_elt

  let iter_bindings ?sep iteri pp_binding =
    Fmt.iter_bindings ?sep (fun f x -> iteri x ~f:(fun ~key ~data -> f key data)) pp_binding
end
