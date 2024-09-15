(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! Core
module F = Format
module Map = Caml.Map.Make (Int)

module Set = struct
  include Caml.Set.Make (Int)

  let pp fmt set = Fmt.braces (Fmt.iter iter ~sep:Fmt.comma F.pp_print_int) fmt set
end

module Hash = Caml.Hashtbl.Make (Int)
