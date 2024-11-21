(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module T = struct
  type t = string [@@deriving compare, hash, equal]
end

module Map = Stdlib.Map.Make (T)
module Set = Stdlib.Set.Make (T)
module Hash = Stdlib.Hashtbl.Make (T)
