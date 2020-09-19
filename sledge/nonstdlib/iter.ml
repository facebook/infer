(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include IterLabels

module Import = struct
  type 'a iter = 'a t
end

let pop seq =
  match head seq with Some x -> Some (x, drop 1 seq) | None -> None
