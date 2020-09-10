(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
(* Type pointers *)
module TypePtr = struct
  (* extensible type to allow users to specify more variants *)
  type t = ..

  type t += Ptr of int

  let wrap x = Ptr x

  let unwrap = function Ptr x -> x | _ -> raise (invalid_arg "Unknown variant type")
end
