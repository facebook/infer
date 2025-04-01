(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** dummy version of [LlvmSledgeFrontend] *)

module LlvmSledgeFrontend = struct
  exception Invalid_llvm of string

  let translate ?dump_bitcode:_ _input = assert false
end
