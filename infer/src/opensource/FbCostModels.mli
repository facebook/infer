(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Call : sig
  val dispatch : 'context -> 'procname_t -> 'arg_payload_FuncArg_t_list -> 'f option
end
