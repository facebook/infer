(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module Val : sig
  type t

  val is_integer_type : t -> bool
end

type get_cast_type = Ident.t -> Val.t

val compute_get_cast_type : Callbacks.proc_callback_args -> Ident.t -> Val.t
