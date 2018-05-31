(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
(* Module that suggest adding nullability annotations *)

open! IStd

val checker : Callbacks.proc_callback_t
