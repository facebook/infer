(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = BufferOverrunDomain.Mem.no_oenv_t

val pp : Format.formatter -> t -> unit

type get_summary = Procname.t -> t option
