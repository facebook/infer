(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

module Error : sig
  type kind

  type t = Logging.error * kind

  val pp_kind : Format.formatter -> kind -> unit [@@warning "-unused-value-declaration"]
end

module Module : sig
  type t

  val pp : Format.formatter -> t -> unit [@@warning "-unused-value-declaration"]
end

val mk : debug:bool -> FFI.Code.t -> (Module.t, Error.t) result
