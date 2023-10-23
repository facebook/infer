(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

module Location : sig
  type t

  val pp : Format.formatter -> t -> unit
end

module Error : sig
  type kind

  type t = Logging.error * Location.t * kind

  val pp_kind : Format.formatter -> kind -> unit
end

module Module : sig
  type t

  val pp : Format.formatter -> t -> unit [@@warning "-unused-value-declaration"]
end

val mk : debug:bool -> FFI.Code.t -> (Module.t, Error.t) result
