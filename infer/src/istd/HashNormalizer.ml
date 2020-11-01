(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! Core

module type NormalizedT = sig
  include Caml.Hashtbl.HashedType

  val normalize : t -> t
end

module type S = sig
  type t

  val normalize : t -> t

  val reset : unit -> unit
end

module Make (T : NormalizedT) = struct
  type t = T.t

  module H = Caml.Hashtbl.Make (T)

  let table : t H.t = H.create 11

  let normalize t =
    match H.find_opt table t with
    | Some t' ->
        t'
    | None ->
        let normalized = T.normalize t in
        H.add table normalized normalized ;
        normalized


  let reset () = H.reset table
end

module StringNormalizer = Make (struct
  include String

  let normalize = Fn.id
end)
