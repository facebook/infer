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
end

let normalizer_reset_funs : (unit -> unit) list ref = ref []

module Make (T : NormalizedT) = struct
  type t = T.t

  module H = Caml.Hashtbl.Make (T)

  let table : t H.t = H.create 11

  let () = normalizer_reset_funs := (fun () -> H.reset table) :: !normalizer_reset_funs

  let normalize t =
    match H.find_opt table t with
    | Some t' ->
        t'
    | None ->
        let normalized = T.normalize t in
        H.add table normalized normalized ;
        normalized
end

module StringNormalizer = Make (struct
  include String

  let normalize = Fn.id
end)

module rec StringListNormalizer : (S with type t = string list) = Make (struct
  type t = string list [@@deriving equal]

  let hash = Hashtbl.hash

  let normalize string_list =
    match string_list with
    | [] ->
        []
    | x :: xs ->
        let xs' = StringListNormalizer.normalize xs in
        let x' = StringNormalizer.normalize x in
        if phys_equal x x' && phys_equal xs xs' then string_list else x' :: xs'
end)

let reset_all_normalizers () = List.iter !normalizer_reset_funs ~f:(fun f -> f ())
