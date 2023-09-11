(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

(* file used only to apply the deriver through [inline] and commit the generated functions
   so as to track changes *)

type record = {s: string; i: int} [@@deriving equal, hash] [@@deriving_inline normalize]

let _ = fun (_ : record) -> ()

let normalize_record t =
  let {s; i} = t in
  let s' = HashNormalizer.StringNormalizer.normalize s in
  if phys_equal s s' then t else {s= s'; i}


let _ = normalize_record

[@@@end]

module RecordHashNormalizer = HashNormalizer.Make (struct
  type t = record [@@deriving equal, hash] [@@deriving_inline normalize]

  let _ = fun (_ : t) -> ()

  let normalize = normalize_record

  let _ = normalize

  [@@@end]
end)

type tuple = string * int * string [@@deriving equal, hash] [@@deriving_inline normalize]

let _ = fun (_ : tuple) -> ()

let normalize_tuple t =
  let x0, x1, x2 = t in
  let x2' = HashNormalizer.StringNormalizer.normalize x2 in
  let x0' = HashNormalizer.StringNormalizer.normalize x0 in
  if phys_equal x2 x2' && phys_equal x0 x0' then t else (x0', x1, x2')


let _ = normalize_tuple

[@@@end]

module TupleHashNormalizer = HashNormalizer.Make (struct
  type t = tuple [@@deriving equal, hash] [@@deriving_inline normalize]

  let _ = fun (_ : t) -> ()

  let normalize = normalize_tuple

  let _ = normalize

  [@@@end]
end)

type variant =
  | NoArgs
  | String of string
  | Int of int
  | Tuple of int * string
  | Record of {i: int; s: string}
  | NonInline of record
[@@deriving equal, hash] [@@deriving_inline normalize]

let _ = fun (_ : variant) -> ()

let normalize_variant t =
  match t with
  | NoArgs ->
      t
  | String x0 ->
      let x0' = HashNormalizer.StringNormalizer.normalize x0 in
      if phys_equal x0 x0' then t else String x0'
  | Int _ ->
      t
  | Tuple (x0, x1) ->
      let x1' = HashNormalizer.StringNormalizer.normalize x1 in
      if phys_equal x1 x1' then t else Tuple (x0, x1')
  | Record {i; s} ->
      let s' = HashNormalizer.StringNormalizer.normalize s in
      if phys_equal s s' then t else Record {i; s= s'}
  | NonInline x0 ->
      let x0' = normalize_record x0 in
      if phys_equal x0 x0' then t else NonInline x0'


let _ = normalize_variant

[@@@end]

module VariantHashNormalizer = HashNormalizer.Make (struct
  type t = variant [@@deriving equal, hash] [@@deriving_inline normalize]

  let _ = fun (_ : t) -> ()

  let normalize = normalize_variant

  let _ = normalize

  [@@@end]
end)
