(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Uninterpreted Predicate Symbols *)

type t = string [@@deriving compare, equal, hash, sexp]

let pp ppf p =
  let pf fmt = Format.fprintf ppf fmt in
  pf "%s" p

let uninterp p = p
