(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module F = Format

(* for now this is just a call site, but in the future we may add input access path, output kind,
   etc. depending on what we need *)
type t =
  {
    site : CallSite.t;
  }
[@@deriving compare]

let make site =
  { site }

let site t =
  t.site

let pp fmt s =
  F.fprintf fmt "%a" CallSite.pp s.site

module Set = PrettyPrintable.MakePPSet(struct
    type nonrec t = t
    let compare = compare
    let pp = pp
  end)
