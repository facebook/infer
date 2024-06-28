(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(* for now this is just a call site, but in the future we may add input access path, output kind,
   etc. depending on what we need *)
type t = {site: CallSite.t} [@@deriving compare]

let site t = t.site

let pp fmt s = CallSite.pp fmt s.site

module Set = PrettyPrintable.MakePPSet (struct
  type nonrec t = t [@@deriving compare]

  let pp = pp
end)
