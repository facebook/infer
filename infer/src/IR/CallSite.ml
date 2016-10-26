(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module F = Format

type t =
  {
    pname : Procname.t;
    loc : Location.t;
  }

let compare t1 t2 =
  let n = Procname.compare t1.pname t2.pname in
  if n <> 0
  then n
  else Location.compare t1.loc t2.loc

let equal t1 t2 =
  compare t1 t2 = 0

let pname t =
  t.pname

let loc t =
  t.loc

let make pname loc =
  { pname; loc; }

let dummy =
  make Procname.empty_block Location.dummy

let pp fmt t =
  F.fprintf fmt "%a at %a" Procname.pp t.pname Location.pp t.loc

module Set = PrettyPrintable.MakePPSet(struct
    type nonrec t = t
    let compare = compare
    let pp_element = pp
  end)
