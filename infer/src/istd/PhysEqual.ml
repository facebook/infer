(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let rec compare_fields ox oy i =
  i < 0 || (phys_equal (Obj.field ox i) (Obj.field oy i) && compare_fields ox oy (i - 1))


let shallow_equal x y =
  phys_equal x y
  ||
  let ox = Obj.repr x in
  let oy = Obj.repr y in
  let tx = Obj.tag ox in
  let ty = Obj.tag oy in
  Int.equal tx ty && tx < Obj.no_scan_tag
  &&
  let sx = Obj.size ox in
  let sy = Obj.size oy in
  Int.equal sx sy && compare_fields ox oy (sx - 1)


let optim1 ~res x = if shallow_equal res x then x else res

let optim2 ~res x1 x2 =
  if shallow_equal res x1 then x1 else if shallow_equal res x2 then x2 else res
