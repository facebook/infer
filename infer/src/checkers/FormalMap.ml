(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module F = Format
module L = Logging

type t = int AccessPath.BaseMap.t

let make pdesc =
  let pname = Procdesc.get_proc_name pdesc in
  let attrs = Procdesc.get_attributes pdesc in
  let formals_with_nums =
    IList.mapi
      (fun index (name, typ) ->
         let pvar = Pvar.mk name pname in
         AccessPath.base_of_pvar pvar typ, index)
      attrs.ProcAttributes.formals in
  List.fold
    ~f:(fun formal_map (base, index) -> AccessPath.BaseMap.add base index formal_map)
    ~init:AccessPath.BaseMap.empty
    formals_with_nums

let empty = AccessPath.BaseMap.empty

let is_formal = AccessPath.BaseMap.mem

let get_formal_index base t =
  try Some (AccessPath.BaseMap.find base t)
  with Not_found -> None

let get_formal_base index t =
  List.find ~f:(fun (_, i) -> Int.equal i index) (AccessPath.BaseMap.bindings t)
  |> Option.map ~f:fst
