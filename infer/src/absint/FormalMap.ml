(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t = int AccessPath.BaseMap.t

let make attrs =
  let pname = attrs.ProcAttributes.proc_name in
  let formals_with_nums =
    List.mapi
      ~f:(fun index (name, typ, _) ->
        let pvar = Pvar.mk name pname in
        (AccessPath.base_of_pvar pvar typ, index) )
      attrs.ProcAttributes.formals
  in
  List.fold
    ~f:(fun formal_map (base, index) -> AccessPath.BaseMap.add base index formal_map)
    ~init:AccessPath.BaseMap.empty formals_with_nums


let is_formal = AccessPath.BaseMap.mem

let get_formal_index base t = AccessPath.BaseMap.find_opt base t

let get_formal_base index t =
  List.find ~f:(fun (_, i) -> Int.equal i index) (AccessPath.BaseMap.bindings t)
  |> Option.map ~f:fst


let pp = AccessPath.BaseMap.pp ~pp_value:Int.pp

let cardinal = AccessPath.BaseMap.cardinal

let iter = AccessPath.BaseMap.iter
