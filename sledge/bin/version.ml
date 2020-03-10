(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Version information populated by build system *)

let debug =
  let d = ref false in
  assert (
    d := true ;
    true ) ;
  !d

module B = Build_info.V1

let version_to_string v =
  Option.value_map ~f:B.Version.to_string v ~default:"dev"

let version =
  version_to_string (B.version ()) ^ if debug then "-dbg" else ""

let build_info =
  let libs =
    List.map (B.Statically_linked_libraries.to_list ()) ~f:(fun lib ->
        ( B.Statically_linked_library.name lib
        , version_to_string (B.Statically_linked_library.version lib) ) )
    |> List.sort ~compare:[%compare: string * string]
  in
  let max_length =
    List.fold_left libs ~init:0 ~f:(fun n (name, _) ->
        max n (String.length name) )
  in
  String.concat ~sep:"\n"
    ( "statically linked libraries:"
      :: List.map libs ~f:(fun (name, v) ->
             Printf.sprintf "- %-*s %s" max_length name v )
    @ ["version:"] )
