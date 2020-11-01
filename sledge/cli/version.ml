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

module Build_info = Build_info.V1

let version_to_string v =
  Option.map_or ~f:Build_info.Version.to_string v ~default:"dev"

let version =
  Format.sprintf "%s%s"
    (version_to_string (Build_info.version ()))
    (if debug then "-dbg" else "")

let build_info =
  let libs =
    List.map (Build_info.Statically_linked_libraries.to_list ())
      ~f:(fun lib ->
        ( Build_info.Statically_linked_library.name lib
        , version_to_string
            (Build_info.Statically_linked_library.version lib) ) )
    |> List.sort ~cmp:[%compare: string * string]
  in
  let max_length =
    List.fold_left libs 0 ~f:(fun n (name, _) ->
        max n (String.length name) )
  in
  String.concat ~sep:"\n"
    ( Printf.sprintf "%-*s %s" (max_length + 2) "ocaml:" Sys.ocaml_version
      :: "statically linked libraries:"
      :: List.map libs ~f:(fun (name, v) ->
             Printf.sprintf "- %-*s %s" max_length name v )
    @ [Printf.sprintf "%-*s %b" (max_length + 2) "debug:" debug; "version:"]
    )
