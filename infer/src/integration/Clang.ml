(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd

type compiler =
  | Clang
  | Make [@@deriving compare]

let extended_env_to_string (env : Unix.env) =
  match env with
  | `Replace values
  | `Extend values ->
      let concat_elt (el1, el2) = String.concat ~sep:"=" [el1; el2] in
      String.concat ~sep:"\n" (List.map ~f:concat_elt values)
  | `Replace_raw values ->
      String.concat ~sep:"\n" values

let env_to_string ?exclude_var env =
  let env_element_to_string elt acc =
    match exclude_var with
    | Some var when String.is_prefix ~prefix:var elt -> ""
    | _ ->
        String.concat [elt; acc] ~sep:"\n" in
  Array.fold_right ~init:"" ~f:env_element_to_string env


(* TODO(t16929015): make the clang/clang++ integration just a function call. *)
let capture _ ~prog ~args =
  let path_var = "PATH" in
  let new_path = Config.wrappers_dir ^ ":" ^ (Sys.getenv_exn path_var) in
  let extended_env = `Extend [path_var, new_path] in
  Logging.out "Running command %s with env:\n%s %s\n@."
    prog
    (env_to_string ~exclude_var:path_var (Unix.environment ()))
    (extended_env_to_string extended_env);
  Unix.fork_exec ~prog:prog ~args:(prog::args) ~env:extended_env ()
  |> Unix.waitpid
  |> function
  | Ok () -> ()
  | Error _ as status ->
      failwithf "*** ERROR: capture command failed:@*** %s@*** %s@"
        (String.concat ~sep:" " (prog::args))
        (Unix.Exit_or_signal.to_string_hum status)
