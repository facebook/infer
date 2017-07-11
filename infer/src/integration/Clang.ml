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

type compiler = Clang | Make [@@deriving compare]

let rec pp_list pp fmt = function
  | []
   -> ()
  | [x]
   -> pp fmt x
  | x :: tl
   -> F.fprintf fmt "%a@\n%a" pp x (pp_list pp) tl

let pp_env fmt env = pp_list (fun fmt s -> F.fprintf fmt "%s" s) fmt env

let pp_extended_env fmt (env: Unix.env) =
  let pp_pair fmt (var, value) = F.fprintf fmt "%s=%s" var value in
  let pp_pair_list = pp_list pp_pair in
  match env with
  | `Replace values
   -> pp_pair_list fmt values
  | `Extend values
   -> let is_extended s =
        match String.lsplit2 s ~on:'=' with
        | Some (var, _)
         -> List.exists ~f:(fun (var', _) -> String.equal var var') values
        | None
         -> false
      in
      let env_not_extended =
        Unix.environment () |> Array.to_list |> List.filter ~f:(Fn.non is_extended)
      in
      F.fprintf fmt "%a@\n%a" pp_env env_not_extended pp_pair_list values
  | `Replace_raw values
   -> pp_env fmt values

let capture compiler ~prog ~args =
  match compiler with
  | Clang
   -> ClangWrapper.exe ~prog ~args
  | Make
   -> let path_var = "PATH" in
      let new_path = Config.wrappers_dir ^ ":" ^ Sys.getenv_exn path_var in
      let extended_env = `Extend [(path_var, new_path)] in
      L.environment_info "Running command %s with env:@\n%a@\n@." prog pp_extended_env extended_env ;
      Unix.fork_exec ~prog ~argv:(prog :: args) ~env:extended_env () |> Unix.waitpid
      |> function
        | Ok ()
         -> ()
        | Error _ as status
         -> failwithf "*** capture command failed:@\n*** %s@\n*** %s@."
              (String.concat ~sep:" " (prog :: args))
              (Unix.Exit_or_signal.to_string_hum status)
