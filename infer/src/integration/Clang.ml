(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging

type compiler = Clang | Make [@@deriving compare]

let pp_extended_env fmt (env : Unix.env) =
  let pp_pair fmt (var, value) = F.fprintf fmt "%s=%s" var value in
  match env with
  | `Replace values ->
      F.fprintf fmt "@[<v>%a@]" (Pp.seq ~print_env:Pp.text_break pp_pair) values
  | `Extend values ->
      let is_extended s =
        match String.lsplit2 s ~on:'=' with
        | Some (var, _) ->
            List.exists ~f:(fun (var', _) -> String.equal var var') values
        | None ->
            false
      in
      let env_not_extended =
        Unix.environment () |> Array.to_list |> List.filter ~f:(Fn.non is_extended)
      in
      F.fprintf fmt "@[<v>%a@ %a@]"
        (Pp.seq ~print_env:Pp.text_break F.pp_print_string)
        env_not_extended
        (Pp.seq ~print_env:Pp.text_break pp_pair)
        values
  | `Replace_raw values ->
      F.fprintf fmt "@[<v>%a@]" (Pp.seq ~print_env:Pp.text_break F.pp_print_string) values


let capture compiler ~prog ~args =
  match compiler with
  | Clang ->
      ClangWrapper.exe ~prog ~args
  | Make -> (
      let path_var = "PATH" in
      let old_path = Option.value ~default:"" (Sys.getenv path_var) in
      let new_path = Config.wrappers_dir ^ ":" ^ old_path in
      let extended_env = `Extend [(path_var, new_path); ("INFER_OLD_PATH", old_path)] in
      L.environment_info "Running command %s with env:@\n%a@\n@." prog pp_extended_env extended_env ;
      Unix.fork_exec ~prog ~argv:(prog :: args) ~env:extended_env ()
      |> Unix.waitpid
      |> function
      | Ok () ->
          ()
      | Error _ as status ->
          L.(die ExternalError)
            "*** capture command failed:@\n*** %s@\n*** %s@."
            (String.concat ~sep:" " (prog :: args))
            (Unix.Exit_or_signal.to_string_hum status) )
