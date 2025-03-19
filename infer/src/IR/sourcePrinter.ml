(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Printers for the analysis results, at source level (one invariant per line) *)

let infos_per_sourcefiles_and_lines = DLS.new_key (fun () -> SourceFile.Hash.create 1)

let add_info ~sourcefile ~line ~info =
  let tbl = DLS.get infos_per_sourcefiles_and_lines in
  let per_lines =
    SourceFile.Hash.find_opt tbl sourcefile
    |> Option.value_or_thunk ~default:(fun () ->
           let per_lines_tbl = IInt.Hash.create 1 in
           SourceFile.Hash.replace tbl sourcefile per_lines_tbl ;
           per_lines_tbl )
  in
  IInt.Hash.replace per_lines line info


let write_all () =
  let linereader = LineReader.create () in
  SourceFile.Hash.iter
    (fun sourcefile per_lines_tbl ->
      let output_dir = ResultsDirEntryName.get_path SourceDebug ~results_dir:Config.results_dir in
      IUnix.mkdir_p output_dir ;
      let filename =
        SourceFile.to_rel_path sourcefile |> String.substr_replace_all ~pattern:"/" ~with_:"_"
      in
      Filename.concat output_dir filename
      |> Utils.with_file_out ~f:(fun channel ->
             let print_one_line line_number raw_line =
               Printf.fprintf channel "%s\n" raw_line ;
               IInt.Hash.find_opt per_lines_tbl line_number
               |> Option.iter ~f:(fun info -> Printf.fprintf channel "%s\n" info)
             in
             LineReader.iteri linereader sourcefile ~f:print_one_line ) )
    (DLS.get infos_per_sourcefiles_and_lines)
