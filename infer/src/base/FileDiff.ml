(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module UnixDiff = struct
  type t = Unchanged | New | Old [@@deriving compare]

  let equal = [%compare.equal: t]

  let directive_of_char c =
    match c with
    | 'U' ->
        Unchanged
    | 'N' ->
        New
    | 'O' ->
        Old
    | _ ->
        Logging.die Logging.UserError "Unexpected char in input sequence. Failed parsing"


  let process_raw_directives in_str =
    if String.is_empty in_str then [] else String.to_list in_str |> List.map ~f:directive_of_char


  let pp fmt d =
    match d with
    | Unchanged ->
        Format.pp_print_char fmt 'U'
    | New ->
        Format.pp_print_char fmt 'N'
    | Old ->
        Format.pp_print_char fmt 'O'


  module VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY = struct
    type nonrec t = t = Unchanged | New | Old

    let equal = equal

    let process_raw_directives = process_raw_directives

    let pp = pp
  end
end

let parse_directives directives =
  let rec aux ~d ~lines ~line_ptr ~pred_is_old =
    (* O does not move the line-pointer *)
    (* N moves the line-pointer and marks the line as affected *)
    (* U moves the line-pointer, and marks the line as affected ONLY if it is preceded by O *)
    match d with
    | [] ->
        List.rev lines
    | UnixDiff.Old :: ds ->
        aux ~d:ds ~lines ~line_ptr ~pred_is_old:true
    | UnixDiff.New :: ds ->
        aux ~d:ds ~lines:(line_ptr :: lines) ~line_ptr:(line_ptr + 1) ~pred_is_old:false
    | UnixDiff.Unchanged :: ds ->
        let lines' = if pred_is_old then line_ptr :: lines else lines in
        aux ~d:ds ~lines:lines' ~line_ptr:(line_ptr + 1) ~pred_is_old:false
  in
  if List.is_empty directives then (* handle the case where both files are empty *)
    []
  else if
    (* handle the case where the new-file is empty *)
    List.for_all ~f:(UnixDiff.equal UnixDiff.Old) directives
  then [1]
  else
    let pred_is_old, directives' =
      match directives with UnixDiff.Old :: ds -> (true, ds) | _ -> (false, directives)
    in
    aux ~d:directives' ~lines:[] ~line_ptr:1 ~pred_is_old


module VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY = struct
  let parse_directives = parse_directives
end

(** Given a difference between two files, return the relevant lines in the new file; a line is
    relevant when a change took place in it, or nearby. To generate a valid input for this
    parser, use unix-diff command with the following formatter arguments:
    diff --unchanged-line-format="U" --old-line-format="O" --new-line-format="N" File1 File2 *)
let parse_unix_diff str = UnixDiff.process_raw_directives str |> parse_directives
