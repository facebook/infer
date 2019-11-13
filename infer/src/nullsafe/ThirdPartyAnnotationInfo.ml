(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module Hashtbl = Caml.Hashtbl

type storage = (ThirdPartyMethod.unique_repr, ThirdPartyMethod.nullability) Hashtbl.t

let create_storage () = Hashtbl.create 1

type file_parsing_error =
  {line_number: int; unparsable_method: string; parsing_error: ThirdPartyMethod.parsing_error}

let pp_parsing_error fmt {line_number; unparsable_method; parsing_error} =
  Format.fprintf fmt "Line %d: Could not parse method '%s': %s" line_number unparsable_method
    (ThirdPartyMethod.string_of_parsing_error parsing_error)


(* Consequtively evaluates results for all elements in a list,
   returns Ok () if all succeeded or the first error.
   The evaluator function [f] has access to element's index.
 *)
let bind_list_with_index list ~f =
  List.foldi list ~init:(Ok ()) ~f:(fun index acc elem -> Result.bind acc ~f:(fun _ -> f index elem))


let parse_line_and_add_to_storage storage line =
  let open Result in
  ThirdPartyMethod.parse line
  >>= fun (signature, nullability) -> Ok (Hashtbl.add storage signature nullability)


let add_from_signature_file storage ~lines =
  (* each line in a file should represent a method signature *)
  bind_list_with_index lines ~f:(fun index method_as_str ->
      parse_line_and_add_to_storage storage method_as_str
      |> Result.map_error ~f:(fun parsing_error ->
             {line_number= index + 1; unparsable_method= method_as_str; parsing_error} ) )


let find_nullability_info storage unique_repr = Hashtbl.find_opt storage unique_repr
