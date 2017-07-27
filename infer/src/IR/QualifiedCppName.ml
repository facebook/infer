(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(* internally it uses reversed list to store qualified name, for example: ["get", "shared_ptr<int>", "std"]*)
type t = string list [@@deriving compare]

let equal = [%compare.equal : t]

let empty = []

let append_qualifier quals ~qual = List.cons qual quals

let extract_last = function last :: rest -> Some (last, rest) | [] -> None

let strip_template_args quals =
  let no_template_name s = List.hd_exn (String.split ~on:'<' s) in
  List.map ~f:no_template_name quals

let append_template_args_to_last quals ~args =
  match quals with
  | [last; _] when String.contains last '<'
   -> failwith "expected qualified name without template args"
  | last :: rest
   -> (last ^ args) :: rest
  | []
   -> failwith "expected non-empty qualified name"

let to_list = List.rev

let to_rev_list = ident

let of_list = List.rev

let of_rev_list = ident

let cpp_separator = "::"

(* define [cpp_separator_regex] here to compute it once *)
let cpp_separator_regex = Str.regexp_string cpp_separator

(* This is simplistic and will give the wrong answer in some cases, eg
   "foo<bar::baz<goo>>::someMethod" will get parsed as ["foo<bar", "baz<goo>>",
   "someMethod"]. Avoid using it if possible *)
let of_qual_string str = Str.split cpp_separator_regex str |> List.rev

let to_separated_string quals ~sep = List.rev quals |> String.concat ~sep

let to_qual_string = to_separated_string ~sep:cpp_separator

let pp fmt quals = Format.fprintf fmt "%s" (to_qual_string quals)

module Match = struct
  type quals_matcher = Str.regexp

  let matching_separator = "#"

  let regexp_string_of_qualifiers quals =
    Str.quote (to_separated_string ~sep:matching_separator quals) ^ "$"

  let qualifiers_list_matcher quals_list =
    ( if List.is_empty quals_list then "a^"
    else
      (* regexp that does not match anything *)
      List.map ~f:regexp_string_of_qualifiers quals_list |> String.concat ~sep:"\\|" )
    |> Str.regexp

  let qualifiers_of_fuzzy_qual_name qual_name =
    (* Fail if we detect templates in the fuzzy name. Template instantiations are not taken into
       account when fuzzy matching, and templates may produce wrong results when parsing qualified
       names. *)
    let colon_splits = String.split qual_name ~on:':' in
    List.iter colon_splits ~f:(fun s ->
        (* Filter out the '<' in operator< and operator<= *)
        if not (String.is_prefix s ~prefix:"operator<") && String.contains s '<' then
          failwithf "Unexpected template in fuzzy qualified name %s." qual_name ) ;
    of_qual_string qual_name

  let of_fuzzy_qual_names fuzzy_qual_names =
    List.map fuzzy_qual_names ~f:qualifiers_of_fuzzy_qual_name |> qualifiers_list_matcher

  let match_qualifiers matcher quals =
    (* qual_name may have qualifiers with template parameters - drop them to whitelist all
       instantiations *)
    let normalized_qualifiers = strip_template_args quals in
    Str.string_match matcher (to_separated_string ~sep:matching_separator normalized_qualifiers) 0
end
