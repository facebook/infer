(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
open Result.Monad_infix

type fully_qualified_type = string [@@deriving sexp]

type unique_repr =
  { class_name: fully_qualified_type
  ; method_name: method_name
  ; param_types: fully_qualified_type list }
[@@deriving sexp]

and method_name = Constructor | Method of string

type type_nullability = Nullable | Nonnull [@@deriving sexp]

type nullability = {ret_nullability: type_nullability; param_nullability: type_nullability list}
[@@deriving sexp]

type parsing_error = BadStructure | MalformedNullability | LackingParam | MalformedParam

let string_of_parsing_error = function
  | BadStructure ->
      "BadStructure"
  | MalformedNullability ->
      "MalformedNullability"
  | LackingParam ->
      "LackingParam"
  | MalformedParam ->
      "MalformedParam"


let pp_unique_repr fmt signature = Sexp.pp fmt (sexp_of_unique_repr signature)

let pp_nullability fmt nullability = Sexp.pp fmt (sexp_of_nullability nullability)

let parse_nullability str =
  match String.strip str with
  | "@Nullable" ->
      Ok Nullable
  | "" ->
      Ok Nonnull
  | _ ->
      Error MalformedNullability


let whitespace_no_line_break = lazy (Str.regexp "[ \t]+")

let parse_param str =
  let trimmed_param = String.strip str in
  match Str.split (Lazy.force whitespace_no_line_break) trimmed_param with
  | [nullability_str; typ] ->
      parse_nullability nullability_str >>= fun nullability -> Ok (typ, nullability)
  | [typ] ->
      Ok (typ, Nonnull)
  | [] ->
      Error LackingParam
  | _ ->
      Error MalformedParam


(* Given a list of results and a binding function, returns Ok (results) if all results
   are Ok or the first error if any of results has Error *)
let bind_list list_of_results ~f =
  List.fold list_of_results ~init:(Ok []) ~f:(fun acc element ->
      acc
      >>= fun accumulated_success_results ->
      f element >>= fun success_result -> Ok (accumulated_success_results @ [success_result]) )


let split_params str =
  let stripped = String.strip str in
  (* Empty case is the special one: lack of params mean an empty list,
     not a list of a single empty string *)
  if String.is_empty stripped then [] else String.split stripped ~on:','


let parse_params str = split_params str |> bind_list ~f:parse_param

let parse_method_name str =
  match String.strip str with "<init>" -> Constructor | _ as method_name -> Method method_name


let match_after_close_brace (split_result : Str.split_result list) =
  (* After close brace there can be either nothing or return type nullability information *)
  match split_result with
  | [] ->
      parse_nullability ""
  | [Text nullability] ->
      parse_nullability nullability
  | _ ->
      Error BadStructure


let match_after_open_brace (split_result : Str.split_result list) =
  (* We expect to see <params>)<nullability>, so let's look for `)` *)
  ( match split_result with
  | Text params :: Delim ")" :: rest ->
      Ok (params, rest)
  | Delim ")" :: rest ->
      Ok ("", rest)
  | _ ->
      Error BadStructure )
  >>= fun (params, rest) ->
  parse_params params
  >>= fun parsed_params ->
  match_after_close_brace rest >>= fun ret_nullability -> Ok (parsed_params, ret_nullability)


let hashsign_and_parentheses = lazy (Str.regexp "[#()]")

let parse str =
  (* Expected string is <Class>#<method>(<params>)<ret_nullability>,
     let's look what is between #, (, and ) *)
  match Str.full_split (Lazy.force hashsign_and_parentheses) str with
  | Text class_name_str :: Delim "#" :: Text method_name_str :: Delim "(" :: rest ->
      let method_name = parse_method_name method_name_str in
      let class_name = String.strip class_name_str in
      match_after_open_brace rest
      >>= fun (parsed_params, ret_nullability) ->
      let param_types, param_nullability = List.unzip parsed_params in
      Ok ({class_name; method_name; param_types}, {ret_nullability; param_nullability})
  | _ ->
      Error BadStructure


let pp_parse_result fmt (unique_repr, nullability) =
  F.fprintf fmt "(%a; %a)" pp_unique_repr unique_repr pp_nullability nullability
