(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
open Result.Monad_infix

type t =
  { class_name: fully_qualified_type
  ; method_name: method_name
  ; ret_nullability: type_nullability
  ; params: (fully_qualified_type * type_nullability) list }

and fully_qualified_type = string [@@deriving sexp]

and method_name = Constructor | Method of string

and type_nullability = Nullable | Nonnull [@@deriving sexp]

type parsing_error =
  | BadStructure
  | BadClassName
  | BadMethodName
  | BadReturnNullability
  | BadParamList
  | BadParam

let string_of_parsing_error = function
  | BadStructure ->
      "Accepted format is <class>#<method>(<params>)[<return nullability>]"
  | BadMethodName ->
      "Method name should be a valid identifier"
  | BadClassName ->
      "Class name should be fully qualified, including package name"
  | BadReturnNullability ->
      "Unexpected string after the closing parenthesis, expected @Nullable"
  | BadParamList ->
      "Params should be separated by a comma, followed by a single space"
  | BadParam ->
      "Each param should have form of [@Nullable] <fully qualified type name>"


let nullable_annotation = "@Nullable"

let identifier_regexp = lazy (Str.regexp "[_a-zA-Z][_a-zA-Z0-9]*$")

let class_regexp =
  (* package should be a list of valid identifiers, separared by '.' *)
  let package_name = "[_a-zA-Z][_a-zA-Z0-9\\.]*" in
  (* class name should be a list of identifiers, separated by $ (a symbol for a nested class) *)
  let class_name = "[_a-zA-Z][_a-zA-Z0-9\\$]*" in
  lazy (Str.regexp (package_name ^ "\\." ^ class_name ^ "$"))


let type_regexp =
  (* identifiers, possiblibly separated by `.` (package delimiter) or `$` (a symbol for a nested class) *)
  lazy (Str.regexp "[_a-zA-Z][_a-zA-Z0-9\\$\\.]*")


let parse_class str =
  if Str.string_match (Lazy.force class_regexp) str 0 then Ok str else Error BadClassName


let parse_param_type str =
  if Str.string_match (Lazy.force type_regexp) str 0 then Ok str else Error BadParam


let parse_param str =
  match String.split str ~on:' ' with
  | [nullability_str; typ] ->
      Result.ok_if_true (String.equal nullable_annotation nullability_str) ~error:BadParam
      >>= fun _ -> parse_param_type typ >>= fun parsed_typ -> Ok (parsed_typ, Nullable)
  | [typ] ->
      parse_param_type typ >>= fun parsed_typ -> Ok (parsed_typ, Nonnull)
  | [] ->
      Error BadParamList
  | _ ->
      Error BadParam


(* Given a list of results and a binding function, returns Ok (results) if all results
   are Ok or the first error if any of results has Error *)
let bind_list list_of_results ~f =
  List.fold list_of_results ~init:(Ok []) ~f:(fun acc element ->
      acc
      >>= fun accumulated_success_results ->
      f element >>= fun success_result -> Ok (accumulated_success_results @ [success_result]) )


let strip_first_space str =
  String.chop_prefix str ~prefix:" " |> Result.of_option ~error:BadParamList


let split_params str =
  (* Empty case is the special one: lack of params mean an empty list,
     not a list of a single empty string *)
  if String.is_empty str then Ok []
  else
    String.split str ~on:','
    |> List.mapi ~f:(fun param_index param_as_str -> (param_index, param_as_str))
    |> bind_list ~f:(fun (param_index, str) ->
           match param_index with
           | 0 ->
               Ok str
           | _ ->
               (* Params should be separated by ", ", so we expect a space after each comma *)
               strip_first_space str )


let parse_params str = split_params str >>= fun params -> bind_list params ~f:parse_param

let parse_method_name str =
  match str with
  | "<init>" ->
      Ok Constructor
  | _ as method_name ->
      if Str.string_match (Lazy.force identifier_regexp) method_name 0 then Ok (Method method_name)
      else Error BadMethodName


let match_after_close_brace (split_result : Str.split_result list) =
  (* After close brace there can be either nothing or return type nullability information *)
  match split_result with
  | [] ->
      Ok Nonnull
  | [Text nullability] ->
      Result.ok_if_true
        (String.equal (" " ^ nullable_annotation) nullability)
        ~error:BadReturnNullability
      >>= fun _ -> Ok Nullable
  | _ ->
      Error BadReturnNullability


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
  match Str.full_split (Lazy.force hashsign_and_parentheses) (String.rstrip str) with
  | Text class_name_str :: Delim "#" :: Text method_name_str :: Delim "(" :: rest ->
      parse_class class_name_str
      >>= fun class_name ->
      parse_method_name method_name_str
      >>= fun method_name ->
      match_after_open_brace rest
      >>= fun (params, ret_nullability) -> Ok {class_name; method_name; ret_nullability; params}
  | _ ->
      Error BadStructure


let to_canonical_string
    { class_name: fully_qualified_type
    ; method_name: method_name
    ; ret_nullability: type_nullability
    ; params } =
  let method_name_to_string = function Constructor -> "<init>" | Method name -> name in
  let param_to_string (typ, nullability) =
    match nullability with Nullable -> Format.sprintf "@Nullable %s" typ | Nonnull -> typ
  in
  let param_list_to_string params = List.map params ~f:param_to_string |> String.concat ~sep:", " in
  let ret_to_string = function Nullable -> " @Nullable" | Nonnull -> "" in
  Format.sprintf "%s#%s(%s)%s" class_name
    (method_name_to_string method_name)
    (param_list_to_string params) (ret_to_string ret_nullability)


(* Pretty print exactly as the canonical representation, for convenience *)
let pp = Pp.of_string ~f:to_canonical_string
