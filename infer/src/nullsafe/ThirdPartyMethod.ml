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


let pp_unique_repr fmt signature = Sexp.pp fmt (sexp_of_unique_repr signature)

let java_type_to_string java_type =
  let package = Typ.Name.Java.Split.package java_type in
  let type_name = Typ.Name.Java.Split.type_name java_type in
  match package with
  | None ->
      (* Primitive type *)
      type_name
  | Some package ->
      package ^ "." ^ type_name


let unique_repr_of_java_proc_name java_proc_name =
  let class_name = Typ.Procname.Java.get_class_name java_proc_name in
  let method_name =
    if Typ.Procname.Java.is_constructor java_proc_name then Constructor
    else Method (Typ.Procname.Java.get_method java_proc_name)
  in
  let param_types =
    Typ.Procname.Java.get_parameters java_proc_name |> List.map ~f:java_type_to_string
  in
  {class_name; method_name; param_types}


let pp_nullability fmt nullability = Sexp.pp fmt (sexp_of_nullability nullability)

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
      >>= fun (parsed_params, ret_nullability) ->
      let param_types, param_nullability = List.unzip parsed_params in
      Ok ({class_name; method_name; param_types}, {ret_nullability; param_nullability})
  | _ ->
      Error BadStructure


let pp_parse_result fmt (unique_repr, nullability) =
  F.fprintf fmt "(%a; %a)" pp_unique_repr unique_repr pp_nullability nullability
