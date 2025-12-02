(** WARNING: this file is partially auto-generated. Do not edit `GAstOfJson.ml`
    by hand. Edit `GAstOfJson.template.ml` instead, or improve the code
    generation tool so avoid the need for hand-writing things.

    `GAstOfJson.template.ml` contains the manual definitions and some `(*
    __REPLACEn__ *)` comments. These comments are replaced by auto-generated
    definitions by running `make generate-ml` in the crate root. The
    code-generation code is in `charon/src/bin/generate-ml`.
 *)

open Yojson.Basic
open OfJsonBasic
open Identifiers
open Meta
open Values
open Types
open Scalars
open Expressions
open GAst
module FileId = IdGen ()

(** The default logger *)
let log = Logging.llbc_of_json_logger

type id_to_file_map = file FileId.Map.t
type of_json_ctx = id_to_file_map

let path_buf_of_json = string_of_json

let big_int_of_json _ (js : json) : (big_int, string) result =
    combine_error_msgs js __FUNCTION__
      (match js with
      | `Int i -> Ok (Z.of_int i)
      | `String is -> Ok (Z.of_string is)
      | _ -> Error "")

(* __REPLACE0__ *)
