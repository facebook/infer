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
module HashConsId = IdGen ()

(** The default logger *)
let log = Logging.llbc_of_json_logger


type id_to_file_map = file FileId.Map.t
type of_json_ctx = {
    id_to_file_map : id_to_file_map;
    ty_hashcons_map: (ty HashConsId.Map.t) ref;
    tref_hashcons_map: (trait_ref HashConsId.Map.t) ref;
}

let empty_of_json_ctx : of_json_ctx =
  {
    id_to_file_map = FileId.Map.empty;
    ty_hashcons_map = ref HashConsId.Map.empty;
    tref_hashcons_map = ref HashConsId.Map.empty;
  }

let hash_consed_val_of_json (map : 'a HashConsId.Map.t ref)
    (of_json : of_json_ctx -> json -> ('a, string) result) (ctx : of_json_ctx)
    (js : json) : ('a, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Untagged", json) ] -> of_json ctx json
    | `Assoc [ ("HashConsedValue", `List [ `Int id; json ]) ] ->
        let* v = of_json ctx json in
        let id = HashConsId.of_int id in
        map := HashConsId.Map.add id v !map;
        Ok v
    | `Assoc [ ("Deduplicated", `Int id) ] -> begin
        let id = HashConsId.of_int id in
        match HashConsId.Map.find_opt id !map with
        | Some v -> Ok v
        | None ->
            Error
              "Hash-consing key not found; there is a serialization mismatch \
               between Rust and OCaml"
      end
    | _ -> Error "")

let path_buf_of_json = string_of_json

let big_int_of_json _ (js : json) : (big_int, string) result =
    combine_error_msgs js __FUNCTION__
      (match js with
      | `Int i -> Ok (Z.of_int i)
      | `String is -> Ok (Z.of_string is)
      | _ -> Error "")

(* __REPLACE0__ *)
