(** Functions to load ULLBC ASTs from json.

    See the comments for {!GAstOfJson} *)

open OfJsonBasic
open Types
open Expressions
open UllbcAst
include GAstOfJson
include Generated_UllbcOfJson

let expr_body_of_json (ctx : of_json_ctx) (js : json) :
    (expr_body option, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Unstructured", body) ] ->
        let* body = gexpr_body_of_json (list_of_json block_of_json) ctx body in
        Ok (Some body)
    | _ -> Ok None)

let crate_of_json (js : json) : (crate, string) result =
  gcrate_of_json expr_body_of_json js
