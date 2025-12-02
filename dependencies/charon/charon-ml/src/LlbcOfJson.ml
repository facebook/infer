(** Functions to load LLBC ASTs from json.

    See the comments for {!Charon.GAstOfJson} *)

open OfJsonBasic
open Types
open LlbcAst
include GAstOfJson
include Generated_LlbcOfJson

let expr_body_of_json (ctx : of_json_ctx) (js : json) :
    (expr_body, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Structured", body) ] ->
        let* body = gexpr_body_of_json block_of_json ctx body in
        Ok body
    | _ -> Error "")

let crate_of_json (js : json) : (crate, string) result =
  gcrate_of_json expr_body_of_json js
