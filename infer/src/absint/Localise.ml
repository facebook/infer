(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Support for localisation *)

module F = Format
module MF = MarkupFormatter

module Tags = struct
  type t = (string * string) list [@@deriving compare]

  let bucket = "bucket"

  let line = "line"

  (** string describing a C value, e.g. "x.date" *)
  let value = "value"

  let create () = ref []

  let add tags tag value = List.Assoc.add ~equal:String.equal tags tag value

  let update tags tag value = tags := add !tags tag value

  let get tags tag = List.Assoc.find ~equal:String.equal tags tag
end

type error_desc =
  {descriptions: string list; suggestion: string option; tags: Tags.t; dotty: string option}
[@@deriving compare]

(** empty error description *)
let no_desc : error_desc = {descriptions= []; suggestion= None; tags= []; dotty= None}

(** verbatim desc from a string, not to be used for user-visible descs *)
let verbatim_desc ?suggestion s = {no_desc with descriptions= [s]; suggestion}

(** pretty print an error qualifier *)
let pp_error_qualifier fmt err_desc = Pp.seq F.pp_print_string fmt err_desc.descriptions

(** pretty print an error suggestion *)
let pp_error_suggestion fmt err_desc = Option.iter ~f:(F.pp_print_string fmt) err_desc.suggestion

(** pretty print a full error description including suggestion *)
let pp_error_desc fmt err_desc =
  Format.fprintf fmt "%a%a" pp_error_qualifier err_desc pp_error_suggestion err_desc


let error_desc_get_dotty err_desc = err_desc.dotty

(** get the bucket value of an error_desc, if any *)
let error_desc_get_bucket err_desc = Tags.get err_desc.tags Tags.bucket

(** get the value tag, if any *)
let get_value_line_tag tags =
  try
    let value = snd (List.find_exn ~f:(fun (tag, _) -> String.equal tag Tags.value) tags) in
    let line = snd (List.find_exn ~f:(fun (tag, _) -> String.equal tag Tags.line) tags) in
    Some [value; line]
  with Not_found_s _ | Stdlib.Not_found -> None


(** extract from desc a value on which to apply polymorphic hash and equality *)
let desc_get_comparable err_desc =
  match get_value_line_tag err_desc.tags with Some sl' -> sl' | None -> err_desc.descriptions


(** hash function for error_desc *)
let error_desc_hash desc = Hashtbl.hash (desc_get_comparable desc)

(** equality for error_desc *)
let error_desc_equal desc1 desc2 =
  [%equal: string list] (desc_get_comparable desc1) (desc_get_comparable desc2)


let line_tag_ tags tag loc =
  let line_str = string_of_int loc.Location.line in
  Tags.update tags tag line_str ;
  let s = "line " ^ line_str in
  if loc.Location.col <> -1 then
    let col_str = string_of_int loc.Location.col in
    s ^ ", column " ^ col_str
  else s


let at_line_tag tags tag loc = "at " ^ line_tag_ tags tag loc

let at_line tags loc = at_line_tag tags Tags.line loc

let desc_condition_always_true_false i cond_str_opt loc =
  let tags = Tags.create () in
  let value = match cond_str_opt with None -> "" | Some s -> s in
  let tt_ff = if IntLit.iszero i then "false" else "true" in
  Tags.update tags Tags.value value ;
  let description =
    Format.sprintf "Boolean condition %s is always %s %s"
      (if String.equal value "" then "" else " " ^ MF.monospaced_to_string value)
      tt_ff (at_line tags loc)
  in
  {no_desc with descriptions= [description]; tags= !tags}
