(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module StringMap = IString.Map

module Node : sig
  type t =
    | Dict of dict
    | List of t list
    | Str of string
    | Int of int
    | Float of float
    | Bool of bool
    | Null
  [@@deriving compare, equal]

  and dict = t StringMap.t

  val type_field_name : string

  val field_value : string

  val field_id : string

  val field_ctx : string

  val field_func : string

  val field_args : string

  val field_keywords : string

  val field_lineno : string

  val field_end_lineno : string

  val get_type : dict -> t

  val is_type : dict -> String.t -> bool

  val is_line_number_field : String.t -> bool

  val is_type_annotation_field : String.t -> bool

  val get_line_number : dict -> int option

  val get_end_line_number : dict -> int option

  val get_line_number_of_node : t -> int option

  val make_dict_node : (string * t) list -> t

  val find_field_or_null : string -> dict -> t

  val assoc_of_dict : dict -> string * (string * t) list

  val dict_of_assoc : string -> (string * t) list -> dict

  val to_str : ?indent:int -> t -> string

  val pp : Format.formatter -> t -> unit
end

type error

val pp_error : Format.formatter -> error -> unit

val build_parser : unit -> ?filename:string -> string -> (Node.t, error) result

val iter_from_index : f:(Node.t -> unit) -> index_filename:string -> (unit, error list) result
