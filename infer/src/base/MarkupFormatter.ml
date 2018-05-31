(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type 'a formatter =
  { wrap_monospaced: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit
  ; pp_monospaced: Format.formatter -> string -> unit
  ; monospaced_to_string: string -> string
  ; wrap_code: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit
  ; pp_code: Format.formatter -> string -> unit
  ; wrap_bold: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit
  ; pp_bold: Format.formatter -> string -> unit
  ; bold_to_string: string -> string }

module NoFormatter : sig
  val formatter : 'a formatter
end = struct
  let wrap_simple pp fmt x = pp fmt x

  let pp_simple = wrap_simple Format.pp_print_string

  let formatter =
    { wrap_monospaced= wrap_simple
    ; pp_monospaced= pp_simple
    ; monospaced_to_string= Fn.id
    ; wrap_code= wrap_simple
    ; pp_code= pp_simple
    ; wrap_bold= wrap_simple
    ; pp_bold= pp_simple
    ; bold_to_string= Fn.id }
end

module PhabricatorFormatter : sig
  val formatter : 'a formatter
end = struct
  (* https://secure.phabricator.com/book/phabricator/article/remarkup/ *)
  let wrap_monospaced pp fmt x = Format.fprintf fmt "`%a`" pp x

  let pp_monospaced fmt s = wrap_monospaced Format.pp_print_string fmt s

  let monospaced_to_string s = Format.asprintf "%a" pp_monospaced s

  let wrap_code pp fmt x = Format.fprintf fmt "```%a```" pp x

  let pp_code fmt s = wrap_code Format.pp_print_string fmt s

  let wrap_bold pp fmt x = Format.fprintf fmt "**%a**" pp x

  let pp_bold fmt s = wrap_bold Format.pp_print_string fmt s

  let bold_to_string s = Format.asprintf "%a" pp_bold s

  let formatter =
    { wrap_monospaced
    ; pp_monospaced
    ; monospaced_to_string
    ; wrap_code
    ; pp_code
    ; wrap_bold
    ; pp_bold
    ; bold_to_string }
end

let formatter =
  match Config.report_formatter with
  | `No_formatter ->
      NoFormatter.formatter
  | `Phabricator_formatter ->
      PhabricatorFormatter.formatter


let wrap_monospaced = formatter.wrap_monospaced

let pp_monospaced = formatter.pp_monospaced

let monospaced_to_string = formatter.monospaced_to_string

let wrap_code = formatter.wrap_code

let pp_code = formatter.pp_code

let wrap_bold = formatter.wrap_bold

let pp_bold = formatter.pp_bold

let bold_to_string = formatter.bold_to_string
