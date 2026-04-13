(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** 1. Unified Cache: Stores (mangled_input -> (parsed_identifier * last_parsed_index)) to avoid
    re-traversing the same mangled strings across strict/relaxed calls. *)
let demangle_raw_cache = String.Table.create ()

(** Core Parser: Extracts the last length-prefixed identifier from a Swift mangled string. This is
    the exact original recursive logic. *)
let parse_mangled_identifier_core mangled =
  Hashtbl.find_or_add demangle_raw_cache mangled ~default:(fun () ->
      let len = String.length mangled in
      let rec parse idx last_found last_end =
        if idx >= len then (last_found, last_end)
        else if Char.is_digit mangled.[idx] then
          let rec find_digit_end i =
            if i < len && Char.is_digit mangled.[i] then find_digit_end (i + 1) else i
          in
          let digit_end = find_digit_end idx in
          if digit_end < len && Char.equal mangled.[digit_end] '_' then
            parse (digit_end + 1) last_found last_end
          else
            let num_str = String.sub mangled ~pos:idx ~len:(digit_end - idx) in
            match Int.of_string_opt num_str with
            | Some n when n > 0 && digit_end + n <= len ->
                let name = String.sub mangled ~pos:digit_end ~len:n in
                (* Update last_end to the index right after the extracted name *)
                parse (digit_end + n) name (digit_end + n)
            | _ ->
                parse (idx + 1) last_found last_end
        else parse (idx + 1) last_found last_end
      in
      parse 0 "" 0 )


(** Generalized heuristic to extract the plain class name from a Swift mangled name. It finds the
    *last* length-prefixed string in the mangling. *)
let demangle_swift_class_name (mangled : string) : string =
  (* Fast bypass for frames and obvious non-mangled types *)
  if
    String.is_suffix mangled ~suffix:"::Frame"
    || not
         ( String.is_prefix mangled ~prefix:"$s"
         || String.is_prefix mangled ~prefix:"%T"
         || String.is_prefix mangled ~prefix:"_T"
         || String.is_prefix mangled ~prefix:"T" )
  then mangled
  else
    let parsed_result, last_end = parse_mangled_identifier_core mangled in
    if String.is_empty parsed_result then mangled
    else
      (* --- THE SUFFIX VALIDATOR --- *)
      (* Extract whatever is left in the string after our parsed name *)
      let suffix = String.sub mangled ~pos:last_end ~len:(String.length mangled - last_end) in
      let clean_suffix =
        String.chop_suffix_if_exists suffix ~suffix:"*" |> String.chop_suffix_if_exists ~suffix:"D"
      in
      (* In the Swift ABI, after the length-prefixed identifier, the mangler appends a type kind.
         If the string represents metadata, functions, or coroutines, it will have a complex
         suffix (e.g., `XMT` for Metatypes, `WP` for Witness Tables, `vM` for accessors).
         We ONLY want to extract and demangle clean nominal instance types.

         Valid Nominal Instance Type Suffixes:
           ""   : Plain type (sometimes stripped earlier in the pipeline)
           "C"  : Class
           "V"  : Struct (Value type)
           "O"  : Enum
           "P"  : Protocol
           "Sg" : Optional (Swift's generic Enum)

         Combinations (e.g., "CSg", "SgV") represent Optionals of those specific nominal types. *)
      match clean_suffix with
      | ""
      | "C"
      | "V"
      | "O"
      | "P"
      | "Sg"
      | "CSg"
      | "VSg"
      | "OSg"
      | "PSg"
      | "SgC"
      | "SgV"
      | "SgO"
      | "SgP" ->
          parsed_result (* It is a valid instance type! *)
      | _ ->
          (* It has extra garbage (like XMT, WP, vM, tcfC). It's metadata/function. Do not demangle! *)
          mangled


(** 2. RELAXED VERSION: Used to recover types from procedure names (initializers/getters).
    Specifically handles the '$s...fC' (allocating init) and '$s...vg' (getter) patterns. *)
let extract_type_from_mangled_proc (mangled : string) : string option =
  (* Procedures for our purposes usually start with $s or _T *)
  if not (String.is_prefix mangled ~prefix:"$s" || String.is_prefix mangled ~prefix:"_T") then None
  else
    let parsed_result, last_end = parse_mangled_identifier_core mangled in
    if String.is_empty parsed_result then None
    else
      let suffix = String.sub mangled ~pos:last_end ~len:(String.length mangled - last_end) in
      (* Strip common pointer/metadata markers from the suffix for the check *)
      let clean_suffix = String.chop_suffix_if_exists suffix ~suffix:"*" in
      (* We accept suffixes that indicate class instance production/access:
         fC (init), vg (getter), au (accessor), or C (class marker). *)
      if
        String.is_suffix clean_suffix ~suffix:"fC"
        || String.is_suffix clean_suffix ~suffix:"vg"
        || String.is_suffix clean_suffix ~suffix:"au"
        || String.contains clean_suffix 'C' || String.is_empty clean_suffix
      then Some parsed_result
      else None
