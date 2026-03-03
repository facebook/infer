(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
(* 1. Create a cache at the module level *)
let demangle_cache = String.Table.create ()

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
    (* 2. Check the cache *)
    match Hashtbl.find demangle_cache mangled with
    | Some demangled_name ->
        demangled_name
    | None ->
        let len = String.length mangled in
        (* We now track 'last_end' to know exactly where the identifier finished *)
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
        let parsed_result, last_end = parse 0 "" 0 in
        let final_result =
          if String.is_empty parsed_result then mangled
          else
            (* --- THE SUFFIX VALIDATOR --- *)
            (* Extract whatever is left in the string after our parsed name *)
            let suffix = String.sub mangled ~pos:last_end ~len:(len - last_end) in
            let clean_suffix = String.chop_suffix_if_exists suffix ~suffix:"*" in
            let clean_suffix = String.chop_suffix_if_exists clean_suffix ~suffix:"D" in
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
        in
        Hashtbl.set demangle_cache ~key:mangled ~data:final_result ;
        final_result
