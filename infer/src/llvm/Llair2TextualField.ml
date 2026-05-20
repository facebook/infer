(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Llair
module State = Llair2TextualState
module TypeName = Llair2TextualTypeName

let get_suffix = ".get"

let set_suffix = ".set"

let modify_suffix = ".modify"

module OffsetIndex = struct
  (** Extract field name from getter method's unmangled name. E.g., "age.get" -> Some "age", "foo"
      -> None *)

  let extract_field_name_from_getter plain_name = String.chop_suffix ~suffix:get_suffix plain_name

  (** Recursively traverse expression to find the innermost Select with offset and class type. For
      nested types like %ptr_elt*[1]:%T5Hello6PersonC[0]:%TSi, we want offset 1 (PersonC) which is
      the innermost Select in the expression tree. *)
  let rec extract_offset_and_type_from_exp lang ~mangled_map ~plain_map struct_map func
      (exp : Llair.Exp.t) =
    let formals_types = func.formals_types |> StdUtils.iarray_to_list in
    let formals = func.formals |> StdUtils.iarray_to_list in
    match exp with
    | Ap1 (Select offset, typ, inner_exp) -> (
      (* First recurse to find innermost Select *)
      match
        extract_offset_and_type_from_exp lang ~mangled_map ~plain_map struct_map func inner_exp
      with
      | Some _ as some_result ->
          (* Use the innermost result *)
          some_result
      | None -> (
        (* This is the innermost Select, use it if it has a struct type *)
        match (typ, inner_exp, formals, formals_types) with
        | Llair.Typ.Struct {name; _}, _, _, _
          when not (String.equal Textual.BaseTypeName.swift_any_type_name.value name) ->
            Some
              ( TypeName.struct_name_of_mangled_name lang ~mangled_map:(Some mangled_map) struct_map
                  name
              , offset )
        | _, Reg {id}, [arg], [arg_typ] when Int.equal id (Reg.id arg) ->
            Option.map
              ~f:(fun typ -> (typ, offset))
              (TypeName.struct_name_of_plain_name plain_map arg_typ)
        | _ ->
            None ) )
    | Ap1 (_, _, inner_exp) ->
        extract_offset_and_type_from_exp lang ~mangled_map ~plain_map struct_map func inner_exp
    | Ap2 (_, _, exp1, exp2) -> (
      match extract_offset_and_type_from_exp lang ~mangled_map ~plain_map struct_map func exp1 with
      | Some _ as some_result ->
          some_result
      | None ->
          extract_offset_and_type_from_exp lang ~mangled_map ~plain_map struct_map func exp2 )
    | Ap3 (_, _, exp1, exp2, exp3) -> (
      match extract_offset_and_type_from_exp lang ~mangled_map ~plain_map struct_map func exp1 with
      | Some _ as some_result ->
          some_result
      | None -> (
        match
          extract_offset_and_type_from_exp lang ~mangled_map ~plain_map struct_map func exp2
        with
        | Some _ as some_result ->
            some_result
        | None ->
            extract_offset_and_type_from_exp lang ~mangled_map ~plain_map struct_map func exp3 ) )
    | _ ->
        None


  (** Extract the first instance of offset and class type from instructions in a function *)
  let extract_offset_from_instructions lang ~mangled_map ~plain_map struct_map func cmnd =
    let instrs = StdUtils.iarray_to_list cmnd in
    List.find_map instrs ~f:(fun inst ->
        match inst with
        | Llair.Load {ptr; _} ->
            extract_offset_and_type_from_exp lang ~mangled_map ~plain_map struct_map func ptr
        | Move {reg_exps: (Reg.t * Exp.t) NS.iarray} ->
            let exps = StdUtils.iarray_to_list reg_exps in
            List.find_map exps ~f:(fun (_, exp) ->
                extract_offset_and_type_from_exp lang ~mangled_map ~plain_map struct_map func exp )
        | _ ->
            None )


  (** Build the field offset map from getter/setter functions. This maps (class_name, offset) ->
      field_name by analyzing getter/setter methods. *)
  let build_field_offset_map lang ~mangled_map ~plain_map struct_map functions =
    let field_offset_map = State.FieldOffsetMap.create 64 in
    let process_function (func_name, func) =
      match FuncName.unmangled_name func_name with
      | None ->
          ()
      | Some plain_name -> (
        match extract_field_name_from_getter plain_name with
        | None ->
            ()
        | Some field_name -> (
            let entry = func.Llair.entry in
            match
              extract_offset_from_instructions lang ~mangled_map ~plain_map struct_map func
                entry.cmnd
            with
            | Some (class_name, offset) ->
                let key = State.FieldOffset.{class_name; offset} in
                State.FieldOffsetMap.replace field_offset_map key
                  (Textual.FieldName.of_string field_name)
            | None ->
                () ) )
    in
    List.iter functions ~f:process_function ;
    field_offset_map
end

let field_of_pos type_name pos =
  let name = Format.asprintf "field_%s" (Int.to_string pos) in
  Textual.{enclosing_class= type_name; name= FieldName.of_string name}


let field_of_pos_with_map field_offset_map type_name pos =
  let key = State.FieldOffset.{class_name= type_name; offset= pos} in
  match State.FieldOffsetMap.find_opt field_offset_map key with
  | Some field_name ->
      Textual.{enclosing_class= type_name; name= field_name}
  | None ->
      field_of_pos type_name pos


(* --- Byte-offset based field lookup --------------------------------------- *)
(* Swift class instances are laid out in LLVM as
     { metadata_ptr (8B), refcount (8B), <user fields...> }
   so user fields begin 16 bytes past the start of the object.  Textual omits
   the header from the field list, so an LLVM byte offset of 16 corresponds to
   the *first* declared Textual field. *)
let swift_class_header_bytes = 16

(* Conservative byte size for a Textual field's type, used only for advancing
   the byte-offset cursor while looking up a field by byte offset.
   - Optional / existential containers (Swift mangling suffix [Sg] or [P])
     occupy two pointer-sized slots (value + witness), i.e. 16 bytes.
   - Everything else is treated as a single pointer (8 bytes).
   The estimates only need to be accurate enough for the lookup to land on the
   correct field boundary; on a mismatch the caller falls back to [llvm_nondet]. *)
let estimate_field_byte_size (typ : Textual.Typ.t) =
  let is_existential_or_optional name =
    String.is_suffix ~suffix:"Sg" name || String.is_suffix ~suffix:"_p" name
  in
  match typ with
  | Textual.Typ.Ptr (Struct {name; _}, _)
    when is_existential_or_optional (Textual.BaseTypeName.to_string name) ->
      16
  | _ ->
      8


let lookup_field_by_byte_offset struct_map type_name byte_offset =
  if byte_offset < swift_class_header_bytes then None
  else
    match Textual.TypeName.Map.find_opt type_name struct_map with
    | None ->
        None
    | Some (textual_struct : Textual.Struct.t) ->
        let rec walk fields cursor =
          match fields with
          | [] ->
              None
          | (field_decl : Textual.FieldDecl.t) :: rest ->
              if Int.equal cursor byte_offset then Some field_decl.qualified_name
              else if cursor > byte_offset then None
              else walk rest (cursor + estimate_field_byte_size field_decl.typ)
        in
        walk textual_struct.fields swift_class_header_bytes


let tuple_field_prefix = "__infer_tuple_field_"

let tuple_field_of_pos type_name pos =
  let name = Format.sprintf "%s%s" tuple_field_prefix (Int.to_string pos) in
  Textual.{enclosing_class= type_name; name= FieldName.of_string name}


(* Module-level cache for Wvd demangling *)
let wvd_cache = Hashtbl.create (module String)

(** * Parses a Swift Field Offset Vector (Wvd) mangled name to extract the * enclosing class/struct
    name and the specific field name. * * Example Input:
    $s5Hello17WidgetDisplayViewC8hostCellAA09DashboardbF0CSgvpWvd * Example Output: Some
    ("T5Hello17WidgetDisplayViewC", "hostCell") * * Why a manual parser instead of regex? * Swift
    often appends the property's type signature to the end of the mangled name * (e.g., the
    `AA09...` part above). If the property's type happens to be a class * or struct, a greedy regex
    will accidentally skip the parent class and match * the `C` or `V` marker of the property's type
    instead. * * This parser strictly evaluates the Swift ABI length-prefixes (e.g., reading `5`, *
    then skipping 5 characters for "Hello") to deterministically stop at the exact * class boundary,
    guaranteeing we extract the correct field name. * Results are cached to ensure O(1) performance
    on hot compiler paths. *)
let extract_class_and_field_from_wvd mangled =
  match Hashtbl.find wvd_cache mangled with
  | Some cached_result ->
      cached_result
  | None ->
      let len = String.length mangled in
      let rec consume_digits pos =
        if pos < len && Char.is_digit mangled.[pos] then consume_digits (pos + 1) else pos
      in
      let rec parse pos =
        if pos >= len then None
        else
          let end_digits = consume_digits pos in
          if Int.equal pos end_digits then
            (* No length-prefix digit at [pos]: this is typically a Swift
               substitution back-reference (e.g. "0A26..." for nested types)
               or the leftover marker after a substitution-compressed
               identifier (e.g. "05AudioB9ViewModel..." where the parser
               consumed "05Audio" as length=5, leaving "B" here). Skip the
               non-digit char and continue scanning so we can still locate
               the trailing class/struct boundary marker (C/V) and the
               field's length-prefix. *)
            parse (pos + 1)
          else
            let len_str = String.sub mangled ~pos ~len:(end_digits - pos) in
            match int_of_string_opt len_str with
            | Some str_len -> (
                let str_end = end_digits + str_len in
                if str_end >= len then None
                else
                  let next_char = mangled.[str_end] in
                  (* When the length-prefixed name is followed by a Swift substitution
                     back-reference (typically `A<idx>`, where `<idx>` is a single base-36
                     char) and *then* the class/struct marker, treat the whole
                     `<length-prefix><name><A><idx><C/V>` chunk as the class designator
                     so the property segment that follows is parsed correctly. Without
                     this, the recursive `parse` step walks past the marker and the parser
                     ends up returning `unknown_field`.

                     Concretely covers the parent-namespace back-reference shape
                     `<class>AAC<property>...Wvd` exercised by
                     [wvd_substitution_parent_namespace]. The two-char skip (`A` + one
                     index char) is the common production shape; the sibling
                     [wvd_substitution_nested] pattern (where the substitution is part of
                     a *nested-type* expansion) is still handled by the existing
                     non-digit fallthrough in `parse pos+1` and does not need this
                     skip. *)
                  let class_marker_pos =
                    if Char.equal next_char 'C' || Char.equal next_char 'V' then Some str_end
                    else if
                      Char.equal next_char 'A'
                      && str_end + 2 < len
                      && ( Char.equal mangled.[str_end + 2] 'C'
                         || Char.equal mangled.[str_end + 2] 'V' )
                    then Some (str_end + 2)
                    else None
                  in
                  match class_marker_pos with
                  | Some cv_pos -> (
                      let class_part_len = cv_pos - 2 + 1 in
                      let class_part = String.sub mangled ~pos:2 ~len:class_part_len in
                      let class_name = "T" ^ class_part in
                      let prop_pos = cv_pos + 1 in
                      let end_prop_digits = consume_digits prop_pos in
                      if Int.equal prop_pos end_prop_digits then None
                      else
                        let prop_len_str =
                          String.sub mangled ~pos:prop_pos ~len:(end_prop_digits - prop_pos)
                        in
                        (* A leading '0' on a multi-digit length prefix is Swift's marker for a
                         word-substitution-compressed identifier (e.g. "08reactionC4View" encodes
                         "reactionBubbleView" with "Bubble" back-referenced from an earlier name).
                         We don't expand substitutions, so reading the literal `0<n>` as length=n
                         produces a truncated wrong field name. Two distinct properties on the
                         same class would then collide on the truncated common prefix and confuse
                         downstream analyses (false retain cycles, etc.).

                         Disambiguate by hashing the full mangled suffix to a short hex tag so
                         two distinct properties keep distinct identities. When the suffix
                         contains a long-enough lowercase letter run (typically the un-compressed
                         literal portion of the property name, e.g. "reaction" or "viewScreenshot"),
                         prepend it as a human-readable hint so the field is recognisable in
                         textual dumps and Pulse trace messages. *)
                        let is_substitution_compressed =
                          String.length prop_len_str > 1
                          && Char.equal (String.get prop_len_str 0) '0'
                        in
                        if is_substitution_compressed then
                          let suffix = String.sub mangled ~pos:prop_pos ~len:(len - prop_pos) in
                          let sanitized =
                            String.map suffix ~f:(fun c ->
                                if Char.is_alphanum c || Char.equal c '_' then c else '_' )
                          in
                          let hash_tag =
                            String.sub (Utils.string_crc_hex32 sanitized) ~pos:0 ~len:8
                          in
                          (* The leading [0<n>] block names the literal-prefix portion of the
                           property name (Swift back-references the rest from the substitution
                           table). [int_of_string_opt] silently drops the leading 0, so [prop_len]
                           is exactly the length of that literal prefix. *)
                          let literal_hint =
                            match int_of_string_opt prop_len_str with
                            | Some prop_len when prop_len > 0 && end_prop_digits + prop_len <= len
                              ->
                                Some (String.sub mangled ~pos:end_prop_digits ~len:prop_len)
                            | _ ->
                                None
                          in
                          let field_name =
                            match literal_hint with
                            | Some hint ->
                                "field_" ^ hint ^ "_" ^ hash_tag
                            | None ->
                                "field_" ^ hash_tag
                          in
                          Some (class_name, field_name)
                        else
                          match int_of_string_opt prop_len_str with
                          | Some prop_len ->
                              let prop_name =
                                String.sub mangled ~pos:end_prop_digits ~len:prop_len
                              in
                              Some (class_name, prop_name)
                          | None ->
                              None )
                  | None ->
                      parse str_end )
            | None ->
                None
      in
      let result =
        if String.is_prefix mangled ~prefix:"$s" then
          match parse 2 with Some (c, f) -> (Some c, f) | None -> (None, "unknown_field")
        else (None, "unknown_field")
      in
      (* Cache the parsed result for all future O(1) lookups *)
      Hashtbl.set wvd_cache ~key:mangled ~data:result ;
      result
