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

(** Synthetic structural types whose layout is shared across modules and whose field-position-0 has
    no class-specific meaning. Selects on these types are not class-property accesses, so they must
    be skipped by [OffsetIndex.extract_offset_and_type_from_exp] — otherwise a getter that unpacks
    one of them (e.g. a Swift property getter returning a closure value, which unpacks the
    [(fn_ptr, captured_env)] pair before returning) would register a
    [(synthetic_type, 0) -> property_name] entry in [field_offset_map], polluting every later
    Field-position-0 lookup on that synthetic type with a field name from an unrelated class. The
    existing [swift_any_type_name] exclusion is the same idea for the [ptr_elt] opaque pointer. *)
let is_synthetic_structural_type name =
  String.equal Textual.BaseTypeName.swift_any_type_name.value name
  || String.equal "swift::function" name
  || String.equal "objc_block" name


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
        | Llair.Typ.Struct {name; _}, _, _, _ when not (is_synthetic_structural_type name) ->
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


let lookup_field_by_byte_offset ?field_byte_offset_map struct_map type_name byte_offset =
  (* Fast path: if the Wvd descriptor for this (class, byte_offset) is in scope,
     use it directly. Bypasses [estimate_field_byte_size]'s assumptions about
     pointer sizes / header layout, which are wrong for classes with non-pointer
     stored properties (e.g. [Int8], [Bool], [CGRect]). *)
  let from_byte_map =
    match field_byte_offset_map with
    | None ->
        None
    | Some m -> (
        let key = State.FieldOffset.{class_name= type_name; offset= byte_offset} in
        match State.FieldOffsetMap.find_opt m key with
        | Some field_name ->
            Some Textual.{enclosing_class= type_name; name= field_name}
        | None ->
            None )
  in
  match from_byte_map with
  | Some _ ->
      from_byte_map
  | None -> (
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
            walk textual_struct.fields swift_class_header_bytes )


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
                      (* Handle nested Swift classes (e.g. [BCNPublicViewCountShareCardSheet.Controller]
                         mangled as [...ShareCardSheetC10ControllerC<property>...Wvd]).
                         After the first class/struct marker, if what follows is another
                         [<length-prefix><name><C/V>] chunk, it's a nested class continuation
                         (not the property), so extend the class designator through it. Repeat
                         until the next chunk is no longer a length-prefix-followed-by-C/V.

                         Skip extension when the next length prefix starts with [0] — that's
                         Swift's substitution-compression marker on the PROPERTY name (e.g.
                         [<class>C08embeddedC0AA<idx>C...] for property [embeddedWidget] whose
                         type happens to be a class), and the [C] mid-stream is part of the
                         field's type signature, not a nested-class boundary. *)
                      let rec extend_through_nested_classes cv_pos =
                        let next_pos = cv_pos + 1 in
                        if next_pos >= len then cv_pos
                        else
                          let end_d = consume_digits next_pos in
                          if Int.equal next_pos end_d then cv_pos
                          else
                            let len_str =
                              String.sub mangled ~pos:next_pos ~len:(end_d - next_pos)
                            in
                            if String.length len_str > 1 && Char.equal (String.get len_str 0) '0'
                            then cv_pos
                            else
                              match int_of_string_opt len_str with
                              | None ->
                                  cv_pos
                              | Some n ->
                                  let s_end = end_d + n in
                                  if s_end >= len then cv_pos
                                  else if
                                    Char.equal mangled.[s_end] 'C' || Char.equal mangled.[s_end] 'V'
                                  then extend_through_nested_classes s_end
                                  else cv_pos
                      in
                      let cv_pos = extend_through_nested_classes cv_pos in
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


(** Build a [(class_name, byte_offset) → FieldName.t] map from Wvd field-offset descriptor globals:
    every Wvd global whose initialiser is a constant byte offset gives us the property's exact
    layout offset directly. This is the most reliable source of byte-offset ↔ field-name
    correspondence and bypasses the fragile [estimate_field_byte_size] walk used as a fallback.
    Externally- declared Wvds (no initializer) are skipped: they survive translation as a
    declaration but we don't know their numeric offset until the defining module is in scope. *)
let build_field_byte_offset_map lang ~mangled_map struct_map globals_map =
  let m = State.FieldOffsetMap.create 64 in
  Textual.VarName.Map.iter
    (fun _var Llair.GlobalDefn.{name; init; _} ->
      let global_name = Llair.Global.name name in
      if String.is_suffix global_name ~suffix:"Wvd" then
        let class_opt, field_name_str = extract_class_and_field_from_wvd global_name in
        let offset_opt =
          match init with
          | Some (Llair.Exp.Integer {data; _}, _) ->
              Some (NS.Z.to_int data)
          | _ ->
              None
        in
        match (class_opt, offset_opt) with
        | Some mangled_class, Some offset when not (String.equal field_name_str "unknown_field") ->
            let class_name =
              TypeName.struct_name_of_mangled_name lang ~mangled_map:(Some mangled_map) struct_map
                mangled_class
            in
            let key = State.FieldOffset.{class_name; offset} in
            State.FieldOffsetMap.replace m key (Textual.FieldName.of_string field_name_str)
        | _ ->
            () )
    globals_map ;
  m
