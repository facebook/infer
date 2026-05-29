(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
include PpDetailLevel

type builtin =
  | DerivedEnumEquals
  | DynamicCall
  | InitTuple
  | Memcpy
  | NonDet
  | ObjcMsgSend
  | ObjcMsgSendSuper2
  | ObjcAllocFromSwift  (** Swift-driven ObjC allocation, takes [(sizeof typ, dynamic class)]. *)
  | OptionalInitNone  (** Swift [Optional<T>] construction of [.none]. *)
  | OptionalInitSome  (** Swift [Optional<T>] construction of [.some(payload)]. *)
  | OptionalInitSg
      (** Swift [Optional<T>] construction from a symbolic discriminator, [Sg]-class shape (e.g.
          [Int?]). Path-splits on the tag value: [tag = 1] is [.none], [tag = 0] is [.some]. *)
  | OptionalInitTuple
      (** Swift [Optional<T>] construction from a symbolic discriminator, 2-component
          [__infer_tuple_class<int,int>] shape (e.g. [String?]). Path-splits on the tag value:
          [tag = 0] is [.none], non-zero is [.some]. *)
  | OptionalUnsafelyUnwrapped  (** Swift [Optional<T>.unsafelyUnwrapped]. *)
  | OptionalForceUnwrapTrap
      (** Swift Optional force-unwrap (postfix [!]) trap: emitted on the proven-[.none] branch of
          the [if eq(disc, 0) then assert_fail] idiom. *)
  | SwiftAlloc  (** Swift class allocation, takes a single [sizeof typ] arg. *)
  | SwiftGetDynamicType
  | MetadataEquals  (** Used to compare metadata of two types. *)
[@@deriving compare, equal, yojson_of, sexp, hash, normalize, enumerate]

type t =
  | ClassMethod of {class_name: Typ.Name.t; method_name: Mangled.t}
  | Function of {function_name: Mangled.t}
  | Builtin of builtin
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

let mk_function mangled = Function {function_name= mangled}

let mk_class_method class_name mangled = ClassMethod {class_name; method_name= mangled}

let mk_builtin builtin = Builtin builtin

let show_builtin = function
  | NonDet ->
      "llvm_nondet"
  | InitTuple ->
      "llvm_init_tuple"
  | DynamicCall ->
      "llvm_dynamic_call"
  | DerivedEnumEquals ->
      "__derived_enum_equals"
  | ObjcMsgSend ->
      "objc_msgSend"
  | ObjcMsgSendSuper2 ->
      "objc_msgSendSuper2"
  | ObjcAllocFromSwift ->
      "__objc_alloc_from_swift"
  | OptionalInitNone ->
      "__swift_optional_init_none"
  | OptionalInitSome ->
      "__swift_optional_init_some"
  | OptionalInitSg ->
      "__swift_optional_init_sg"
  | OptionalInitTuple ->
      "__swift_optional_init_tuple"
  | OptionalUnsafelyUnwrapped ->
      "__swift_optional_unsafely_unwrapped"
  | OptionalForceUnwrapTrap ->
      "__swift_optional_force_unwrap_trap"
  | SwiftAlloc ->
      "swift_allocObject"
  | Memcpy ->
      "memcpy"
  | SwiftGetDynamicType ->
      "swift_getDynamicType"
  | MetadataEquals ->
      "__swift_metadata_equals"


let get_function_name osig =
  match osig with
  | ClassMethod {method_name} ->
      method_name
  | Function {function_name} ->
      function_name
  | Builtin builtin ->
      Mangled.from_string (show_builtin builtin)


let pp_plain_class_name fmt proc_name =
  match proc_name with
  | ClassMethod {class_name= Typ.SwiftClass name} ->
      SwiftClassName.pp_plain_name fmt name
  | _ ->
      ()


(** Walk a Swift mangled symbol and return the dotted path [Module(.Type)?(.method)?]. Reads
    length-prefixed identifiers along with the char that follows each — the trailing context
    classifies the identifier:
    - uppercase [C/V/O/P] = type marker — the identifier was a class / struct / enum / protocol;
    - digit = another length-prefixed token follows directly, signalling argument labels (so the
      identifier was the method);
    - lowercase / end = falls through to the "last identifier" fallback.

    Substitution-compressed identifiers ([0<refs><n><literal>], where [<refs>] is uppercase
    back-references into Swift's substitution table that we don't expand) contribute only their
    literal-prefix portion — better than skipping them entirely.

    Init constructors don't carry a separate length-prefixed method identifier — they're signalled
    by an [fC]/[fc] suffix on the whole symbol — so we render those as [<Module>.<Type>.init]. *)
let parse_swift_mangled_head mangled =
  let len = String.length mangled in
  if len < 4 || not (String.is_prefix mangled ~prefix:"$s") then None
  else
    let consume_digits start =
      let rec aux i = if i < len && Char.is_digit mangled.[i] then aux (i + 1) else i in
      aux start
    in
    let read_ident pos =
      let end_digits = consume_digits pos in
      if Int.equal pos end_digits then None
      else
        let len_str = String.sub mangled ~pos ~len:(end_digits - pos) in
        match int_of_string_opt len_str with
        | Some n when n > 0 && end_digits + n <= len ->
            Some (String.sub mangled ~pos:end_digits ~len:n, end_digits + n)
        | _ ->
            (* Substitution-compressed identifier: bare [0] then uppercase back-refs then a plain
               [<n><literal>] tail. Fish out the literal portion only. *)
            if Int.equal end_digits (pos + 1) && Char.equal mangled.[pos] '0' then
              let rec skip_upper i =
                if i < len && Char.is_uppercase mangled.[i] then skip_upper (i + 1) else i
              in
              let after_refs = skip_upper end_digits in
              if after_refs > end_digits then
                let end_lit_digits = consume_digits after_refs in
                if end_lit_digits > after_refs then
                  let lit_len_str =
                    String.sub mangled ~pos:after_refs ~len:(end_lit_digits - after_refs)
                  in
                  match int_of_string_opt lit_len_str with
                  | Some n when n > 0 && end_lit_digits + n <= len ->
                      Some (String.sub mangled ~pos:end_lit_digits ~len:n, end_lit_digits + n)
                  | _ ->
                      None
                else None
              else None
            else None
    in
    let skip_to_next_digit pos =
      let rec aux i = if i < len && not (Char.is_digit mangled.[i]) then aux (i + 1) else i in
      aux pos
    in
    let rec collect pos acc =
      let p = skip_to_next_digit pos in
      if p >= len then List.rev acc
      else
        match read_ident p with
        | None ->
            List.rev acc
        | Some (id, p') ->
            let next_char = if p' < len then Some mangled.[p'] else None in
            collect p' ((id, next_char) :: acc)
    in
    let identifiers = collect 2 [] in
    let is_type_marker = function
      | Some 'C' | Some 'V' | Some 'O' | Some 'P' ->
          true
      | _ ->
          false
    in
    let is_digit_ctx = function Some c -> Char.is_digit c | _ -> false in
    let ends_with_init =
      String.is_suffix mangled ~suffix:"fC" || String.is_suffix mangled ~suffix:"fc"
    in
    let path =
      match identifiers with
      | [] ->
          []
      | [(only, _)] ->
          [only]
      | (md, _) :: rest -> (
          let typ_opt, rest_after_type =
            match rest with
            | (typ, ctx) :: more when is_type_marker ctx ->
                (Some typ, more)
            | _ ->
                (None, rest)
          in
          let method_opt =
            if ends_with_init then Some "init"
            else
              match List.find rest_after_type ~f:(fun (_, ctx) -> is_digit_ctx ctx) with
              | Some (m, _) ->
                  Some m
              | None -> (
                match List.last rest_after_type with Some (m, _) -> Some m | None -> None )
          in
          let prefix = match typ_opt with Some t -> [md; t] | None -> [md] in
          match method_opt with
          | Some m when not (List.mem prefix m ~equal:String.equal) ->
              prefix @ [m]
          | _ ->
              prefix )
    in
    match path with [] -> None | _ -> Some path


let memo_shorten : string IString.Hash.t = IString.Hash.create 256

(** Shrink a Swift mangled name to its dotted path. Display-only — procname identity is still the
    raw [Mangled.t] underneath, so two overloads on the same class look the same in a trace
    (surrounding call-site context disambiguates them). Non-Swift symbols (no [$s] prefix) and
    parses we don't recognise are returned unchanged. *)
let shorten_swift_mangled mangled =
  match IString.Hash.find_opt memo_shorten mangled with
  | Some cached ->
      cached
  | None ->
      let result =
        match parse_swift_mangled_head mangled with
        | Some parts ->
            String.concat ~sep:"." parts
        | None ->
            mangled
      in
      IString.Hash.add memo_shorten mangled result ;
      result


(* Shorten Swift mangled names ([$s...]) for display in trace messages only. Restricted to
   [NameOnly] / [FullNameOnly] verbosities (the trace-message rendering path used by
   [PulseCallEvent.pp]); [Simple] and [Non_verbose] keep the raw mangled name because those flow
   into procname identity / lookup paths (e.g. [Procname.to_string], the [procedure_id] in the
   report, the issue [err_key] qualifier that feeds report dedup) where a shortened key can collide
   across distinct closure / overload procnames and silently swallow reports. *)
let pp_method_name_shortened fmt method_name =
  F.pp_print_string fmt (shorten_swift_mangled (Mangled.to_string method_name))


let pp verbosity fmt proc_name =
  let sep = "." in
  match proc_name with
  | ClassMethod osig -> (
    match verbosity with
    | Simple ->
        F.pp_print_string fmt (Mangled.to_string osig.method_name)
    | Non_verbose ->
        F.fprintf fmt "%a%s%s" pp_plain_class_name proc_name sep
          (Mangled.to_string osig.method_name)
    | NameOnly | FullNameOnly ->
        F.fprintf fmt "%a%s%a" pp_plain_class_name proc_name sep pp_method_name_shortened
          osig.method_name
    | Verbose ->
        F.fprintf fmt "%s%s%a" (Typ.Name.name osig.class_name) sep Mangled.pp_full osig.method_name
    )
  | Function osig -> (
    match verbosity with
    | Simple | Non_verbose ->
        F.pp_print_string fmt (Mangled.to_string osig.function_name)
    | NameOnly | FullNameOnly ->
        pp_method_name_shortened fmt osig.function_name
    | Verbose ->
        F.pp_print_string fmt (Mangled.to_string_full osig.function_name) )
  | Builtin builtin ->
      F.pp_print_string fmt (show_builtin builtin)


let builtin_from_string =
  let tbl = IString.Hash.create 100 in
  List.iter all_of_builtin ~f:(fun builtin -> IString.Hash.add tbl (show_builtin builtin) builtin) ;
  fun str -> IString.Hash.find_opt tbl str


let to_string osig = Format.asprintf "%a" (pp Simple) osig
