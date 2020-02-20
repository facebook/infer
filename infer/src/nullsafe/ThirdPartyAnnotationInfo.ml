(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module Hashtbl = Caml.Hashtbl

type signature_info = {filename: string; line_number: int; nullability: ThirdPartyMethod.nullability}

type storage = {signature_map: signature_map; filenames: string list}

and signature_map = (ThirdPartyMethod.unique_repr, signature_info) Hashtbl.t

let create_storage () = {signature_map= Hashtbl.create 1; filenames= []}

type file_parsing_error =
  {line_number: int; unparsable_method: string; parsing_error: ThirdPartyMethod.parsing_error}

let pp_parsing_error fmt {line_number; unparsable_method; parsing_error} =
  Format.fprintf fmt "Line %d: Could not parse method '%s': %s" line_number unparsable_method
    (ThirdPartyMethod.string_of_parsing_error parsing_error)


(* Consequtively evaluates results for all elements in a list,
   returns Ok (folded results) if all succeeded, or the first error.
   The evaluator function [f] has access to element's index.
 *)
let bind_list_with_index ~init list ~f =
  List.foldi list ~init:(Ok init) ~f:(fun index acc elem ->
      Result.bind acc ~f:(fun acc -> f acc index elem) )


let is_whitespace_or_comment line =
  let stripped_line = String.strip line in
  String.is_empty stripped_line || String.is_prefix stripped_line ~prefix:"//"


let parse_line_and_add_to_storage signature_map ~filename ~line_index line =
  let open Result in
  if is_whitespace_or_comment line then Ok signature_map
  else
    ThirdPartyMethod.parse line
    >>= fun (signature, nullability) ->
    Ok
      ( Hashtbl.add signature_map signature {filename; line_number= line_index + 1; nullability} ;
        signature_map )


let add_from_signature_file storage ~filename ~lines =
  (* each line in a file should represent a method signature *)
  let open Result in
  let new_filenames = storage.filenames @ [filename] in
  bind_list_with_index lines ~init:storage.signature_map
    ~f:(fun signature_map line_index method_as_str ->
      parse_line_and_add_to_storage signature_map ~filename ~line_index method_as_str
      |> Result.map_error ~f:(fun parsing_error ->
             {line_number= line_index + 1; unparsable_method= method_as_str; parsing_error} ) )
  >>= fun new_map -> Ok {signature_map= new_map; filenames= new_filenames}


let find_nullability_info {signature_map} unique_repr = Hashtbl.find_opt signature_map unique_repr

let does_package_match_file ~package sig_filename =
  (* Filename should be of form <some.package.sig>, where <some.package> is prefix
     for third party packages that should be declared in this file *)
  let allowed_package_prefix = String.chop_suffix_exn sig_filename ~suffix:".sig" in
  String.is_prefix package ~prefix:allowed_package_prefix


let lookup_related_sig_file {filenames} ~package =
  List.filter filenames ~f:(does_package_match_file ~package)
  (* In case two different files match the package, we choose the most specific;
     it will have the longest length *)
  |> List.max_elt ~compare:(fun name1 name2 -> String.length name1 - String.length name2)


let lookup_related_sig_file_for_proc storage procname =
  let package =
    match procname with
    | Procname.Java java_pname ->
        Procname.Java.get_package java_pname
    | _ ->
        None
  in
  Option.bind package ~f:(fun package -> lookup_related_sig_file storage ~package)


let is_third_party_proc storage procname =
  let is_from_config =
    match procname with
    | Procname.Java java_pname ->
        Procname.Java.is_external java_pname
    | _ ->
        false
  in
  let lookup_sig_file _ = lookup_related_sig_file_for_proc storage procname in
  is_from_config || Option.is_some (lookup_sig_file ())


(* There is a bit of duplication relative to [is_third_party_proc] due to mismatch between
   [Typ.Name.Java] and [JavaClassName]. When those types are consolidated would be a good
   idea to refactor this function. *)
let is_third_party_typ storage typ =
  IOption.Let_syntax.(
    let* typ_name = Typ.name typ in
    let+ class_name =
      match typ_name with Typ.JavaClass class_name -> return class_name | _ -> None
    in
    let is_from_config = JavaClassName.is_external_via_config class_name in
    let lookup_sig_file _ =
      let* package = JavaClassName.package class_name in
      lookup_related_sig_file storage ~package
    in
    is_from_config || Option.is_some (lookup_sig_file ()))
  |> Option.value ~default:false
