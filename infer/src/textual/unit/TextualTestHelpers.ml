(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Textual

let sourcefile = SourceFile.create "dummy.sil"

let parse_string_and_verify sourcefile text =
  let open IResult.Let_syntax in
  let* parsed = TextualParser.parse_string sourcefile text in
  TextualVerification.verify_strict parsed
  |> Result.map_error ~f:(fun err -> [TextualParser.VerificationError err])


let parse_module ?(verify = true) text =
  if verify then parse_string_and_verify sourcefile text
  else TextualParser.parse_string sourcefile text


let parse_module_ok ?(verify = true) text =
  parse_module ~verify text |> Result.ok |> Option.value_exn


let parse_module_print_errors text =
  match parse_string_and_verify sourcefile text with
  | Ok _ ->
      raise (Failure "Successfuly parsed a module while expected parsing to fail")
  | Error es ->
      List.iter es ~f:(fun e -> F.printf "%a" (TextualParser.pp_error sourcefile) e)


let remove_effects_in_subexprs lang module_ =
  let _, decls = TextualDecls.make_decls module_ in
  TextualTransform.remove_effects_in_subexprs lang decls module_


let test_restore_ssa text =
  let module_ = TextualParser.parse_string sourcefile text |> Result.ok |> Option.value_exn in
  match TextualVerification.verify_keep_going module_ with
  | Ok (module_, errors) when List.length errors > 0 ->
      F.printf "%a\n" (Textual.Module.pp ~show_location:false) module_ ;
      F.printf "TYPE ERRORS AFTER TRANSFORMATION:@\n" ;
      List.iter errors ~f:(fun err -> F.printf "%a\n" TextualVerification.pp_error err)
  | Ok (module_, _) ->
      F.printf "%a\n" (Textual.Module.pp ~show_location:false) module_ ;
      let lang = Module.lang module_ in
      let module_, _ = TextualTransform.run lang module_ in
      F.printf "AFTER COMPLETE TRANSFORMATION:\n%a\n"
        (Textual.Module.pp ~show_location:false)
        module_
  | Error errs ->
      List.iter errs ~f:(F.printf "%a@\n" (TextualVerification.pp_error_with_sourcefile sourcefile))


let type_check module_ =
  match TextualVerification.verify_strict module_ with
  | Ok _ ->
      F.printf "verification succeeded@\n"
  | Error errs ->
      List.iter errs ~f:(F.printf "%a@\n" (TextualVerification.pp_error_with_sourcefile sourcefile))


let parse_string_and_verify_keep_going text =
  let pp = Textual.Module.pp ~show_location:false in
  let map_error err = [TextualParser.VerificationError err] in
  (let open IResult.Let_syntax in
   let* module_ = TextualParser.parse_string sourcefile text in
   TextualVerification.verify_keep_going module_ |> Result.map_error ~f:map_error )
  |> function
  | Ok (textual, []) ->
      F.printf "verification succeeded - no warnings@\n------@\n%a@\n" pp textual ;
      F.printf "Veryfing the transformed module...@\n" ;
      type_check textual
  | Ok (textual, errors) ->
      F.printf "verification succeeded - %d warnings@\n------@\n" (List.length errors) ;
      List.iter errors
        ~f:
          (F.printf "%a@\n"
             (TextualVerification.pp_error_with_sourcefile textual.Module.sourcefile) ) ;
      F.printf "------@\n%a@\n" pp textual ;
      F.printf "Veryfing the filtered module...@\n" ;
      type_check textual
  | Error errors ->
      F.printf "verification failed - %d errors@\n------@\n" (List.length errors) ;
      List.iter errors ~f:(F.printf "%a@\n" (TextualParser.pp_error sourcefile))


let show module_ = F.printf "%a" (Module.pp ~show_location:true) module_

let show_result = function
  | Ok module_ ->
      show module_
  | Error errs ->
      List.iter errs ~f:(fun e -> F.printf "%a@\n" (TextualParser.pp_error sourcefile) e)
