(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging
module F = Format

let capture prog (args : string list) =
  if not (String.equal prog "rustc") then
    L.die UserError "rustc should be explicitly used instead of %s." prog ;
  let json_file = List.hd_exn args in
  let json = Yojson.Basic.from_file json_file in
  let textual =
    match Charon.UllbcOfJson.crate_of_json json with
    | Ok crate ->
        RustFrontend.RustMir2Textual.mk_module crate json_file
    | Error err ->
        L.die UserError "%s: %s" err (Yojson.Basic.to_string json)
  in
  let sourcefile = Textual.SourceFile.create json_file in
  let verified_textual =
    match TextualVerification.verify_strict textual with
    | Ok vt ->
        vt
    | Error err ->
        L.die UserError "Textual verification failed:%a" (F.pp_print_list TextualVerification.pp_error) err
  in
  let transformed_textual, decls = TextualTransform.run Rust verified_textual in
  let cfg, tenv =
    match TextualSil.module_to_sil Rust transformed_textual decls with
    | Ok s ->
        s
    | Error err ->
        L.die UserError "Module to sil failed: %a" (F.pp_print_list (Textual.pp_transform_error sourcefile)) err
  in
  let sil = {TextualParser.TextualFile.sourcefile; cfg; tenv} in
  TextualParser.TextualFile.capture ~use_global_tenv:true sil ;
  Tenv.Global.store ~normalize:false sil.tenv
