(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type error =
  | BasicError of TextualBasicVerification.error
  | TypeError of TextualTypeVerification.error
  | DeclaredTwiceError of TextualDecls.error

let pp_error fmt err =
  match err with
  | BasicError err ->
      TextualBasicVerification.pp_error fmt err
  | TypeError err ->
      TextualTypeVerification.pp_error fmt err
  | DeclaredTwiceError err ->
      TextualDecls.pp_error fmt err


let pp_error_with_sourcefile sourcefile fmt err =
  let open Textual in
  match err with
  | BasicError err ->
      let loc = TextualBasicVerification.error_loc err in
      F.fprintf fmt "%a, %a: SIL consistency error: " SourceFile.pp sourcefile Location.pp loc ;
      TextualBasicVerification.pp_error fmt err
  | TypeError err ->
      let loc = TextualTypeVerification.error_loc err in
      F.fprintf fmt "%a, %a: textual type error: " SourceFile.pp sourcefile Location.pp loc ;
      TextualTypeVerification.pp_error fmt err
  | DeclaredTwiceError err ->
      F.fprintf fmt "%a: SIL consistency error: " SourceFile.pp sourcefile ;
      TextualDecls.pp_error fmt err


let verify module_ =
  let errors, decls_env = TextualDecls.make_decls module_ in
  let errors = List.map errors ~f:(fun x -> DeclaredTwiceError x) in
  if List.is_empty errors then
    let errors =
      TextualBasicVerification.run module_ decls_env |> List.map ~f:(fun x -> BasicError x)
    in
    if List.is_empty errors then
      match TextualTypeVerification.run module_ decls_env with
      | Ok module_ ->
          Ok module_
      | Error errors ->
          let errors = List.map ~f:(fun x -> TypeError x) errors in
          Error errors
    else Error errors
  else Error errors
