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
      F.fprintf fmt "SIL: Consistency Error: %a, %a: " SourceFile.pp sourcefile Location.pp loc ;
      TextualBasicVerification.pp_error fmt err
  | TypeError err ->
      let loc = TextualTypeVerification.error_loc err in
      F.fprintf fmt "Textual: Type Error: %a, %a: " SourceFile.pp sourcefile Location.pp loc ;
      TextualTypeVerification.pp_error fmt err
  | DeclaredTwiceError err ->
      F.fprintf fmt "SIL: Consistency Error: %a: " SourceFile.pp sourcefile ;
      TextualDecls.pp_error fmt err


let verify_common ~restore_ssa module_ =
  let errors, decls_env = TextualDecls.make_decls module_ in
  let errors = List.map errors ~f:(fun x -> DeclaredTwiceError x) in
  if List.is_empty errors then
    let errors =
      TextualBasicVerification.run module_ decls_env |> List.map ~f:(fun x -> BasicError x)
    in
    if List.is_empty errors then
      match TextualTypeVerification.run ~restore_ssa module_ decls_env with
      | Ok module_ ->
          Ok module_
      | Error (errors, module_) ->
          let errors = List.map ~f:(fun x -> TypeError x) errors in
          Error (errors, Some module_)
    else Error (errors, None)
  else Error (errors, None)


let verify_strict module_ = verify_common ~restore_ssa:false module_ |> Result.map_error ~f:fst

let verify_keep_going module_ =
  match verify_common ~restore_ssa:true module_ with
  | Ok module_ ->
      Ok (module_, [])
  | Error (errors, Some module_) ->
      Ok (module_, errors)
  | Error (errors, None) ->
      Error errors
