(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

type matching =
  | JavaMatching of DoliJavaAst.extendedSignature list
  | ObjCMatching of DoliObjCAst.extendedSignature list

type doliRule = {ruleName: string; match_: matching; body: Textual.Body.t}

type doliProgram = DoliProgram of doliRule list

let return_type_to_textual (doliRule : doliRule) : Textual.Typ.t =
  match doliRule.match_ with
  | JavaMatching extSigns ->
      DoliJavaAst.return_type_to_textual extSigns
  | _ ->
      L.die InternalError "ObjectiveC doli rules not being handled yet"


let param_types_to_textual (doliRule : doliRule) : Textual.Typ.annotated list =
  match doliRule.match_ with
  | JavaMatching extSigns ->
      DoliJavaAst.param_types_to_textual extSigns
  | _ ->
      L.die InternalError "ObjC doli rules not being handled yet"


let get_parameter_names (doliRule : doliRule) : Textual.VarName.t list =
  match doliRule.match_ with
  | JavaMatching extSigns ->
      DoliJavaAst.get_parameter_names extSigns
  | _ ->
      L.die InternalError "ObjC doli rules not being handled yet"
