(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type matching =
  | JavaMatching of DoliJavaAst.extendedSignature list
  | ObjCMatching of DoliObjCAst.extendedSignature list

type doliRule = {ruleName: string; match_: matching; body: Textual.Body.t}

type doliProgram = DoliProgram of doliRule list

val return_type_to_textual : doliRule -> Textual.Typ.t

val param_types_to_textual : doliRule -> Textual.Typ.annotated list

val get_parameter_names : doliRule -> Textual.VarName.t list
