(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type doliBody = DoliBodyStub (* a stub at the moment; Textual to appear here *)

type matching =
  | JavaMatching of DoliJavaAst.extendedSignature list
  | ObjCMatching of DoliObjCAst.extendedSignature list

type doliInstruction = {match_: matching; body: doliBody}

type doliProgram = DoliProgram of doliInstruction list
