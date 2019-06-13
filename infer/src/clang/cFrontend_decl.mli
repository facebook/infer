(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Translate declarations **)

module CFrontend_decl_funct (T : CModule_type.CTranslation) : CModule_type.CFrontend
