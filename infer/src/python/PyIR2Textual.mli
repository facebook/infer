(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

val mk_module : PyIR.Module.t -> Textual.Module.t

val add_pyir_type :
  PyIRTypeInference.t -> module_name:PyIR.Ident.t -> Textual.Module.t -> Textual.Module.t
