(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseModelsImport

val dispatch :
  Tenv.t -> Procname.t -> ValueOrigin.t ProcnameDispatcher.Call.FuncArg.t list -> model option

val dispatch_builtins :
     Procname.t
  -> ValueOrigin.t ProcnameDispatcher.Call.FuncArg.t list
  -> (unit -> unit PulseModelsDSL.model_monad) option
