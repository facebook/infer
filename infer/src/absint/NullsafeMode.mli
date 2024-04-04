(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type t = Default | Local | Strict

val of_java_procname : Tenv.t -> Procname.Java.t -> t
