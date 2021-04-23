(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val struct_copy :
  Tenv.t -> Location.t -> Exp.t -> Exp.t -> typ:Typ.t -> struct_name:Typ.name -> Sil.instr list
