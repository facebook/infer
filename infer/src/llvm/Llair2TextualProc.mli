(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module State = Llair2TextualState

val to_qualified_proc_name :
     ?loc:Textual.Location.t
  -> Textual.TypeName.t Textual.ProcName.Hashtbl.t
  -> string
  -> Textual.QualifiedProcName.t

val builtin_qual_proc_name : string -> Textual.QualifiedProcName.t
