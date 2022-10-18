(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t

val init : SourceFile.t -> t

val declare_global : t -> Textual.Global.t -> unit

val declare_proc : t -> Textual.ProcDecl.t -> unit

val declare_struct : t -> Textual.Struct.t -> unit

val fold_globals : t -> init:'a -> f:('a -> Textual.VarName.t -> Textual.Global.t -> 'a) -> 'a

val fold_procnames : t -> init:'a -> f:('a -> Textual.ProcDecl.t -> 'a) -> 'a

val fold_structs : t -> init:'a -> f:('a -> Textual.TypeName.t -> Textual.Struct.t -> 'a) -> 'a

val get_fielddecl : t -> Textual.qualified_fieldname -> Textual.FieldDecl.t option

val get_global : t -> Textual.VarName.t -> Textual.Global.t option

val get_procname : t -> Textual.qualified_procname -> Textual.ProcDecl.t option

val get_struct : t -> Textual.TypeName.t -> Textual.Struct.t option

val is_field_declared : t -> Textual.qualified_fieldname -> bool

val is_procname_declared : t -> Textual.qualified_procname -> bool

val source_file : t -> SourceFile.t

val make_decls : Textual.Module.t -> t
