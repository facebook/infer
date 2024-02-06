(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t

val pp : Format.formatter -> t -> unit [@@warning "-unused-value-declaration"]

module ProcEntry : sig
  type t = Decl of Textual.ProcDecl.t | Desc of Textual.ProcDesc.t
end

val init : Textual.SourceFile.t -> Textual.Lang.t option -> t

val declare_global : t -> Textual.Global.t -> unit

val declare_proc : t -> ProcEntry.t -> unit

val declare_struct : t -> Textual.Struct.t -> unit

val fold_globals : t -> init:'a -> f:('a -> Textual.VarName.t -> Textual.Global.t -> 'a) -> 'a

val fold_procdecls : t -> init:'a -> f:('a -> Textual.ProcDecl.t -> 'a) -> 'a

val fold_structs : t -> init:'a -> f:('a -> Textual.TypeName.t -> Textual.Struct.t -> 'a) -> 'a

val get_fielddecl : t -> Textual.qualified_fieldname -> Textual.FieldDecl.t option

val get_global : t -> Textual.VarName.t -> Textual.Global.t option

type variadic_status =
  | NotVariadic
  | Variadic of Textual.Typ.t (* type of the variadic parameter *)

type generics_status = Reified | NotReified

val get_procdecl :
  t -> Textual.ProcSig.t -> int -> (variadic_status * generics_status * Textual.ProcDecl.t) option

val get_procdesc : t -> Textual.ProcSig.t -> Textual.ProcDesc.t option

val get_proc_entries_by_enclosing_class :
  t -> ProcEntry.t list Textual.TypeName.Map.t * Textual.TypeName.Set.t
(** returns 1) in a map, all function implementation and declarations, indexed by the name of their
    enclosing class 2) the set of all enclosing class that were not introduced by a type declaration *)

val get_struct : t -> Textual.TypeName.t -> Textual.Struct.t option

val is_field_declared : t -> Textual.qualified_fieldname -> bool

val is_defined_in_a_trait : t -> Textual.QualifiedProcName.t -> bool

val is_trait_method : t -> Textual.ProcSig.t -> bool

val source_file : t -> Textual.SourceFile.t

val lang : t -> Textual.Lang.t option

val get_undefined_types : t -> Textual.TypeName.t Seq.t

type error

val pp_error : Textual.SourceFile.t -> Format.formatter -> error -> unit

val make_decls : Textual.Module.t -> error list * t
