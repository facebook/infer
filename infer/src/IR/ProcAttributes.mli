(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Attributes of a procedure. *)

(** flags for a procedure *)
type proc_flags = (string, string) Caml.Hashtbl.t [@@deriving compare]

type clang_method_kind =
  | CPP_INSTANCE
  | OBJC_INSTANCE
  | CPP_CLASS
  | OBJC_CLASS
  | BLOCK
  | C_FUNCTION
  [@@deriving compare]

val equal_clang_method_kind : clang_method_kind -> clang_method_kind -> bool

val string_of_clang_method_kind : clang_method_kind -> string

type objc_accessor_type =
  | Objc_getter of Typ.Struct.field
  | Objc_setter of Typ.Struct.field
  [@@deriving compare]

val kind_of_objc_accessor_type : objc_accessor_type -> string

type var_attribute =
  | Modify_in_block
  (* __block attribute of Objective-C variables, means that it will be modified inside a block *)
  [@@deriving compare]

val var_attribute_equal : var_attribute -> var_attribute -> bool
(** Equality for var_attribute *)

type var_data = {name: Mangled.t; typ: Typ.t; attributes: var_attribute list} [@@deriving compare]

type t =
  { access: PredSymb.access  (** visibility access *)
  ; captured: (Mangled.t * Typ.t) list  (** name and type of variables captured in blocks *)
  ; mutable did_preanalysis: bool  (** true if we performed preanalysis on the CFG for this proc *)
  ; err_log: Errlog.t  (** Error log for the procedure *)
  ; exceptions: string list  (** exceptions thrown by the procedure *)
  ; formals: (Mangled.t * Typ.t) list  (** name and type of formal parameters *)
  ; const_formals: int list  (** list of indices of formals that are const-qualified *)
  ; by_vals: int list  (** list of indices of formals that are passed by-value *)
  ; func_attributes: PredSymb.func_attribute list
  ; is_abstract: bool  (** the procedure is abstract *)
  ; is_bridge_method: bool  (** the procedure is a bridge method *)
  ; is_defined: bool  (** true if the procedure is defined, and not just declared *)
  ; is_cpp_noexcept_method: bool  (** the procedure is an C++ method annotated with "noexcept" *)
  ; is_java_synchronized_method: bool  (** the procedure is a Java synchronized method *)
  ; is_model: bool  (** the procedure is a model *)
  ; is_specialized: bool  (** the procedure is a clone specialized for dynamic dispatch handling *)
  ; is_synthetic_method: bool  (** the procedure is a synthetic method *)
  ; clang_method_kind: clang_method_kind  (** the kind of method the procedure is *)
  ; loc: Location.t  (** location of this procedure in the source code *)
  ; translation_unit: SourceFile.t option  (** translation unit to which the procedure belongs *)
  ; mutable locals: var_data list  (** name, type and attributes of local variables *)
  ; method_annotation: Annot.Method.t  (** annotations for all methods *)
  ; objc_accessor: objc_accessor_type option  (** type of ObjC accessor, if any *)
  ; proc_flags: proc_flags  (** flags of the procedure *)
  ; proc_name: Typ.Procname.t  (** name of the procedure *)
  ; ret_type: Typ.t  (** return type *)
  ; source_file_captured: SourceFile.t  (** source file where the procedure was captured *) }
  [@@deriving compare]

val default : Typ.Procname.t -> t
(** Create a proc_attributes with default values. *)

val pp : Format.formatter -> t -> unit

module SQLite : SqliteUtils.Data with type t = t
