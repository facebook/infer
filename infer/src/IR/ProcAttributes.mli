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

val proc_flag_skip : string
(** keys for proc_flags *)

val proc_flag_ignore_return : string
(** key to specify that a function should be treated as a skip function *)

(** key to specify that it is OK to ignore the return value *)

val proc_flags_empty : unit -> proc_flags
(** empty proc flags *)

val proc_flags_add : proc_flags -> string -> string -> unit
(** add a key value pair to a proc flags *)

val proc_flags_find : proc_flags -> string -> string
(** find a value for a key in the proc flags *)

type objc_accessor_type =
  | Objc_getter of Typ.Fieldname.t
  | Objc_setter of Typ.Fieldname.t
  [@@deriving compare]

type t =
  { access: PredSymb.access  (** visibility access *)
  ; captured: (Mangled.t * Typ.t) list  (** name and type of variables captured in blocks *)
  ; mutable changed: bool  (** true if proc has changed since last analysis *)
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
  ; is_objc_instance_method: bool  (** the procedure is an objective-C instance method *)
  ; is_cpp_instance_method: bool  (** the procedure is an C++ instance method *)
  ; is_cpp_noexcept_method: bool  (** the procedure is an C++ method annotated with "noexcept" *)
  ; is_java_synchronized_method: bool  (** the procedure is a Java synchronized method *)
  ; is_model: bool  (** the procedure is a model *)
  ; is_synthetic_method: bool  (** the procedure is a synthetic method *)
  ; language: Config.language  (** language of the procedure *)
  ; loc: Location.t  (** location of this procedure in the source code *)
  ; translation_unit: SourceFile.t option  (** translation unit to which the procedure belongs *)
  ; mutable locals: (Mangled.t * Typ.t) list  (** name and type of local variables *)
  ; method_annotation: Annot.Method.t  (** annotations for java methods *)
  ; objc_accessor: objc_accessor_type option  (** type of ObjC accessor, if any *)
  ; proc_flags: proc_flags  (** flags of the procedure *)
  ; proc_name: Typ.Procname.t  (** name of the procedure *)
  ; ret_type: Typ.t  (** return type *)
  ; source_file_captured: SourceFile.t  (** source file where the procedure was captured *) }
  [@@deriving compare]

val default : Typ.Procname.t -> Config.language -> t
(** Create a proc_attributes with default values. *)
