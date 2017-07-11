(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Attributes of a procedure. *)
open! IStd
module Hashtbl = Caml.Hashtbl
module L = Logging
module F = Format

(** flags for a procedure *)
type proc_flags = (string, string) Hashtbl.t

let compare_proc_flags x y =
  let bindings x = Hashtbl.fold (fun k d l -> (k, d) :: l) x [] in
  [%compare : (string * string) list] (bindings x) (bindings y)

let proc_flags_empty () : proc_flags = Hashtbl.create 1

let proc_flag_skip = "skip"

let proc_flag_ignore_return = "ignore_return"

let proc_flags_add proc_flags key value = Hashtbl.replace proc_flags key value

let proc_flags_find proc_flags key = Hashtbl.find proc_flags key

(** Type for ObjC accessors *)
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

let default proc_name language =
  { access= PredSymb.Default
  ; captured= []
  ; changed= true
  ; did_preanalysis= false
  ; err_log= Errlog.empty ()
  ; exceptions= []
  ; formals= []
  ; const_formals= []
  ; func_attributes= []
  ; is_abstract= false
  ; is_bridge_method= false
  ; is_cpp_instance_method= false
  ; is_cpp_noexcept_method= false
  ; is_java_synchronized_method= false
  ; is_defined= false
  ; is_objc_instance_method= false
  ; is_model= false
  ; is_synthetic_method= false
  ; language
  ; loc= Location.dummy
  ; translation_unit= None
  ; locals= []
  ; method_annotation= Annot.Method.empty
  ; objc_accessor= None
  ; proc_flags= proc_flags_empty ()
  ; proc_name
  ; ret_type= Typ.mk Typ.Tvoid
  ; source_file_captured= SourceFile.invalid __FILE__ }
