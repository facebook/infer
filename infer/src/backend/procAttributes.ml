(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Attributes of a procedure. *)

module L = Logging
module F = Format
open Utils

(** Type for ObjC accessors *)
type objc_accessor_type =
  | Objc_getter of Ident.fieldname
  | Objc_setter of Ident.fieldname

type t =
  {
    access : Sil.access; (** visibility access *)
    captured : (Mangled.t * Sil.typ) list; (** name and type of variables captured in blocks *)
    err_log: Errlog.t; (** Error log for the procedure *)
    exceptions : string list; (** exceptions thrown by the procedure *)
    formals : (Mangled.t * Sil.typ) list; (** name and type of formal parameters *)
    func_attributes : Sil.func_attribute list;
    is_abstract : bool; (** the procedure is abstract *)
    mutable is_bridge_method : bool; (** the procedure is a bridge method *)
    is_defined : bool; (** true if the procedure is defined, and not just declared *)
    is_objc_instance_method : bool; (** the procedure is an objective-C instance method *)
    is_cpp_instance_method : bool; (** the procedure is an C++ instance method *)
    objc_accessor : objc_accessor_type option; (** the proc is ObjC accessor *)
    mutable is_synthetic_method : bool; (** the procedure is a synthetic method *)
    language : Config.language; (** language of the procedure *)
    loc : Location.t; (** location of this procedure in the source code *)
    mutable locals : (Mangled.t * Sil.typ) list; (** name and type of local variables *)
    method_annotation : Sil.method_annotation; (** annotations for java methods *)
    proc_flags : proc_flags; (** flags of the procedure *)
    proc_name : Procname.t; (** name of the procedure *)
    ret_type : Sil.typ; (** return type *)
  }

let copy pa =
  {
    access = pa.access;
    captured = pa.captured;
    err_log = pa.err_log;
    exceptions = pa.exceptions;
    formals = pa.formals;
    func_attributes = pa.func_attributes;
    is_abstract = pa.is_abstract;
    is_bridge_method = pa.is_bridge_method;
    is_defined = pa.is_defined;
    is_objc_instance_method = pa.is_objc_instance_method;
    is_cpp_instance_method = pa.is_cpp_instance_method;
    objc_accessor = pa.objc_accessor;
    is_synthetic_method = pa.is_synthetic_method;
    language = pa.language;
    loc = pa.loc;
    locals = pa.locals;
    method_annotation = pa.method_annotation;
    proc_flags = pa.proc_flags;
    proc_name = pa.proc_name;
    ret_type = pa.ret_type;
  }

let default proc_name language = {
  access = Sil.Default;
  formals = [];
  captured = [];
  err_log = Errlog.empty ();
  exceptions = [];
  func_attributes = [];
  is_abstract = false;
  is_defined = false;
  is_bridge_method = false;
  is_objc_instance_method = false;
  is_cpp_instance_method = false;
  objc_accessor = None;
  is_synthetic_method = false;
  language;
  loc = Location.dummy;
  locals = [];
  method_annotation = Sil.method_annotation_empty;
  proc_flags = proc_flags_empty ();
  proc_name;
  ret_type = Sil.Tvoid;
}
