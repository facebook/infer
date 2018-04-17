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

type clang_method_kind =
  | CPP_INSTANCE
  | OBJC_INSTANCE
  | CPP_CLASS
  | OBJC_CLASS
  | BLOCK
  | C_FUNCTION
  [@@deriving compare]

let equal_clang_method_kind = [%compare.equal : clang_method_kind]

let string_of_clang_method_kind = function
  | CPP_INSTANCE ->
      "CPP_INSTANCE"
  | OBJC_INSTANCE ->
      "OBJC_INSTANCE"
  | CPP_CLASS ->
      "CPP_CLASS"
  | OBJC_CLASS ->
      "OBJC_CLASS"
  | BLOCK ->
      "BLOCK"
  | C_FUNCTION ->
      "C_FUNCTION"


(** Type for ObjC accessors *)
type objc_accessor_type =
  | Objc_getter of Typ.Struct.field
  | Objc_setter of Typ.Struct.field
  [@@deriving compare]

let kind_of_objc_accessor_type accessor =
  match accessor with Objc_getter _ -> "getter" | Objc_setter _ -> "setter"


let pp_objc_accessor_type fmt objc_accessor_type =
  let fieldname, typ, annots =
    match objc_accessor_type with Objc_getter field | Objc_setter field -> field
  in
  F.fprintf fmt "%s<%a:%a@,[%a]>"
    (kind_of_objc_accessor_type objc_accessor_type)
    Typ.Fieldname.pp fieldname (Typ.pp Pp.text) typ
    (Pp.semicolon_seq (Pp.pair ~fst:Annot.pp ~snd:F.pp_print_bool))
    annots


type var_attribute = Modify_in_block [@@deriving compare]

let string_of_var_attribute = function Modify_in_block -> "<Modify_in_block>"

let var_attribute_equal = [%compare.equal : var_attribute]

type var_data = {name: Mangled.t; typ: Typ.t; attributes: var_attribute list} [@@deriving compare]

let pp_var_data fmt {name; typ; attributes} =
  F.fprintf fmt "{@[<h>name=@ %a;@,typ=@ %a;@,attributes=@ %a@]}" Mangled.pp name (Typ.pp Pp.text)
    typ
    (Pp.semicolon_seq (Pp.to_string ~f:string_of_var_attribute))
    attributes


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

let default proc_name =
  { access= PredSymb.Default
  ; captured= []
  ; did_preanalysis= false
  ; err_log= Errlog.empty ()
  ; exceptions= []
  ; formals= []
  ; const_formals= []
  ; by_vals= []
  ; func_attributes= []
  ; is_abstract= false
  ; is_bridge_method= false
  ; is_cpp_noexcept_method= false
  ; is_java_synchronized_method= false
  ; is_defined= false
  ; is_model= false
  ; is_specialized= false
  ; is_synthetic_method= false
  ; clang_method_kind= C_FUNCTION
  ; loc= Location.dummy
  ; translation_unit= None
  ; locals= []
  ; method_annotation= Annot.Method.empty
  ; objc_accessor= None
  ; proc_flags= proc_flags_empty ()
  ; proc_name
  ; ret_type= Typ.mk Typ.Tvoid
  ; source_file_captured= SourceFile.invalid __FILE__ }


let pp_parameters = Pp.semicolon_seq (Pp.pair ~fst:Mangled.pp ~snd:(Typ.pp Pp.text))

let pp fmt attributes =
  let[@warning "+9"] { access
                     ; captured
                     ; did_preanalysis
                     ; err_log
                     ; exceptions
                     ; formals
                     ; const_formals
                     ; by_vals
                     ; func_attributes
                     ; is_abstract
                     ; is_bridge_method
                     ; is_defined
                     ; is_cpp_noexcept_method
                     ; is_java_synchronized_method
                     ; is_model
                     ; is_specialized
                     ; is_synthetic_method
                     ; clang_method_kind
                     ; loc
                     ; translation_unit
                     ; locals
                     ; method_annotation
                     ; objc_accessor
                     ; proc_flags
                     ; proc_name
                     ; ret_type
                     ; source_file_captured } =
    attributes
  in
  Format.fprintf fmt
    "{@[access= %a;@ captured= [@[%a@]];@ did_preanalysis= %b;@ err_log= [@[%a%a@]];@ exceptions= \
     [@[%a@]];@ formals= [@[%a@]];@ const_formals= [@[%a@]];@ by_vals= [@[%a@]];@ \
     func_attributes= [@[%a@]];@ is_abstract= %b;@ is_bridge_method= %b;@ is_defined= %b;@ \
     is_cpp_noexcept_method= %b;@ is_java_synchronized_method= %b;@ is_model= %b;@ \
     is_specialized= %b;@ is_synthetic_method= %b;@ clang_method_kind= %a;@ loc= %a;@ \
     translation_unit= %a;@ locals= [@[%a@]];@ method_annotation= %a;@ objc_accessor= %a;@ \
     proc_flags= [@[%a@]];@ proc_name= %a;@ ret_type= %a;@ source_file_captured=%a@]}"
    (Pp.to_string ~f:PredSymb.string_of_access)
    access pp_parameters captured did_preanalysis Errlog.pp_errors err_log Errlog.pp_warnings
    err_log
    (Pp.semicolon_seq F.pp_print_string)
    exceptions pp_parameters formals (Pp.semicolon_seq F.pp_print_int) const_formals
    (Pp.semicolon_seq F.pp_print_int) by_vals
    (Pp.semicolon_seq PredSymb.pp_func_attribute)
    func_attributes is_abstract is_bridge_method is_defined is_cpp_noexcept_method
    is_java_synchronized_method is_model is_specialized is_synthetic_method
    (Pp.to_string ~f:string_of_clang_method_kind)
    clang_method_kind Location.pp loc (Pp.option SourceFile.pp) translation_unit
    (Pp.semicolon_seq pp_var_data) locals (Annot.Method.pp "") method_annotation
    (Pp.option pp_objc_accessor_type) objc_accessor
    (Pp.hashtbl ~key:F.pp_print_string ~value:F.pp_print_string)
    proc_flags Typ.Procname.pp proc_name (Typ.pp Pp.text) ret_type SourceFile.pp
    source_file_captured


module SQLite = SqliteUtils.MarshalledData (struct
  type nonrec t = t
end)
