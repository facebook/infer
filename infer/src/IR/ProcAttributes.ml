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
type objc_accessor_type = Objc_getter of Typ.Struct.field | Objc_setter of Typ.Struct.field
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
    (Pp.semicolon_seq ~print_env:Pp.text_break (Pp.pair ~fst:Annot.pp ~snd:F.pp_print_bool))
    annots


type var_attribute = Modify_in_block [@@deriving compare]

let string_of_var_attribute = function Modify_in_block -> "<Modify_in_block>"

let var_attribute_equal = [%compare.equal : var_attribute]

type var_data = {name: Mangled.t; typ: Typ.t; attributes: var_attribute list} [@@deriving compare]

let pp_var_data fmt {name; typ; attributes} =
  F.fprintf fmt "@[<h>{ name=@ %a;@ typ=@ %a" Mangled.pp name (Typ.pp_full Pp.text) typ ;
  if not (List.is_empty attributes) then
    F.fprintf fmt ";@ attributes=@ [@[%a@]]"
      (Pp.semicolon_seq ~print_env:Pp.text_break (Pp.to_string ~f:string_of_var_attribute))
      attributes ;
  F.fprintf fmt " }@]"


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


let pp_parameters =
  Pp.semicolon_seq ~print_env:Pp.text_break (Pp.pair ~fst:Mangled.pp ~snd:(Typ.pp_full Pp.text))


let pp f
    ({ access
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
     ; source_file_captured }[@warning "+9"]) =
  let default = default proc_name in
  let pp_bool_default ~default title b f () =
    if not (Bool.equal default b) then F.fprintf f "; %s= %b@," title b
  in
  F.fprintf f "@[<v>{ proc_name= %a@,; source_file_captured= %a@," Typ.Procname.pp proc_name
    SourceFile.pp source_file_captured ;
  if not (PredSymb.equal_access default.access access) then
    F.fprintf f "; access= %a@," (Pp.to_string ~f:PredSymb.string_of_access) access ;
  if not ([%compare.equal : (Mangled.t * Typ.t) list] default.captured captured) then
    F.fprintf f "; captured= [@[%a@]]@," pp_parameters captured ;
  pp_bool_default ~default:default.did_preanalysis "did_preanalysis" did_preanalysis f () ;
  if not (Errlog.equal default.err_log err_log) then
    F.fprintf f "; err_log= [@[%a%a@]]@," Errlog.pp_errors err_log Errlog.pp_warnings err_log ;
  if not ([%compare.equal : string list] default.exceptions exceptions) then
    F.fprintf f "; exceptions= [@[%a@]]@,"
      (Pp.semicolon_seq ~print_env:Pp.text_break F.pp_print_string)
      exceptions ;
  (* always print formals *)
  F.fprintf f "; formals= [@[%a@]]@," pp_parameters formals ;
  if not ([%compare.equal : int list] default.const_formals const_formals) then
    F.fprintf f "; const_formals= [@[%a@]]@,"
      (Pp.semicolon_seq ~print_env:Pp.text_break F.pp_print_int)
      const_formals ;
  if not ([%compare.equal : int list] default.by_vals by_vals) then
    F.fprintf f "; by_vals= [@[%a@]]@,"
      (Pp.semicolon_seq ~print_env:Pp.text_break F.pp_print_int)
      by_vals ;
  if not ([%compare.equal : PredSymb.func_attribute list] default.func_attributes func_attributes)
  then
    F.fprintf f "; func_attributes= [@[%a@]]@,"
      (Pp.semicolon_seq ~print_env:Pp.text_break PredSymb.pp_func_attribute)
      func_attributes ;
  pp_bool_default ~default:default.did_preanalysis "did_preanalysis" did_preanalysis f () ;
  pp_bool_default ~default:default.is_abstract "is_abstract" is_abstract f () ;
  pp_bool_default ~default:default.is_bridge_method "is_bridge_method" is_bridge_method f () ;
  pp_bool_default ~default:default.is_defined "is_defined" is_defined f () ;
  pp_bool_default ~default:default.is_cpp_noexcept_method "is_cpp_noexcept_method"
    is_cpp_noexcept_method f () ;
  pp_bool_default ~default:default.is_java_synchronized_method "is_java_synchronized_method"
    is_java_synchronized_method f () ;
  pp_bool_default ~default:default.is_model "is_model" is_model f () ;
  pp_bool_default ~default:default.is_specialized "is_specialized" is_specialized f () ;
  pp_bool_default ~default:default.is_synthetic_method "is_synthetic_method" is_synthetic_method f
    () ;
  if not ([%compare.equal : clang_method_kind] default.clang_method_kind clang_method_kind) then
    F.fprintf f "; clang_method_kind= %a@,"
      (Pp.to_string ~f:string_of_clang_method_kind)
      clang_method_kind ;
  if not (Location.equal default.loc loc) then F.fprintf f "; loc= %a@," Location.pp loc ;
  if not ([%compare.equal : SourceFile.t option] default.translation_unit translation_unit) then
    F.fprintf f "; translation_unit= %a@," (Pp.option SourceFile.pp) translation_unit ;
  if not ([%compare.equal : var_data list] default.locals locals) then
    F.fprintf f "; locals= [@[%a@]]@,"
      (Pp.semicolon_seq ~print_env:Pp.text_break pp_var_data)
      locals ;
  if not (Annot.Method.equal default.method_annotation method_annotation) then
    F.fprintf f "; method_annotation= %a@," (Annot.Method.pp "") method_annotation ;
  if not ([%compare.equal : objc_accessor_type option] default.objc_accessor objc_accessor) then
    F.fprintf f "; objc_accessor= %a@," (Pp.option pp_objc_accessor_type) objc_accessor ;
  if
    (* HACK: this hardcodes the default instead of comparing to [default.proc_flags], and tests
       emptiness in linear time too *)
    not (Int.equal (Hashtbl.length proc_flags) 0)
  then
    F.fprintf f "; proc_flags= [@[%a@]]@,"
      (Pp.hashtbl ~key:F.pp_print_string ~value:F.pp_print_string)
      proc_flags ;
  (* always print ret type *)
  F.fprintf f "; ret_type= %a }@]" (Typ.pp_full Pp.text) ret_type


module SQLite = SqliteUtils.MarshalledData (struct
  type nonrec t = t
end)
