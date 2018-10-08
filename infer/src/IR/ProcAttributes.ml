(*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Attributes of a procedure. *)

open! IStd
module F = Format

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


type var_data = {name: Mangled.t; typ: Typ.t; modify_in_block: bool; is_constexpr: bool}
[@@deriving compare]

let pp_var_data fmt {name; typ; modify_in_block} =
  F.fprintf fmt "@[<h>{ name=@ %a;@ typ=@ %a;@ modify_in_block=@ %b@ }@]" Mangled.pp name
    (Typ.pp_full Pp.text) typ modify_in_block


type t =
  { access: PredSymb.access  (** visibility access *)
  ; captured: (Mangled.t * Typ.t) list  (** name and type of variables captured in blocks *)
  ; mutable did_preanalysis: bool  (** true if we performed preanalysis on the CFG for this proc *)
  ; exceptions: string list  (** exceptions thrown by the procedure *)
  ; formals: (Mangled.t * Typ.t) list  (** name and type of formal parameters *)
  ; const_formals: int list  (** list of indices of formals that are const-qualified *)
  ; func_attributes: PredSymb.func_attribute list
  ; is_abstract: bool  (** the procedure is abstract *)
  ; is_bridge_method: bool  (** the procedure is a bridge method *)
  ; is_defined: bool  (** true if the procedure is defined, and not just declared *)
  ; is_cpp_noexcept_method: bool  (** the procedure is an C++ method annotated with "noexcept" *)
  ; is_java_synchronized_method: bool  (** the procedure is a Java synchronized method *)
  ; is_model: bool  (** the procedure is a model *)
  ; is_specialized: bool  (** the procedure is a clone specialized for dynamic dispatch handling *)
  ; is_synthetic_method: bool  (** the procedure is a synthetic method *)
  ; is_variadic: bool  (** the procedure is variadic, only supported for Clang procedures *)
  ; clang_method_kind: ClangMethodKind.t  (** the kind of method the procedure is *)
  ; loc: Location.t  (** location of this procedure in the source code *)
  ; translation_unit: SourceFile.t  (** translation unit to which the procedure belongs *)
  ; mutable locals: var_data list  (** name, type and attributes of local variables *)
  ; method_annotation: Annot.Method.t  (** annotations for all methods *)
  ; objc_accessor: objc_accessor_type option  (** type of ObjC accessor, if any *)
  ; proc_name: Typ.Procname.t  (** name of the procedure *)
  ; ret_type: Typ.t  (** return type *)
  ; has_added_return_param: bool  (** whether or not a return param was added *) }

let default translation_unit proc_name =
  { access= PredSymb.Default
  ; captured= []
  ; did_preanalysis= false
  ; exceptions= []
  ; formals= []
  ; const_formals= []
  ; func_attributes= []
  ; is_abstract= false
  ; is_bridge_method= false
  ; is_cpp_noexcept_method= false
  ; is_java_synchronized_method= false
  ; is_defined= false
  ; is_model= false
  ; is_specialized= false
  ; is_synthetic_method= false
  ; is_variadic= false
  ; clang_method_kind= ClangMethodKind.C_FUNCTION
  ; loc= Location.dummy
  ; translation_unit
  ; locals= []
  ; has_added_return_param= false
  ; method_annotation= Annot.Method.empty
  ; objc_accessor= None
  ; proc_name
  ; ret_type= Typ.mk Typ.Tvoid }


let pp_parameters =
  Pp.semicolon_seq ~print_env:Pp.text_break (Pp.pair ~fst:Mangled.pp ~snd:(Typ.pp_full Pp.text))


let pp f
    ({ access
     ; captured
     ; did_preanalysis
     ; exceptions
     ; formals
     ; const_formals
     ; func_attributes
     ; is_abstract
     ; is_bridge_method
     ; is_defined
     ; is_cpp_noexcept_method
     ; is_java_synchronized_method
     ; is_model
     ; is_specialized
     ; is_synthetic_method
     ; is_variadic
     ; clang_method_kind
     ; loc
     ; translation_unit
     ; locals
     ; has_added_return_param
     ; method_annotation
     ; objc_accessor
     ; proc_name
     ; ret_type }[@warning "+9"]) =
  let default = default translation_unit proc_name in
  let pp_bool_default ~default title b f () =
    if not (Bool.equal default b) then F.fprintf f "; %s= %b@," title b
  in
  F.fprintf f "@[<v>{ proc_name= %a@,; translation_unit= %a@," Typ.Procname.pp proc_name
    SourceFile.pp translation_unit ;
  if not (PredSymb.equal_access default.access access) then
    F.fprintf f "; access= %a@," (Pp.to_string ~f:PredSymb.string_of_access) access ;
  if not ([%compare.equal: (Mangled.t * Typ.t) list] default.captured captured) then
    F.fprintf f "; captured= [@[%a@]]@," pp_parameters captured ;
  if not ([%compare.equal: string list] default.exceptions exceptions) then
    F.fprintf f "; exceptions= [@[%a@]]@,"
      (Pp.semicolon_seq ~print_env:Pp.text_break F.pp_print_string)
      exceptions ;
  (* always print formals *)
  F.fprintf f "; formals= [@[%a@]]@," pp_parameters formals ;
  if not ([%compare.equal: int list] default.const_formals const_formals) then
    F.fprintf f "; const_formals= [@[%a@]]@,"
      (Pp.semicolon_seq ~print_env:Pp.text_break F.pp_print_int)
      const_formals ;
  if not ([%compare.equal: PredSymb.func_attribute list] default.func_attributes func_attributes)
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
  pp_bool_default ~default:default.is_variadic "is_variadic" is_variadic f () ;
  if not (ClangMethodKind.equal default.clang_method_kind clang_method_kind) then
    F.fprintf f "; clang_method_kind= %a@,"
      (Pp.to_string ~f:ClangMethodKind.to_string)
      clang_method_kind ;
  if not (Location.equal default.loc loc) then F.fprintf f "; loc= %a@," Location.pp_file_pos loc ;
  F.fprintf f "; locals= [@[%a@]]@," (Pp.semicolon_seq ~print_env:Pp.text_break pp_var_data) locals ;
  pp_bool_default ~default:default.has_added_return_param "has_added_return_param"
    has_added_return_param f () ;
  if not (Annot.Method.is_empty method_annotation) then
    F.fprintf f "; method_annotation= %a@," (Annot.Method.pp "") method_annotation ;
  if not ([%compare.equal: objc_accessor_type option] default.objc_accessor objc_accessor) then
    F.fprintf f "; objc_accessor= %a@," (Pp.option pp_objc_accessor_type) objc_accessor ;
  (* always print ret type *)
  F.fprintf f "; ret_type= %a @," (Typ.pp_full Pp.text) ret_type ;
  F.fprintf f "; proc_id= %s }@]" (Typ.Procname.to_unique_id proc_name)


module SQLite = SqliteUtils.MarshalledData (struct
  type nonrec t = t
end)
