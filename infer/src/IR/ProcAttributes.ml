(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Attributes of a procedure. *)

open! IStd
module F = Format

(** Type for ObjC accessors *)
type objc_accessor_type = Objc_getter of Struct.field | Objc_setter of Struct.field
[@@deriving compare]

let kind_of_objc_accessor_type accessor =
  match accessor with Objc_getter _ -> "getter" | Objc_setter _ -> "setter"


let pp_objc_accessor_type fmt objc_accessor_type =
  let fieldname, typ, annots =
    match objc_accessor_type with Objc_getter field | Objc_setter field -> field
  in
  F.fprintf fmt "%s<%a:%a@,[%a]>"
    (kind_of_objc_accessor_type objc_accessor_type)
    Fieldname.pp fieldname (Typ.pp Pp.text) typ
    (Pp.semicolon_seq ~print_env:Pp.text_break (Pp.pair ~fst:Annot.pp ~snd:F.pp_print_bool))
    annots


type var_data =
  {name: Mangled.t; typ: Typ.t; modify_in_block: bool; is_constexpr: bool; is_declared_unused: bool}
[@@deriving compare]

let pp_var_data fmt {name; typ; modify_in_block; is_declared_unused} =
  F.fprintf fmt "@[<h>{ name=@ %a;@ typ=@ %a;@ modify_in_block=@ %b;@ is_declared_unused=@ %b@ }@]"
    Mangled.pp name (Typ.pp_full Pp.text) typ modify_in_block is_declared_unused


type specialized_with_blocks_info =
  { orig_proc: Procname.t
  ; formals_to_procs_and_new_formals: (Procname.t * (Mangled.t * Typ.t) list) Mangled.Map.t }
[@@deriving compare]

type t =
  { access: PredSymb.access  (** visibility access *)
  ; captured: CapturedVar.t list  (** name and type of variables captured in blocks *)
  ; exceptions: string list  (** exceptions thrown by the procedure *)
  ; formals: (Mangled.t * Typ.t) list  (** name and type of formal parameters *)
  ; const_formals: int list  (** list of indices of formals that are const-qualified *)
  ; is_abstract: bool  (** the procedure is abstract *)
  ; is_biabduction_model: bool  (** the procedure is a model for the biabduction analysis *)
  ; is_bridge_method: bool  (** the procedure is a bridge method *)
  ; is_defined: bool  (** true if the procedure is defined, and not just declared *)
  ; is_java_synchronized_method: bool  (** the procedure is a Java synchronized method *)
  ; passed_as_noescape_block_to: Procname.t option
        (** Present if the procedure is an Objective-C block that has been passed to the given
            method in a position annotated with the NS_NOESCAPE attribute. *)
  ; is_no_return: bool  (** the procedure is known not to return *)
  ; is_objc_arc_on: bool  (** the ObjC procedure is compiled with ARC *)
  ; is_specialized: bool  (** the procedure is a clone specialized for dynamic dispatch handling *)
  ; is_synthetic_method: bool  (** the procedure is a synthetic method *)
  ; is_variadic: bool  (** the procedure is variadic, only supported for Clang procedures *)
  ; sentinel_attr: (int * int) option  (** __attribute__((sentinel(int, int))) *)
  ; specialized_with_blocks_info: specialized_with_blocks_info option
        (** the procedure is a clone specialized with calls to concrete closures, with link to the
            original procedure, and a map that links the original formals to the elements of the
            closure used to specialize the procedure. *)
  ; clang_method_kind: ClangMethodKind.t  (** the kind of method the procedure is *)
  ; loc: Location.t  (** location of this procedure in the source code *)
  ; translation_unit: SourceFile.t  (** translation unit to which the procedure belongs *)
  ; mutable locals: var_data list  (** name, type and attributes of local variables *)
  ; method_annotation: Annot.Method.t  (** annotations for all methods *)
  ; objc_accessor: objc_accessor_type option  (** type of ObjC accessor, if any *)
  ; proc_name: Procname.t  (** name of the procedure *)
  ; ret_type: Typ.t  (** return type *)
  ; has_added_return_param: bool  (** whether or not a return param was added *) }

let get_annotated_formals {method_annotation= {params}; formals} =
  let rec zip_params ial parl =
    match (ial, parl) with
    | ia :: ial', param :: parl' ->
        (param, ia) :: zip_params ial' parl'
    | [], param :: parl' ->
        (* List of annotations exhausted before the list of params -
           treat lack of annotation info as an empty annotation *)
        (param, Annot.Item.empty) :: zip_params [] parl'
    | [], [] ->
        []
    | _ :: _, [] ->
        (* List of params exhausted before the list of annotations -
           this should never happen *)
        assert false
  in
  (* zip formal params with annotation *)
  List.rev (zip_params (List.rev params) (List.rev formals))


let get_access attributes = attributes.access

let get_formals attributes = attributes.formals

let get_pvar_formals attributes =
  let pname = attributes.proc_name in
  List.map attributes.formals ~f:(fun (name, typ) -> (Pvar.mk name pname, typ))


let get_proc_name attributes = attributes.proc_name

let get_loc attributes = attributes.loc

let default translation_unit proc_name =
  { access= PredSymb.Default
  ; captured= []
  ; exceptions= []
  ; formals= []
  ; const_formals= []
  ; is_abstract= false
  ; is_biabduction_model= false
  ; is_bridge_method= false
  ; is_defined= false
  ; is_java_synchronized_method= false
  ; passed_as_noescape_block_to= None
  ; is_no_return= false
  ; is_objc_arc_on= false
  ; is_specialized= false
  ; specialized_with_blocks_info= None
  ; is_synthetic_method= false
  ; is_variadic= false
  ; sentinel_attr= None
  ; clang_method_kind= ClangMethodKind.C_FUNCTION
  ; loc= Location.dummy
  ; translation_unit
  ; locals= []
  ; has_added_return_param= false
  ; method_annotation= Annot.Method.empty
  ; objc_accessor= None
  ; proc_name
  ; ret_type= StdTyp.void }


let pp_parameters =
  Pp.semicolon_seq ~print_env:Pp.text_break (Pp.pair ~fst:Mangled.pp ~snd:(Typ.pp_full Pp.text))


let pp_specialized_with_blocks_info fmt info =
  let pp_new_formal fmt el =
    F.fprintf fmt "%a:%a" Mangled.pp (fst el) (Typ.pp_full Pp.text) (snd el)
  in
  let pp_new_formals = Pp.semicolon_seq ~print_env:Pp.text_break pp_new_formal in
  F.fprintf fmt "orig_procname=%a, formals_to_procs_and_new_formals=%a" Procname.pp info.orig_proc
    (Mangled.Map.pp ~pp_value:(Pp.pair ~fst:Procname.pp ~snd:pp_new_formals))
    info.formals_to_procs_and_new_formals


let pp_captured = Pp.semicolon_seq ~print_env:Pp.text_break CapturedVar.pp

let pp f
    ({ access
     ; captured
     ; exceptions
     ; formals
     ; const_formals
     ; is_abstract
     ; is_biabduction_model
     ; is_bridge_method
     ; is_defined
     ; is_java_synchronized_method
     ; passed_as_noescape_block_to
     ; is_no_return
     ; is_objc_arc_on
     ; is_specialized
     ; specialized_with_blocks_info
     ; is_synthetic_method
     ; is_variadic
     ; sentinel_attr
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
  F.fprintf f "@[<v>{ proc_name= %a@,; translation_unit= %a@," Procname.pp proc_name SourceFile.pp
    translation_unit ;
  if not (PredSymb.equal_access default.access access) then
    F.fprintf f "; access= %a@," (Pp.of_string ~f:PredSymb.string_of_access) access ;
  if not ([%compare.equal: CapturedVar.t list] default.captured captured) then
    F.fprintf f "; captured= [@[%a@]]@," pp_captured captured ;
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
  pp_bool_default ~default:default.is_abstract "is_abstract" is_abstract f () ;
  pp_bool_default ~default:default.is_biabduction_model "is_model" is_biabduction_model f () ;
  pp_bool_default ~default:default.is_bridge_method "is_bridge_method" is_bridge_method f () ;
  pp_bool_default ~default:default.is_defined "is_defined" is_defined f () ;
  pp_bool_default ~default:default.is_java_synchronized_method "is_java_synchronized_method"
    is_java_synchronized_method f () ;
  if
    not
      ([%compare.equal: Procname.t option] default.passed_as_noescape_block_to
         passed_as_noescape_block_to)
  then
    F.fprintf f "; passed_as_noescape_block_to %a" (Pp.option Procname.pp)
      passed_as_noescape_block_to ;
  pp_bool_default ~default:default.is_no_return "is_no_return" is_no_return f () ;
  pp_bool_default ~default:default.is_objc_arc_on "is_objc_arc_on" is_objc_arc_on f () ;
  pp_bool_default ~default:default.is_specialized "is_specialized" is_specialized f () ;
  if
    not
      ([%compare.equal: specialized_with_blocks_info option] default.specialized_with_blocks_info
         specialized_with_blocks_info)
  then
    F.fprintf f "; specialized_with_blocks_info %a@,"
      (Pp.option pp_specialized_with_blocks_info)
      specialized_with_blocks_info ;
  pp_bool_default ~default:default.is_synthetic_method "is_synthetic_method" is_synthetic_method f
    () ;
  pp_bool_default ~default:default.is_variadic "is_variadic" is_variadic f () ;
  if not ([%compare.equal: (int * int) option] default.sentinel_attr sentinel_attr) then
    F.fprintf f "; sentinel_attr= %a@,"
      (Pp.option (Pp.pair ~fst:F.pp_print_int ~snd:F.pp_print_int))
      sentinel_attr ;
  if not (ClangMethodKind.equal default.clang_method_kind clang_method_kind) then
    F.fprintf f "; clang_method_kind= %a@,"
      (Pp.of_string ~f:ClangMethodKind.to_string)
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
  F.fprintf f "; proc_id= %a }@]" Procname.pp_unique_id proc_name


module SQLite = SqliteUtils.MarshalledDataNOTForComparison (struct
  type nonrec t = t
end)
