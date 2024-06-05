(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Attributes of a procedure. *)

open! IStd
module F = Format

(** Visibility modifiers. *)
type access = Default | Public | Private | Protected [@@deriving compare, equal]

let string_of_access = function
  | Default ->
      "Default"
  | Public ->
      "Public"
  | Private ->
      "Private"
  | Protected ->
      "Protected"


(** Type for ObjC accessors *)
type objc_accessor_type = Objc_getter of Struct.field | Objc_setter of Struct.field
[@@deriving compare, equal]

let kind_of_objc_accessor_type accessor =
  match accessor with Objc_getter _ -> "getter" | Objc_setter _ -> "setter"


let pp_objc_accessor_type fmt objc_accessor_type =
  let field = match objc_accessor_type with Objc_getter field | Objc_setter field -> field in
  F.fprintf fmt "%s<%a:%a@,[%a]>"
    (kind_of_objc_accessor_type objc_accessor_type)
    Fieldname.pp field.name (Typ.pp Pp.text) field.typ
    (Pp.semicolon_seq ~print_env:Pp.text_break Annot.pp)
    field.annot


type var_data =
  { name: Mangled.t
  ; typ: Typ.t
  ; modify_in_block: bool
  ; is_constexpr: bool
  ; is_declared_unused: bool
  ; is_structured_binding: bool
  ; has_cleanup_attribute: bool
  ; tmp_id: Ident.t option }
[@@deriving compare]

let default_var_data pvar typ =
  { name= Pvar.get_name pvar
  ; typ
  ; modify_in_block= false
  ; is_constexpr= false
  ; is_declared_unused= false
  ; is_structured_binding= false
  ; has_cleanup_attribute= false
  ; tmp_id= Pvar.get_tmp_id pvar }


let pp_var_data fmt
    {name; typ; modify_in_block; is_declared_unused; is_structured_binding; has_cleanup_attribute} =
  F.fprintf fmt
    "@[<h>{ name=@ %a;@ typ=@ %a;@ modify_in_block=@ %b;@ is_declared_unused=@ %b;@ \
     is_structured_binding=@ %b;@ has_cleanup_attribute=@ %b@ }@]"
    Mangled.pp name (Typ.pp_full Pp.text) typ modify_in_block is_declared_unused
    is_structured_binding has_cleanup_attribute


type specialized_with_aliasing_info = {orig_proc: Procname.t; aliases: Pvar.t list list}
[@@deriving compare, equal]

type 'captured_var passed_closure =
  | Closure of (Procname.t * 'captured_var list)
  | Fields of (Fieldname.t * 'captured_var passed_closure) list
[@@deriving compare, equal]

type specialized_with_closures_info =
  {orig_proc: Procname.t; formals_to_closures: CapturedVar.t passed_closure Pvar.Map.t}
[@@deriving compare, equal]

type block_as_arg_attributes = {passed_to: Procname.t; passed_as_noescape_block: bool}
[@@deriving compare, equal]

let pp_block_as_arg_attributes fmt {passed_to; passed_as_noescape_block} =
  F.fprintf fmt "@[{ passed_to=%a;passed_as_noescape_block=%b } @]" Procname.pp passed_to
    passed_as_noescape_block


type t =
  { access: access  (** visibility access *)
  ; captured: CapturedVar.t list  (** name and type of variables captured in blocks *)
  ; mutable changed: bool  (** true if proc has changed since last analysis *)
  ; exceptions: string list  (** exceptions thrown by the procedure *)
  ; formals: (Mangled.t * Typ.t * Annot.Item.t) list  (** name and type of formal parameters *)
  ; const_formals: int list  (** list of indices of formals that are const-qualified *)
  ; reference_formals: int list  (** list of indices of formals that are passed by reference *)
  ; is_abstract: bool  (** the procedure is abstract *)
  ; is_biabduction_model: bool  (** the procedure is a model for the biabduction analysis *)
  ; is_bridge_method: bool  (** the procedure is a bridge method *)
  ; is_cpp_const_member_fun: bool  (** true if the procedure is a const function *)
  ; is_cpp_copy_assignment: bool  (** true if the procedure is a copy assignment *)
  ; is_cpp_copy_ctor: bool  (** true if the procedure is a copy constructor *)
  ; is_cpp_move_ctor: bool  (** true if the procedure is a move constructor *)
  ; is_cpp_deleted: bool  (** true if the procedure is deleted *)
  ; is_cpp_implicit: bool
        (** returns false if the declaration exists in code and true if it was created implicitly by
            the compiler *)
  ; is_defined: bool  (** true if the procedure is defined, and not just declared *)
  ; is_java_synchronized_method: bool  (** the procedure is a Java synchronized method *)
  ; is_csharp_synchronized_method: bool  (** the procedure is a C# synchronized method *)
  ; is_hack_async: bool
  ; is_hack_wrapper: bool
  ; block_as_arg_attributes: block_as_arg_attributes option
        (** Present if the procedure is an Objective-C block that has been passed to a given method
            as argument, including whether it is in a position annotated with the NS_NOESCAPE
            attribute. *)
  ; is_no_return: bool  (** the procedure is known not to return *)
  ; is_objc_arc_on: bool  (** the ObjC procedure is compiled with ARC *)
  ; is_specialized: bool  (** the procedure is a clone specialized for dynamic dispatch handling *)
  ; is_synthetic_method: bool  (** the procedure is a synthetic method *)
  ; is_clang_variadic: bool  (** the procedure is variadic, only supported for Clang procedures *)
  ; hack_variadic_position: int option
        (** the procedure is variadic and [Some n] means the variadic vector is composed of the
            arguments n, n+1, ..., length formals -1 *)
  ; sentinel_attr: (int * int) option  (** __attribute__((sentinel(int, int))) *)
  ; specialized_with_aliasing_info: specialized_with_aliasing_info option
  ; specialized_with_closures_info: specialized_with_closures_info option
        (** the procedure is a clone specialized with calls to concrete closures, with link to the
            original procedure, and a map that links the original formals to the elements of the
            closure used to specialize the procedure. *)
  ; clang_method_kind: ClangMethodKind.t  (** the kind of method the procedure is *)
  ; loc: Location.t  (** location of this procedure in the source code *)
  ; loc_instantiated: Location.t option  (** location of this procedure is possibly instantiated *)
  ; translation_unit: SourceFile.t  (** translation unit to which the procedure belongs *)
  ; mutable locals: var_data list  (** name, type and attributes of local variables *)
  ; objc_accessor: objc_accessor_type option  (** type of ObjC accessor, if any *)
  ; proc_name: Procname.t  (** name of the procedure *)
  ; ret_type: Typ.t  (** return type *)
  ; ret_annots: Annot.Item.t  (** annotations of return type *)
  ; has_added_return_param: bool  (** whether or not a return param was added *)
  ; is_ret_type_pod: bool  (** whether or not the return type is POD *)
  ; is_ret_constexpr: bool  (** whether the (C++) function or method is declared as [constexpr] *)
  }

let get_access attributes = attributes.access

let get_pvar_formals attributes =
  let pname = attributes.proc_name in
  List.map attributes.formals ~f:(fun (name, typ, _) -> (Pvar.mk name pname, typ))


let get_passed_by_value_formals attributes =
  List.filteri (get_pvar_formals attributes) ~f:(fun i _ ->
      not (List.mem ~equal:Int.equal attributes.reference_formals i) )


let get_passed_by_ref_formals attributes =
  List.filteri (get_pvar_formals attributes) ~f:(fun i _ ->
      List.mem ~equal:Int.equal attributes.reference_formals i )


let get_pointer_formals attributes =
  List.filter (get_pvar_formals attributes) ~f:(fun (_, typ) -> Typ.is_strong_pointer typ)


let get_proc_name attributes = attributes.proc_name

let get_loc attributes = attributes.loc

let get_loc_instantiated attributes = attributes.loc_instantiated

let to_return_type attributes =
  if attributes.has_added_return_param then
    match List.last attributes.formals with
    | Some (_, {Typ.desc= Tptr (t, _)}, _) ->
        t
    | _ ->
        attributes.ret_type
  else attributes.ret_type


let default translation_unit proc_name =
  { access= Default
  ; captured= []
  ; changed= true
  ; exceptions= []
  ; formals= []
  ; const_formals= []
  ; reference_formals= []
  ; is_abstract= false
  ; is_biabduction_model= false
  ; is_bridge_method= false
  ; is_cpp_const_member_fun= false
  ; is_cpp_copy_assignment= false
  ; is_cpp_copy_ctor= false
  ; is_cpp_move_ctor= false
  ; is_cpp_deleted= false
  ; is_cpp_implicit= false
  ; is_defined= false
  ; is_java_synchronized_method= false
  ; is_csharp_synchronized_method= false
  ; is_hack_async= false
  ; is_hack_wrapper= false
  ; block_as_arg_attributes= None
  ; is_no_return= false
  ; is_objc_arc_on= false
  ; is_specialized= false
  ; specialized_with_aliasing_info= None
  ; specialized_with_closures_info= None
  ; is_synthetic_method= false
  ; is_clang_variadic= false
  ; hack_variadic_position= None
  ; sentinel_attr= None
  ; clang_method_kind= ClangMethodKind.C_FUNCTION
  ; loc= Location.dummy
  ; loc_instantiated= None
  ; translation_unit
  ; locals= []
  ; has_added_return_param= false
  ; objc_accessor= None
  ; proc_name
  ; ret_type= StdTyp.void
  ; ret_annots= Annot.Item.empty
  ; is_ret_type_pod= true
  ; is_ret_constexpr= false }


let pp_parameters =
  Pp.semicolon_seq ~print_env:Pp.text_break (fun f (mangled, typ, _) ->
      Pp.pair ~fst:Mangled.pp ~snd:(Typ.pp_full Pp.text) f (mangled, typ) )


let pp_specialized_with_aliasing_info fmt (info : specialized_with_aliasing_info) =
  let pp_aliases fmt aliases =
    let pp_alias fmt alias = Pp.seq ~sep:"=" Pvar.pp_value fmt alias in
    PrettyPrintable.pp_collection ~pp_item:pp_alias fmt aliases
  in
  F.fprintf fmt "orig_procname=%a, aliases=%a" Procname.pp info.orig_proc pp_aliases info.aliases


let pp_specialized_with_closures_info fmt info =
  let rec pp_passed_closure fmt passed_closure =
    match passed_closure with
    | Closure closure ->
        let pp_captured_vars = Pp.semicolon_seq ~print_env:Pp.text_break CapturedVar.pp in
        Pp.pair ~fst:Procname.pp ~snd:pp_captured_vars fmt closure
    | Fields field_to_function_map ->
        PrettyPrintable.pp_collection
          ~pp_item:(fun fmt (fld, func) ->
            F.fprintf fmt "%a->%a" Fieldname.pp fld pp_passed_closure func )
          fmt field_to_function_map
  in
  F.fprintf fmt "orig_procname=%a, formals_to_closures=%a" Procname.pp info.orig_proc
    (Pvar.Map.pp ~pp_value:pp_passed_closure)
    info.formals_to_closures


let pp_captured = Pp.semicolon_seq ~print_env:Pp.text_break CapturedVar.pp

let pp f
    ({ access
     ; captured
     ; changed
     ; exceptions
     ; formals
     ; const_formals
     ; reference_formals
     ; is_abstract
     ; is_biabduction_model
     ; is_bridge_method
     ; is_cpp_const_member_fun
     ; is_cpp_copy_assignment
     ; is_cpp_copy_ctor
     ; is_cpp_move_ctor
     ; is_cpp_deleted
     ; is_cpp_implicit
     ; is_defined
     ; is_java_synchronized_method
     ; is_csharp_synchronized_method
     ; is_hack_async
     ; is_hack_wrapper
     ; block_as_arg_attributes
     ; is_no_return
     ; is_objc_arc_on
     ; is_specialized
     ; specialized_with_aliasing_info
     ; specialized_with_closures_info
     ; is_synthetic_method
     ; is_clang_variadic
     ; hack_variadic_position
     ; sentinel_attr
     ; clang_method_kind
     ; loc
     ; loc_instantiated
     ; translation_unit
     ; locals
     ; has_added_return_param
     ; objc_accessor
     ; proc_name
     ; ret_type
     ; ret_annots
     ; is_ret_type_pod
     ; is_ret_constexpr } [@warning "+missing-record-field-pattern"] ) =
  let default = default translation_unit proc_name in
  let pp_bool_default ~default title b f () =
    if not (Bool.equal default b) then F.fprintf f "; %s= %b@," title b
  in
  F.fprintf f "@[<v>{ proc_name= %a@,; translation_unit= %a@," Procname.pp proc_name SourceFile.pp
    translation_unit ;
  if not (equal_access default.access access) then
    F.fprintf f "; access= %a@," (Pp.of_string ~f:string_of_access) access ;
  if not ([%equal: CapturedVar.t list] default.captured captured) then
    F.fprintf f "; captured= [@[%a@]]@," pp_captured captured ;
  if not (Bool.equal default.changed changed) then
    F.fprintf f "; changed_since_last_capture= %b@," changed ;
  if not ([%equal: string list] default.exceptions exceptions) then
    F.fprintf f "; exceptions= [@[%a@]]@,"
      (Pp.semicolon_seq ~print_env:Pp.text_break F.pp_print_string)
      exceptions ;
  (* always print formals *)
  F.fprintf f "; formals= [@[%a@]]@," pp_parameters formals ;
  if not ([%equal: int list] default.const_formals const_formals) then
    F.fprintf f "; const_formals= [@[%a@]]@,"
      (Pp.semicolon_seq ~print_env:Pp.text_break F.pp_print_int)
      const_formals ;
  if not ([%equal: int list] default.reference_formals reference_formals) then
    F.fprintf f "; reference_formals= [@[%a@]]@,"
      (Pp.semicolon_seq ~print_env:Pp.text_break F.pp_print_int)
      reference_formals ;
  pp_bool_default ~default:default.is_abstract "is_abstract" is_abstract f () ;
  pp_bool_default ~default:default.is_biabduction_model "is_model" is_biabduction_model f () ;
  pp_bool_default ~default:default.is_bridge_method "is_bridge_method" is_bridge_method f () ;
  pp_bool_default ~default:default.is_cpp_const_member_fun "is_cpp_const_member_fun "
    is_cpp_const_member_fun f () ;
  pp_bool_default ~default:default.is_cpp_copy_assignment "is_cpp_copy_assignment"
    is_cpp_copy_assignment f () ;
  pp_bool_default ~default:default.is_cpp_copy_ctor "is_cpp_copy_ctor" is_cpp_copy_ctor f () ;
  pp_bool_default ~default:default.is_cpp_move_ctor "is_cpp_move_ctor" is_cpp_move_ctor f () ;
  pp_bool_default ~default:default.is_cpp_deleted "is_deleted" is_cpp_deleted f () ;
  pp_bool_default ~default:default.is_cpp_implicit "is_cpp_implicit" is_cpp_implicit f () ;
  pp_bool_default ~default:default.is_defined "is_defined" is_defined f () ;
  pp_bool_default ~default:default.is_java_synchronized_method "is_java_synchronized_method"
    is_java_synchronized_method f () ;
  pp_bool_default ~default:default.is_csharp_synchronized_method "is_csharp_synchronized_method"
    is_csharp_synchronized_method f () ;
  pp_bool_default ~default:default.is_hack_async "is_hack_async" is_hack_async f () ;
  pp_bool_default ~default:default.is_hack_wrapper "is_hack_wrapper" is_hack_wrapper f () ;
  if
    not
      ([%equal: block_as_arg_attributes option] default.block_as_arg_attributes
         block_as_arg_attributes )
  then
    F.fprintf f "; block_as_arg_attributes %a"
      (Pp.option pp_block_as_arg_attributes)
      block_as_arg_attributes ;
  pp_bool_default ~default:default.is_no_return "is_no_return" is_no_return f () ;
  pp_bool_default ~default:default.is_objc_arc_on "is_objc_arc_on" is_objc_arc_on f () ;
  pp_bool_default ~default:default.is_specialized "is_specialized" is_specialized f () ;
  if
    not
      ([%equal: specialized_with_aliasing_info option] default.specialized_with_aliasing_info
         specialized_with_aliasing_info )
  then
    F.fprintf f "; specialized_with_aliasing_info %a@,"
      (Pp.option pp_specialized_with_aliasing_info)
      specialized_with_aliasing_info ;
  if
    not
      ([%equal: specialized_with_closures_info option] default.specialized_with_closures_info
         specialized_with_closures_info )
  then
    F.fprintf f "; specialized_with_closures_info %a@,"
      (Pp.option pp_specialized_with_closures_info)
      specialized_with_closures_info ;
  pp_bool_default ~default:default.is_synthetic_method "is_synthetic_method" is_synthetic_method f
    () ;
  pp_bool_default ~default:default.is_clang_variadic "is_clang_variadic" is_clang_variadic f () ;
  Option.iter hack_variadic_position ~f:(fun n -> F.fprintf f "; hack_variadic_position= %d@," n) ;
  if not ([%equal: (int * int) option] default.sentinel_attr sentinel_attr) then
    F.fprintf f "; sentinel_attr= %a@,"
      (Pp.option (Pp.pair ~fst:F.pp_print_int ~snd:F.pp_print_int))
      sentinel_attr ;
  if not (ClangMethodKind.equal default.clang_method_kind clang_method_kind) then
    F.fprintf f "; clang_method_kind= %a@,"
      (Pp.of_string ~f:ClangMethodKind.to_string)
      clang_method_kind ;
  if not (Location.equal default.loc loc) then F.fprintf f "; loc= %a@," Location.pp_file_pos loc ;
  Option.iter loc_instantiated ~f:(fun loc_instantiated ->
      F.fprintf f "; loc_instantiated= %a@," Location.pp_file_pos loc_instantiated ) ;
  F.fprintf f "; locals= [@[%a@]]@," (Pp.semicolon_seq ~print_env:Pp.text_break pp_var_data) locals ;
  pp_bool_default ~default:default.has_added_return_param "has_added_return_param"
    has_added_return_param f () ;
  if not ([%equal: objc_accessor_type option] default.objc_accessor objc_accessor) then
    F.fprintf f "; objc_accessor= %a@," (Pp.option pp_objc_accessor_type) objc_accessor ;
  (* always print ret type *)
  F.fprintf f "; ret_type= %a @," (Typ.pp_full Pp.text) ret_type ;
  if not (Annot.Item.is_empty ret_annots) then
    F.fprintf f "; ret_annots= %a@," Annot.Item.pp ret_annots ;
  pp_bool_default ~default:default.is_ret_type_pod "is_ret_type_pod" is_ret_type_pod f () ;
  pp_bool_default ~default:default.is_ret_constexpr "is_ret_constexpr" is_ret_constexpr f () ;
  F.fprintf f "; proc_id= %a }@]" Procname.pp_unique_id proc_name


let get_this attributes =
  let open IOption.Let_syntax in
  let+ name =
    match Procname.get_language attributes.proc_name with
    | Java ->
        if Procname.is_java_instance_method attributes.proc_name then Some Mangled.this else None
    | Clang -> (
        if Procname.is_objc_instance_method attributes.proc_name then Some Mangled.self
        else match attributes.clang_method_kind with CPP_INSTANCE -> Some Mangled.this | _ -> None )
    | CIL | Erlang | Hack | Python ->
        None
  in
  Pvar.mk name attributes.proc_name


module SQLite = SqliteUtils.MarshalledDataNOTForComparison (struct
  type nonrec t = t
end)
