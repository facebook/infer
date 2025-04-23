(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type builtin =
  | AsyncGenValueWrapperNew
  | AttributesOfMatchClass
  | BinaryAdd
  | BinaryAnd
  | BinaryFloorDivide
  | BinaryLshift
  | BinaryMatrixMultiply
  | BinaryModulo
  | BinaryMultiply
  | BinaryOr
  | BinaryPower
  | BinaryRshift
  | BinarySlice
  | BinarySubstract
  | BinaryTrueDivide
  | BinaryXor
  | BoolFalse
  | BoolOfMatchClass
  | Bool
  | BoolTrue
  | BuildClass
  | BuildFrozenSet
  | BuildList
  | BuildMap
  | BuildSet
  | BuildSlice
  | BuildString
  | BuildTuple
  | BuildUnpackList
  | BuildUnpackMap
  | BuildUnpackSet
  | BuildUnpackTuple
  | Call
  | CallFunctionEx
  | CallMethod
  | CompareBad
  | CompareEq
  | CompareException
  | CompareGe
  | CompareGt
  | CompareIn
  | CompareIs
  | CompareIsNot
  | CompareLe
  | CompareLt
  | CompareNeq
  | CompareNotIn
  | CopyFreeVars
  | DeleteAttr
  | DeleteDeref
  | DeleteFast
  | DeleteGlobal
  | DeleteName
  | DeleteSubscr
  | DictMerge
  | DictSetItem
  | DictUpdate
  | Format
  | FormatFnAscii
  | FormatFnRepr
  | FormatFnStr
  | GenStartAsyncGenerator
  | GenStartCoroutine
  | GenStartGenerator
  | GetAiter
  | GetAttr
  | GetAwaitable
  | GetIter
  | GetLen
  | GetPreviousException
  | GetYieldFromIter
  | HasNextIter
  | ImportFrom
  | ImportName
  | ImportStar
  | InplaceAdd
  | InplaceAnd
  | InplaceFloorDivide
  | InplaceLshift
  | InplaceMatrixMultiply
  | InplaceModulo
  | InplaceMultiply
  | InplaceOr
  | InplacePower
  | InplaceRshift
  | InplaceSubstract
  | InplaceTrueDivide
  | InplaceXor
  | InvalidUnicode
  | IterData
  | ListAppend
  | ListExtend
  | ListToTuple
  | LoadAssertionError
  | LoadClassDeref
  | LoadClosure
  | LoadDeref
  | LoadFast
  | LoadFastAndClear
  | LoadFastCheck
  | LoadFromDictOrDeref
  | LoadGlobal
  | LoadName
  | LoadLocals
  | LoadSuperAttr
  | MakeBytes
  | MakeComplex
  | MakeCell
  | MakeFloat
  | MakeFunction
  | MakeInt
  | MakeNone
  | MakeString
  | MatchClass
  | MatchSequence
  | NextIter
  | NullifyLocals
  | PrepReraiseStar
  | SetAdd
  | SetAttr
  | SetUpdate
  | SetupAnnotations
  | StoreDeref
  | StoreFast
  | StoreGlobal
  | StoreName
  | StoreSlice
  | StoreSubscript
  | Subscript
  | SetFunctionTypeParams
  | TypevarWithBound
  | TypevarWithConstraints
  | UnaryInvert
  | UnaryNegative
  | UnaryNot
  | UnaryPos
  | UnaryPositive
  | UnpackEx
  | Yield
  | YieldFrom
[@@deriving compare, equal, yojson_of, sexp, hash, normalize, enumerate]

let show_builtin = function
  | AsyncGenValueWrapperNew ->
      "py_async_gen_value_wrapper_new"
  | AttributesOfMatchClass ->
      "py_attributes_of_match_class"
  | BinaryAdd ->
      "py_binary_add"
  | BinaryAnd ->
      "py_binary_and"
  | BinaryFloorDivide ->
      "py_binary_floor_divide"
  | BinaryLshift ->
      "py_binary_lshift"
  | BinaryMatrixMultiply ->
      "py_binary_matrix_multiply"
  | BinaryModulo ->
      "py_binary_modulo"
  | BinaryMultiply ->
      "py_binary_multiply"
  | BinaryOr ->
      "py_binary_or"
  | BinaryPower ->
      "py_binary_power"
  | BinaryRshift ->
      "py_binary_rshift"
  | BinarySlice ->
      "py_binary_slice"
  | BinarySubstract ->
      "py_binary_substract"
  | BinaryTrueDivide ->
      "py_binary_true_divide"
  | BinaryXor ->
      "py_binary_xor"
  | BoolFalse ->
      "py_bool_false"
  | BoolOfMatchClass ->
      "py_bool_of_match_class"
  | Bool ->
      "py_bool"
  | BoolTrue ->
      "py_bool_true"
  | BuildClass ->
      "py_build_class"
  | BuildFrozenSet ->
      "py_build_frozen_set"
  | BuildList ->
      "py_build_list"
  | BuildMap ->
      "py_build_map"
  | BuildSet ->
      "py_build_set"
  | BuildSlice ->
      "py_build_slice"
  | BuildString ->
      "py_build_string"
  | BuildTuple ->
      "py_build_tuple"
  | BuildUnpackList ->
      "py_build_unpack_list"
  | BuildUnpackMap ->
      "py_build_unpack_map"
  | BuildUnpackSet ->
      "py_build_unpack_set"
  | BuildUnpackTuple ->
      "py_build_unpack_tuple"
  | Call ->
      "py_call"
  | CallFunctionEx ->
      "py_call_function_ex"
  | CallMethod ->
      "py_call_method"
  | CompareBad ->
      "py_compare_bad"
  | CompareEq ->
      "py_compare_eq"
  | CompareException ->
      "py_compare_exception"
  | CompareGe ->
      "py_compare_ge"
  | CompareGt ->
      "py_compare_gt"
  | CompareIn ->
      "py_compare_in"
  | CompareIs ->
      "py_compare_is"
  | CompareIsNot ->
      "py_compare_is_not"
  | CompareLe ->
      "py_compare_le"
  | CompareLt ->
      "py_compare_lt"
  | CompareNeq ->
      "py_compare_neq"
  | CompareNotIn ->
      "py_compare_not_in"
  | CopyFreeVars ->
      "py_copy_free_vars"
  | DeleteAttr ->
      "py_delete_attr"
  | DeleteDeref ->
      "py_delete_deref"
  | DeleteFast ->
      "py_delete_fast"
  | DeleteGlobal ->
      "py_delete_global"
  | DeleteName ->
      "py_delete_name"
  | DeleteSubscr ->
      "py_delete_subscr"
  | DictMerge ->
      "py_dict_merge"
  | DictSetItem ->
      "py_dict_set_item"
  | DictUpdate ->
      "py_dict_update"
  | Format ->
      "py_format"
  | FormatFnAscii ->
      "py_format_fn_ascii"
  | FormatFnRepr ->
      "py_format_fn_repr"
  | FormatFnStr ->
      "py_format_fn_str"
  | GenStartAsyncGenerator ->
      "py_gen_start_async_generator"
  | GenStartCoroutine ->
      "py_gen_start_coroutine"
  | GenStartGenerator ->
      "py_gen_start_generator"
  | GetAiter ->
      "py_get_aiter"
  | GetAttr ->
      "py_get_attr"
  | GetAwaitable ->
      "py_get_awaitable"
  | GetIter ->
      "py_get_iter"
  | GetLen ->
      "py_get_len"
  | GetPreviousException ->
      "py_get_previous_exception"
  | GetYieldFromIter ->
      "py_get_yield_from_iter"
  | HasNextIter ->
      "py_has_next_iter"
  | ImportFrom ->
      "py_import_from"
  | ImportName ->
      "py_import_name"
  | ImportStar ->
      "py_import_star"
  | InplaceAdd ->
      "py_inplace_add"
  | InplaceAnd ->
      "py_inplace_and"
  | InplaceFloorDivide ->
      "py_inplace_floor_divide"
  | InplaceLshift ->
      "py_inplace_lshift"
  | InplaceMatrixMultiply ->
      "py_inplace_matrix_multiply"
  | InplaceModulo ->
      "py_inplace_modulo"
  | InplaceMultiply ->
      "py_inplace_multiply"
  | InplaceOr ->
      "py_inplace_or"
  | InplacePower ->
      "py_inplace_power"
  | InplaceRshift ->
      "py_inplace_rshift"
  | InplaceSubstract ->
      "py_inplace_substract"
  | InplaceTrueDivide ->
      "py_inplace_true_divide"
  | InplaceXor ->
      "py_inplace_xor"
  | InvalidUnicode ->
      "py_invalid_unicode"
  | IterData ->
      "py_iter_data"
  | ListAppend ->
      "py_list_append"
  | ListExtend ->
      "py_list_extend"
  | ListToTuple ->
      "py_list_to_tuple"
  | LoadAssertionError ->
      "py_load_assertion_error"
  | LoadClassDeref ->
      "py_load_class_deref"
  | LoadClosure ->
      "py_load_closure"
  | LoadDeref ->
      "py_load_deref"
  | LoadFast ->
      "py_load_fast"
  | LoadFastAndClear ->
      "py_load_fast_and_clear"
  | LoadFastCheck ->
      "py_load_fast_check"
  | LoadFromDictOrDeref ->
      "py_load_from_dict_or_deref"
  | LoadGlobal ->
      "py_load_global"
  | LoadName ->
      "py_load_name"
  | LoadLocals ->
      "py_load_locals"
  | LoadSuperAttr ->
      "py_load_super_attr"
  | MakeBytes ->
      "py_make_bytes"
  | MakeComplex ->
      "py_make_complex"
  | MakeCell ->
      "py_make_cell"
  | MakeFloat ->
      "py_make_float"
  | MakeFunction ->
      "py_make_function"
  | MakeInt ->
      "py_make_int"
  | MakeNone ->
      "py_make_none"
  | MakeString ->
      "py_make_string"
  | MatchClass ->
      "py_match_class"
  | MatchSequence ->
      "py_match_sequence"
  | NextIter ->
      "py_next_iter"
  | NullifyLocals ->
      "py_nullify_locals"
  | PrepReraiseStar ->
      "py_prep_reraise_star"
  | SetAdd ->
      "py_set_add"
  | SetAttr ->
      "py_set_attr"
  | SetUpdate ->
      "py_set_update"
  | SetupAnnotations ->
      "py_setup_annotations"
  | StoreDeref ->
      "py_store_deref"
  | StoreFast ->
      "py_store_fast"
  | StoreGlobal ->
      "py_store_global"
  | StoreName ->
      "py_store_name"
  | StoreSlice ->
      "py_store_slice"
  | StoreSubscript ->
      "py_store_subscript"
  | Subscript ->
      "py_subscript"
  | SetFunctionTypeParams ->
      "py_set_function_type_params"
  | TypevarWithBound ->
      "py_typevar_with_bound"
  | TypevarWithConstraints ->
      "py_typevar_with_constraints"
  | UnaryInvert ->
      "py_unary_invert"
  | UnaryNegative ->
      "py_unary_negative"
  | UnaryNot ->
      "py_unary_not"
  | UnaryPos ->
      "py_unary_pos"
  | UnaryPositive ->
      "py_unary_positive"
  | UnpackEx ->
      "py_unpack_ex"
  | Yield ->
      "py_yield"
  | YieldFrom ->
      "py_yield_from"


let builtin_from_string =
  let tbl = IString.Hash.create 100 in
  List.iter all_of_builtin ~f:(fun builtin -> IString.Hash.add tbl (show_builtin builtin) builtin) ;
  fun str -> IString.Hash.find_opt tbl str


type t = Builtin of builtin | Regular of {module_name: PythonClassName.t; function_name: string}
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

let get_module_type_name = function
  | Builtin _ ->
      None
  | Regular {module_name} ->
      Some (Typ.PythonClass module_name)


let get_module_name_as_a_string = function
  | Builtin _ ->
      "$builtins"
  | Regular {module_name} ->
      PythonClassName.classname module_name


let get_method = function
  | Builtin builtin ->
      show_builtin builtin
  | Regular {function_name} ->
      function_name


let pp fmt = function
  | Builtin builtin ->
      F.fprintf fmt "py_%s" (show_builtin builtin)
  | Regular {module_name; function_name} ->
      F.fprintf fmt "%a.%s" PythonClassName.pp module_name function_name
