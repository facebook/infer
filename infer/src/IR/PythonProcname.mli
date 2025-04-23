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

val show_builtin : builtin -> string

val builtin_from_string : string -> builtin option

type t = Builtin of builtin | Regular of {module_name: PythonClassName.t; function_name: string}
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

val get_module_type_name : t -> Typ.name option

val get_module_name_as_a_string : t -> string

val get_method : t -> string

val pp : F.formatter -> t -> unit
