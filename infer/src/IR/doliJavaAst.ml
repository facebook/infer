(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type modifier = Public | Private | Protected | Static | Final | Abstract | Native

type basicType =
  | IntType
  | BoolType
  | ByteType
  | CharType
  | DoubleType
  | FloatType
  | LongType
  | ShortType

type typeArgument = PLAIN of referenceType | SUPER of referenceType | EXTENDS of referenceType

and referenceType = RT of classType list

and classType = CT of (string * typeArgument list)

and nonVoidType = RefType of referenceType | BasicType of basicType | Array of nonVoidType

and returnType = VoidType | NonVoid of nonVoidType

type signature =
  {modifiers: modifier list; returns: returnType; identifier: string; formParTypes: nonVoidType list}

type extendedSignature = {signs: signature list; under: referenceType}
