(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** We can handle Jave signatures like the one below

    {[
      public static void main(A[] x1, C<Gg,? super D>.B x2) throws A, B.C
    ]}

    Notes:

    + We do not record the throws-part in the internal representation, even though the syntax
      supports throws-clauses. This is so, because throws clauses are not used when identifying
      method calls
    + We do not record the names of the formal parameters in the internal representation, for the
      same reasons as in the previous point *)

(** Moreover, signatures are "extended" to allow for a sequence of signatures, and an "under part"
    describing a superclass or implemented interface *)

(** Note that we support abstract modifiers, even though Java abstract methods cannot have bodies,
    and therefore it is debatable whether it makes sense to have DoLi descriptions for absrract
    methods -- unless we want to have some abstraction over all the concrete methods overriding the
    abstarct method *)
type modifier = Public | Private | Protected | Static | Final | Abstract | Native

(** Doli's description of Java types *)
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
