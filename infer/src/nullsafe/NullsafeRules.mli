(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** This is a single place consolidating core rules driving Nullsafe type checking.
  Nullsafe enforces similar rules in different places (e.g. places dealing with fields,
  function calls, assignments, local variables etc.).
  Those places might have additional specifics, but core checks should be done through this class.
  If you are writing a new or modifying an existing check, ask yourself if you can directly
  use already existng rules from this module.
  If you feel you need a rule of a completely new nature, add it to this module.
  As a rule of thumb, every different "check" that is responsible for detecting issues, should query
  this module instead of doing things on their own.
  *)

val passes_assignment_rule : lhs:NullsafeType.nullability -> rhs:InferredNullability.t -> bool
(** Assignment rule: No expression of nullable type is ever assigned to a location
    of non-nullable type.
  *)
