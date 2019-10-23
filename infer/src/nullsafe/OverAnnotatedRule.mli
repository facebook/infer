(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

(** Checks if a type in signature (e.g. return value) can be made more specific.
   NOTE: This rule is complementatary to assignment rule.
         While assignment rule checks a single assignment `lhs = rhs`, this rule
         checks checks ALL assignments to `lhs` in the program.
   NOTE: Violation of this rule is not a type violation, hence it should never be surfaced as error:
         `lhs`'s type can be intentionally made broad by code author
         (e.g. to anticipate future changes in the implementation).
         Heuristits are required to correctly surface overannotated rule to the user.
         This rule is useful for some scenarios, especially for nullability code conversions
         when it is expected that some signatures were annotated with @Nullable defensively, so
         surfacing such cases can improve API and make migration smooth.
 *)

type violation [@@deriving compare]

val check : what:Nullability.t -> by_rhs_upper_bound:Nullability.t -> (unit, violation) result
(**
   Checks if the declared type for `what` can be narrowed, based on the information about
   all assignments `what = rhs_i`.
   If an upper bound of `rhs_i` over ALL assignents `what = rhs_i` that exist in the program
   is a _strict_ subtype of lhs, `lhs`'s type can be narrowed to be that upper bound.
*)

type violation_type =
  | FieldOverAnnoted of Typ.Fieldname.t
  | ReturnOverAnnotated of Typ.Procname.t  (** Return value of a method can be made non-nullable *)
[@@deriving compare]

val violation_description : violation -> violation_type -> string
