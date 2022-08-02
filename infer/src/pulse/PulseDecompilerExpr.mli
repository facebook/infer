(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module AbstractValue = PulseAbstractValue
module CallEvent = PulseCallEvent

type base = PVar of Pvar.t | ReturnValue of CallEvent.t [@@deriving compare, equal]

type access =
  | CaptureFieldAccess of CapturedVar.t
  | FieldAccess of Fieldname.t
  | ArrayAccess of source_expr option
  | TakeAddress
  | Dereference

and source_expr = base * access list [@@deriving compare, equal]

type t = SourceExpr of source_expr * AbstractValue.t option | Unknown of AbstractValue.t option
[@@deriving compare, equal]

val pp : F.formatter -> t -> unit

val pp_source_expr : F.formatter -> source_expr -> unit

val pp_with_abstract_value : Format.formatter -> t -> unit

val abstract_value_of_expr : t -> AbstractValue.t option

val is_unknown : t -> bool

val yojson_of_t : t -> Yojson.Safe.t

val reset_abstract_value : t -> t
(** forget the underlying abstract value in the argument:
    [abstract_value_of_expr (reset_abstract_value expr)] is [None] *)
