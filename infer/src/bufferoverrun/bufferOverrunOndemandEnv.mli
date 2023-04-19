(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Environment for on-demand symbol evaluation *)
type t =
  { tenv: Tenv.t  (** type environment *)
  ; typ_of_param_path: Symb.SymbolPath.partial -> Typ.t option  (** type of parameter *)
  ; may_last_field: Symb.SymbolPath.partial -> bool
        (** if the path is a last field of a class in C++ *)
  ; entry_location: Location.t  (** location of entry node *)
  ; integer_type_widths: IntegerWidths.t  (** bit sizes of integer types *)
  ; class_name: Typ.name option  (** class name of the procedure being analyzed *) }

val mk : Procdesc.t -> Tenv.t -> IntegerWidths.t -> t
