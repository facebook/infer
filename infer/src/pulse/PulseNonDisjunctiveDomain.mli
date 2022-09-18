(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module BaseMemory = PulseBaseMemory

type copy_spec_t =
  | Copied of
      { typ: Typ.t
      ; location: Location.t (* the location to report the issue *)
      ; copied_location: Location.t option
            (* [copied_location] has a value when the copied location is different to where to
               report: e.g. this is the case for returning copied values. *)
      ; heap: BaseMemory.t
      ; from: PulseAttribute.CopyOrigin.t }
  | Modified

type parameter_spec_t =
  | Unmodified of {typ: Typ.t; location: Location.t; heap: BaseMemory.t}
  | Modified

include AbstractDomain.WithBottomTop

val add_var :
     Var.t
  -> source_addr_opt:PulseAbstractValue.t option
  -> source_opt:Pvar.t option
  -> copy_spec_t
  -> t
  -> t

val add_field : Fieldname.t -> source_opt:PulseDecompilerExpr.t option -> copy_spec_t -> t -> t

val add_parameter : Var.t -> parameter_spec_t -> t -> t

val checked_via_dtor : Var.t -> t -> t

val mark_copy_as_modified :
     is_modified:(BaseMemory.t -> bool)
  -> copied_into:PulseAttribute.CopiedInto.t
  -> source_addr_opt:PulseAbstractValue.t option
  -> t
  -> t

val mark_parameter_as_modified : is_modified:(BaseMemory.t -> bool) -> var:Var.t -> t -> t

val get_copied :
     t
  -> ( PulseAttribute.CopiedInto.t
     * Typ.t
     * Location.t
     * Location.t option
     * PulseAttribute.CopyOrigin.t )
     list

val get_const_refable_parameters : t -> (Var.t * Typ.t * Location.t) list

val is_checked_via_dtor : Var.t -> t -> bool

val set_captured_variables : Exp.t -> t -> t

val set_locked : t -> t

val is_locked : t -> bool
