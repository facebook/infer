(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module CallEvent = PulseCallEvent

type event =
  | Allocation of {f: CallEvent.t; location: Location.t}
  | Assignment of Location.t
  | Call of {f: CallEvent.t; location: Location.t; in_call: t}
  | Capture of {captured_as: Pvar.t; mode: Pvar.capture_mode; location: Location.t}
  | Conditional of {is_then_branch: bool; if_kind: Sil.if_kind; location: Location.t}
  | CppTemporaryCreated of Location.t
  | FormalDeclared of Pvar.t * Location.t
  | StructFieldAddressCreated of Fieldname.t RevList.t * Location.t
  | VariableAccessed of Pvar.t * Location.t
  | VariableDeclared of Pvar.t * Location.t

and t = event list [@@deriving compare, equal, yojson_of]

val pp : F.formatter -> t -> unit

val pp_fields : F.formatter -> Fieldname.t RevList.t -> unit

val location_of_event : event -> Location.t

val add_to_errlog : nesting:int -> t -> Errlog.loc_trace_elem list -> Errlog.loc_trace_elem list
