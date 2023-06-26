(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module HeapPath : sig
  (* Heap symbolic paths in a precondition context *)
  type t = Pvar of Pvar.t | FieldAccess of (Fieldname.t * t) | Dereference of t
  [@@deriving equal, compare]

  val pp : F.formatter -> t -> unit

  module Map : PrettyPrintable.PPMap with type key = t
end

module Pulse : sig
  module Aliases : sig
    (** set of alias sets (Note: list is enough because it is normalised during construction) *)
    type t = Pvar.t list list [@@deriving equal, compare]
  end

  module DynamicTypes : sig
    (** binding from heap paths to their inferred dynamic type (will be used for devirtualization in
        the callee) *)
    type t = Typ.name HeapPath.Map.t [@@deriving equal, compare]
  end

  type t = Aliases of Aliases.t | DynamicTypes of DynamicTypes.t [@@deriving equal, compare]

  val pp : F.formatter -> t -> unit [@@warning "-unused-value-declaration"]

  module Map : PrettyPrintable.PPMap with type key = t

  val is_pulse_specialization_limit_not_reached : 'a Map.t -> bool
end

(** Summary specialization is a technique that permits to increase the precision of summary by
    specializing them to calling context. Each analysis can choose its notion of specialization. The
    interprocedural engine will reanalyze a procedure if it needs specialization and it will add the
    obtained specialized summary to the summaries stored for this procedure. This techique avoids
    cloning procedures. *)
type t = Pulse of Pulse.t
