(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Final unknown function in trace *)
type final = UnknownFrom of Procname.t option

(** Trace elements *)
type elem =
  | ArrayDeclaration
  | Assign of AbsLoc.PowLoc.t
  | Global of AbsLoc.Loc.t
  | JavaIntDecleration
  | Parameter of AbsLoc.Loc.t
  | SetArraySize
  | Through

module Set : sig
  include AbstractDomain.WithBottom

  val singleton : Location.t -> elem -> t

  val singleton_final : Location.t -> final -> t

  val add_elem : Location.t -> elem -> t -> t

  val call : Location.t -> traces_caller:t -> traces_callee:t -> t
  (** Merge traces of [traces_caller] and [traces_callee] *)
end

(** Trace set with issue information *)
module Issue : sig
  include PrettyPrintable.PrintableOrderedType

  type binary = ArrayAccess | Binop

  val binary : Location.t -> binary -> Set.t -> Set.t -> t
  (** Construct issue trace of binary operation. When [binary] is [ArrayAccess], the former [Set.t]
      typed parameter is [offset] and the latter is [length] of array access. *)

  val alloc : Location.t -> Set.t -> t
  (** Construct issue trace of allocation *)

  val call : Location.t -> Set.t -> t -> t
  (** Merge caller's trace set and callee's issue, i.e., [call location caller callee] *)

  val has_unknown : t -> bool
  (** Check if the issue trace includes unknown function calls *)

  val exists_str : f:(string -> bool) -> t -> bool
  (** Check if the issue trace includes an abstract location that satisfies [f] *)

  val make_err_trace : description:string -> t -> (string * Errlog.loc_trace) list
  (** Convert to the common [Errlog] format. The return value is a list of labelled
      [Errlog.loc_trace]s. *)
end
