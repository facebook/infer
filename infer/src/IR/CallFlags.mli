(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The Smallfoot Intermediate Language: Call Flags *)

open! IStd
module F = Format

(** Flags for a procedure call *)
type t =
  { cf_assign_last_arg: bool
  ; cf_injected_destructor: bool
        (** true if this is an implicit C++ destructor call injected by the clang frontend *)
  ; cf_interface: bool
  ; cf_is_objc_block: bool
  ; cf_is_objc_getter_setter: bool
  ; cf_virtual: bool
  ; cf_return_null_checked: bool
        (** true if the value returned by this call is null-checked by the caller before use. Read
            by nullability checkers to suppress missing-annotation reports at the call site when the
            caller already handles a possible nil. Also set by the clang frontend on the allocation
            of a C++ new-expression whose initialization is null-checked. *)
  ; cf_caller_ret_annots: Annot.Item.t
        (** annotations the caller has decided about the return value of this specific call,
            independent of any annotations on the callee's procdesc. Populated by frontends that
            recover information about how the caller treats the result (e.g. the Swift frontend
            recognising that the caller sees an ObjC-method result as [Optional<T>]). Combined with
            the callee's [ret_annots] when checkers consult nullability. *) }
[@@deriving compare, equal, hash, normalize]

val pp : F.formatter -> t -> unit

val default : t
(** Default value where all fields are set to false / empty *)
