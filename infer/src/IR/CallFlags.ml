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
            of a C++ new-expression that only initializes the object when the allocation function
            does not return null. *)
  ; cf_caller_ret_annots: Annot.Item.t
        (** annotations the caller has decided about the return value of this specific call,
            independent of any annotations on the callee's procdesc. Populated by frontends that
            recover information about how the caller treats the result (e.g. the Swift frontend
            recognising that the caller sees an ObjC-method result as [Optional<T>]). Combined with
            the callee's [ret_annots] when checkers consult nullability. *) }
[@@deriving compare, equal, hash, normalize]

let pp f
    ({ cf_assign_last_arg
     ; cf_injected_destructor
     ; cf_interface
     ; cf_is_objc_block
     ; cf_is_objc_getter_setter
     ; cf_virtual
     ; cf_return_null_checked
     ; cf_caller_ret_annots }
     [@warning "+missing-record-field-pattern"] ) =
  if cf_assign_last_arg then F.pp_print_string f " assign_last" ;
  if cf_injected_destructor then F.pp_print_string f " injected" ;
  if cf_interface then F.pp_print_string f " interface" ;
  if cf_is_objc_block then F.pp_print_string f " objc_block" ;
  if cf_is_objc_getter_setter then F.pp_print_string f " objc_getter_setter" ;
  if cf_virtual then F.pp_print_string f " virtual" ;
  if cf_return_null_checked then F.pp_print_string f " return_null_checked" ;
  if not (List.is_empty cf_caller_ret_annots) then
    F.fprintf f " caller_ret_annots=%a" Annot.Item.pp cf_caller_ret_annots ;
  ()


let default =
  { cf_assign_last_arg= false
  ; cf_injected_destructor= false
  ; cf_interface= false
  ; cf_is_objc_block= false
  ; cf_is_objc_getter_setter= false
  ; cf_virtual= false
  ; cf_return_null_checked= false
  ; cf_caller_ret_annots= Annot.Item.empty }
