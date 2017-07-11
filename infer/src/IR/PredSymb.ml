(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** The Smallfoot Intermediate Language: Predicate Symbols *)
open! IStd
module L = Logging
module F = Format

type func_attribute =
  | FA_sentinel of int * int  (** __attribute__((sentinel(int, int))) *)
  [@@deriving compare]

(** Visibility modifiers. *)
type access = Default | Public | Private | Protected [@@deriving compare]

let equal_access = [%compare.equal : access]

(** Return the value of the FA_sentinel attribute in [attr_list] if it is found *)
let get_sentinel_func_attribute_value attr_list =
  match attr_list with
  | (FA_sentinel (sentinel, null_pos)) :: _
   -> Some (sentinel, null_pos)
  | []
   -> None

type mem_kind =
  | Mmalloc  (** memory allocated with malloc *)
  | Mnew  (** memory allocated with new *)
  | Mnew_array  (** memory allocated with new[] *)
  | Mobjc  (** memory allocated with objective-c alloc *)
  [@@deriving compare]

(** resource that can be allocated *)
type resource = Rmemory of mem_kind | Rfile | Rignore | Rlock [@@deriving compare]

(** kind of resource action *)
type res_act_kind = Racquire | Rrelease [@@deriving compare]

let equal_res_act_kind = [%compare.equal : res_act_kind]

(** kind of dangling pointers *)
type dangling_kind =
  (** pointer is dangling because it is uninitialized *)
  | DAuninit
      (** pointer is dangling because it is the address
      of a stack variable which went out of scope *)
  | DAaddr_stack_var  (** pointer is -1 *)
  | DAminusone
  [@@deriving compare]

(** position in a path: proc name, node id *)
type path_pos = Typ.Procname.t * int [@@deriving compare]

let equal_path_pos = [%compare.equal : path_pos]

type taint_kind =
  | Tk_unverified_SSL_socket
  | Tk_shared_preferences_data
  | Tk_privacy_annotation
  | Tk_integrity_annotation
  | Tk_unknown
  [@@deriving compare]

type taint_info = {taint_source: Typ.Procname.t; taint_kind: taint_kind} [@@deriving compare]

(** acquire/release action on a resource *)
type res_action =
  { ra_kind: res_act_kind  (** kind of action *)
  ; ra_res: resource  (** kind of resource *)
  ; ra_pname: Typ.Procname.t  (** name of the procedure used to acquire/release the resource *)
  ; ra_loc: Location.t  (** location of the acquire/release *)
  ; ra_vpath: DecompiledExp.vpath  (** vpath of the resource value *) }

(* ignore other values beside resources: arbitrary merging into one *)
let compare_res_action {ra_kind= k1; ra_res= r1} {ra_kind= k2; ra_res= r2} =
  [%compare : res_act_kind * resource] (k1, r1) (k2, r2)

(* type aliases for components of t values that compare should ignore *)
type _annot_item = Annot.Item.t

let compare__annot_item _ _ = 0

type _location = Location.t

let compare__location _ _ = 0

type _path_pos = path_pos

let compare__path_pos _ _ = 0

(** Attributes are nary function symbols that are applied to expression arguments in Apred and
    Anpred atomic formulas.  Many operations don't make much sense for nullary predicates, and are
    generally treated as no-ops.  The first argument is treated specially, as the "anchor" of the
    predicate application.  For example, adding or removing an attribute uses the anchor to identify
    the atom to operate on.  Also, abstraction and normalization operations treat the anchor
    specially and maintain more information on it than other arguments.  Therefore when attaching an
    attribute to an expression, that expression should be the first argument, optionally followed by
    additional related expressions. *)
type t =
  | Aresource of res_action  (** resource acquire/release *)
  | Aautorelease
  | Adangling of dangling_kind  (** dangling pointer *)
  (** undefined value obtained by calling the given procedure, plus its return value annots *)
  | Aundef of Typ.Procname.t * _annot_item * _location * _path_pos
  | Ataint of taint_info
  | Auntaint of taint_info
  | Alocked
  | Aunlocked  (** value appeared in second argument of division at given path position *)
  | Adiv0 of path_pos
      (** attributed exp is null due to a call to a method with given path as null receiver *)
  | Aobjc_null
      (** value was returned from a call to the given procedure, plus the annots of the return value *)
  | Aretval of Typ.Procname.t * Annot.Item.t
      (** denotes an object registered as an observers to a notification center *)
  | Aobserver  (** denotes an object unsubscribed from observers of a notification center *)
  | Aunsubscribed_observer
  [@@deriving compare]

let equal = [%compare.equal : t]

(** name of the allocation function for the given memory kind *)
let mem_alloc_pname = function
  | Mmalloc
   -> Typ.Procname.from_string_c_fun "malloc"
  | Mnew
   -> Typ.Procname.from_string_c_fun "new"
  | Mnew_array
   -> Typ.Procname.from_string_c_fun "new[]"
  | Mobjc
   -> Typ.Procname.from_string_c_fun "alloc"

(** name of the deallocation function for the given memory kind *)
let mem_dealloc_pname = function
  | Mmalloc
   -> Typ.Procname.from_string_c_fun "free"
  | Mnew
   -> Typ.Procname.from_string_c_fun "delete"
  | Mnew_array
   -> Typ.Procname.from_string_c_fun "delete[]"
  | Mobjc
   -> Typ.Procname.from_string_c_fun "dealloc"

(** Categories of attributes *)
type category =
  | ACresource
  | ACautorelease
  | ACtaint
  | AClock
  | ACdiv0
  | ACobjc_null
  | ACundef
  | ACretval
  | ACobserver
  [@@deriving compare]

let equal_category = [%compare.equal : category]

let to_category att =
  match att with
  | Aresource _ | Adangling _
   -> ACresource
  | Ataint _ | Auntaint _
   -> ACtaint
  | Alocked | Aunlocked
   -> AClock
  | Aautorelease
   -> ACautorelease
  | Adiv0 _
   -> ACdiv0
  | Aobjc_null
   -> ACobjc_null
  | Aretval _
   -> ACretval
  | Aundef _
   -> ACundef
  | Aobserver | Aunsubscribed_observer
   -> ACobserver

let is_undef = function Aundef _ -> true | _ -> false

(** convert the attribute to a string *)
let to_string pe = function
  | Aresource ra
   -> let mk_name = function
        | Mmalloc
         -> "ma"
        | Mnew
         -> "ne"
        | Mnew_array
         -> "na"
        | Mobjc
         -> "oc"
      in
      let name =
        match (ra.ra_kind, ra.ra_res) with
        | Racquire, Rmemory mk
         -> "MEM" ^ mk_name mk
        | Racquire, Rfile
         -> "FILE"
        | Rrelease, Rmemory mk
         -> "FREED" ^ mk_name mk
        | Rrelease, Rfile
         -> "CLOSED"
        | _, Rignore
         -> "IGNORE"
        | Racquire, Rlock
         -> "LOCKED"
        | Rrelease, Rlock
         -> "UNLOCKED"
      in
      let str_vpath =
        if Config.trace_error then F.asprintf "%a" (DecompiledExp.pp_vpath pe) ra.ra_vpath else ""
      in
      name ^ Binop.str pe Lt ^ Typ.Procname.to_string ra.ra_pname ^ ":"
      ^ string_of_int ra.ra_loc.Location.line ^ Binop.str pe Gt ^ str_vpath
  | Aautorelease
   -> "AUTORELEASE"
  | Adangling dk
   -> let dks =
        match dk with
        | DAuninit
         -> "UNINIT"
        | DAaddr_stack_var
         -> "ADDR_STACK"
        | DAminusone
         -> "MINUS1"
      in
      "DANGL" ^ Binop.str pe Lt ^ dks ^ Binop.str pe Gt
  | Aundef (pn, _, loc, _)
   -> "UND" ^ Binop.str pe Lt ^ Typ.Procname.to_string pn ^ Binop.str pe Gt ^ ":"
      ^ string_of_int loc.Location.line
  | Ataint {taint_source}
   -> "TAINTED[" ^ Typ.Procname.to_string taint_source ^ "]"
  | Auntaint _
   -> "UNTAINTED"
  | Alocked
   -> "LOCKED"
  | Aunlocked
   -> "UNLOCKED"
  | Adiv0 (_, _)
   -> "DIV0"
  | Aobjc_null
   -> "OBJC_NULL"
  | Aretval (pn, _)
   -> "RET" ^ Binop.str pe Lt ^ Typ.Procname.to_string pn ^ Binop.str pe Gt
  | Aobserver
   -> "OBSERVER"
  | Aunsubscribed_observer
   -> "UNSUBSCRIBED_OBSERVER"

(** dump an attribute *)
let d_attribute (a: t) = L.add_print_action (L.PTattribute, Obj.repr a)
