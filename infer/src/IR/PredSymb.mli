(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The Smallfoot Intermediate Language: Predicate Symbols *)

open! IStd

(** {2 Programs and Types} *)

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

val equal_res_act_kind : res_act_kind -> res_act_kind -> bool

(** kind of dangling pointers *)
type dangling_kind =
  | DAuninit  (** pointer is dangling because it is uninitialized *)
  | DAaddr_stack_var
      (** pointer is dangling because it is the address of a stack variable which went out of scope *)
  | DAminusone  (** pointer is -1 *)

(** position in a path: proc name, node id *)
type path_pos = Procname.t * int [@@deriving compare]

val equal_path_pos : path_pos -> path_pos -> bool

(** acquire/release action on a resource *)
type res_action =
  { ra_kind: res_act_kind  (** kind of action *)
  ; ra_res: resource  (** kind of resource *)
  ; ra_pname: Procname.t  (** name of the procedure used to acquire/release the resource *)
  ; ra_loc: Location.t  (** location of the acquire/release *)
  ; ra_vpath: DecompiledExp.vpath  (** vpath of the resource value *) }

(** Attributes are nary function symbols that are applied to expression arguments in Apred and
    Anpred atomic formulas. Many operations don't make much sense for nullary predicates, and are
    generally treated as no-ops. The first argument is treated specially, as the "anchor" of the
    predicate application. For example, adding or removing an attribute uses the anchor to identify
    the atom to operate on. Also, abstraction and normalization operations treat the anchor
    specially and maintain more information on it than other arguments. Therefore when attaching an
    attribute to an expression, that expression should be the first argument, optionally followed by
    additional related expressions. *)
type t =
  | Aresource of res_action  (** resource acquire/release *)
  | Aautorelease
  | Adangling of dangling_kind  (** dangling pointer *)
  | Aundef of Procname.t * Annot.Item.t * Location.t * path_pos
  | Alocked
  | Aunlocked
  | Adiv0 of path_pos  (** value appeared in second argument of division at given path position *)
  | Aobjc_null
      (** attributed exp is null due to a call to a method with given path as null receiver *)
  | Aretval of Procname.t * Annot.Item.t
      (** value was returned from a call to the given procedure, plus the annots of the return value *)
  | Aobserver  (** denotes an object registered as an observers to a notification center *)
  | Aunsubscribed_observer
      (** denotes an object unsubscribed from observers of a notification center *)
  | Awont_leak  (** value do not participate in memory leak analysis *)
[@@deriving compare]

val equal : t -> t -> bool

val mem_alloc_pname : mem_kind -> Procname.t
(** name of the allocation function for the given memory kind *)

val mem_dealloc_pname : mem_kind -> Procname.t
(** name of the deallocation function for the given memory kind *)

(** Categories of attributes *)
type category =
  | ACresource
  | ACautorelease
  | AClock
  | ACdiv0
  | ACobjc_null
  | ACundef
  | ACretval
  | ACobserver
  | ACwontleak
[@@deriving compare]

val equal_category : category -> category -> bool

val to_category : t -> category
(** Return the category to which the attribute belongs. *)

val is_undef : t -> bool

val to_string : Pp.env -> t -> string
(** convert the attribute to a string *)

val d_attribute : t -> unit
(** Dump an attribute. *)
