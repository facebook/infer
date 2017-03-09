/*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! IStd;


/** The Smallfoot Intermediate Language: Predicate Symbols */
let module L = Logging;

let module F = Format;


/** {2 Programs and Types} */
type func_attribute =
  | FA_sentinel int int
[@@deriving compare];


/** Return the value of the FA_sentinel attribute in [attr_list] if it is found */
let get_sentinel_func_attribute_value: list func_attribute => option (int, int);


/** Visibility modifiers. */
type access =
  | Default
  | Public
  | Private
  | Protected
[@@deriving compare];

let equal_access: access => access => bool;

type mem_kind =
  | Mmalloc /** memory allocated with malloc */
  | Mnew /** memory allocated with new */
  | Mnew_array /** memory allocated with new[] */
  | Mobjc /** memory allocated with objective-c alloc */
[@@deriving compare];


/** resource that can be allocated */
type resource =
  | Rmemory mem_kind
  | Rfile
  | Rignore
  | Rlock
[@@deriving compare];


/** kind of resource action */
type res_act_kind =
  | Racquire
  | Rrelease
[@@deriving compare];

let equal_res_act_kind: res_act_kind => res_act_kind => bool;


/** kind of dangling pointers */
type dangling_kind =
  /** pointer is dangling because it is uninitialized */
  | DAuninit
  /** pointer is dangling because it is the address of a stack variable which went out of scope */
  | DAaddr_stack_var
  /** pointer is -1 */
  | DAminusone;


/** position in a path: proc name, node id */
type path_pos = (Typ.Procname.t, int) [@@deriving compare];

let equal_path_pos: path_pos => path_pos => bool;

type taint_kind =
  | Tk_unverified_SSL_socket
  | Tk_shared_preferences_data
  | Tk_privacy_annotation
  | Tk_integrity_annotation
  | Tk_unknown;

type taint_info = {taint_source: Typ.Procname.t, taint_kind: taint_kind};


/** acquire/release action on a resource */
type res_action = {
  ra_kind: res_act_kind, /** kind of action */
  ra_res: resource, /** kind of resource */
  ra_pname: Typ.Procname.t, /** name of the procedure used to acquire/release the resource */
  ra_loc: Location.t, /** location of the acquire/release */
  ra_vpath: DecompiledExp.vpath /** vpath of the resource value */
};


/** Attributes are nary function symbols that are applied to expression arguments in Apred and
    Anpred atomic formulas.  Many operations don't make much sense for nullary predicates, and are
    generally treated as no-ops.  The first argument is treated specially, as the "anchor" of the
    predicate application.  For example, adding or removing an attribute uses the anchor to identify
    the atom to operate on.  Also, abstraction and normalization operations treat the anchor
    specially and maintain more information on it than other arguments.  Therefore when attaching an
    attribute to an expression, that expression should be the first argument, optionally followed by
    additional related expressions. */
type t =
  | Aresource res_action /** resource acquire/release */
  | Aautorelease
  | Adangling dangling_kind /** dangling pointer */
  /** undefined value obtained by calling the given procedure, plus its return value annots */
  | Aundef Typ.Procname.t Annot.Item.t Location.t path_pos
  | Ataint taint_info
  | Auntaint taint_info
  | Alocked
  | Aunlocked
  /** value appeared in second argument of division at given path position */
  | Adiv0 path_pos
  /** attributed exp is null due to a call to a method with given path as null receiver */
  | Aobjc_null
  /** value was returned from a call to the given procedure, plus the annots of the return value */
  | Aretval Typ.Procname.t Annot.Item.t
  /** denotes an object registered as an observers to a notification center */
  | Aobserver
  /** denotes an object unsubscribed from observers of a notification center */
  | Aunsubscribed_observer
[@@deriving compare];

let equal: t => t => bool;


/** name of the allocation function for the given memory kind */
let mem_alloc_pname: mem_kind => Typ.Procname.t;


/** name of the deallocation function for the given memory kind */
let mem_dealloc_pname: mem_kind => Typ.Procname.t;


/** Categories of attributes */
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
[@@deriving compare];

let equal_category: category => category => bool;


/**  Return the category to which the attribute belongs. */
let to_category: t => category;

let is_undef: t => bool;


/** convert the attribute to a string */
let to_string: Pp.env => t => string;


/** Dump an attribute. */
let d_attribute: t => unit;
