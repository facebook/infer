/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 *
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open! Utils;


/** The Smallfoot Intermediate Language: Predicate Symbols */
let module L = Logging;

let module F = Format;

type func_attribute =
  | FA_sentinel of int int /** __attribute__((sentinel(int, int))) */;


/** Visibility modifiers. */
type access = | Default | Public | Private | Protected;


/** Return the value of the FA_sentinel attribute in [attr_list] if it is found */
let get_sentinel_func_attribute_value attr_list =>
  switch attr_list {
  | [FA_sentinel sentinel null_pos, ..._] => Some (sentinel, null_pos)
  | [] => None
  };

type mem_kind =
  | Mmalloc /** memory allocated with malloc */
  | Mnew /** memory allocated with new */
  | Mnew_array /** memory allocated with new[] */
  | Mobjc /** memory allocated with objective-c alloc */;

let mem_kind_to_num =
  fun
  | Mmalloc => 0
  | Mnew => 1
  | Mnew_array => 2
  | Mobjc => 3;

let mem_kind_compare mk1 mk2 => int_compare (mem_kind_to_num mk1) (mem_kind_to_num mk2);


/** resource that can be allocated */
type resource = | Rmemory of mem_kind | Rfile | Rignore | Rlock;

let resource_compare r1 r2 => {
  let res_to_num =
    fun
    | Rmemory mk => mem_kind_to_num mk
    | Rfile => 100
    | Rignore => 200
    | Rlock => 300;
  int_compare (res_to_num r1) (res_to_num r2)
};


/** kind of resource action */
type res_act_kind = | Racquire | Rrelease;

let res_act_kind_compare rak1 rak2 =>
  switch (rak1, rak2) {
  | (Racquire, Racquire) => 0
  | (Racquire, Rrelease) => (-1)
  | (Rrelease, Racquire) => 1
  | (Rrelease, Rrelease) => 0
  };


/** kind of dangling pointers */
type dangling_kind =
  /** pointer is dangling because it is uninitialized */
  | DAuninit
  /** pointer is dangling because it is the address
      of a stack variable which went out of scope */
  | DAaddr_stack_var
  /** pointer is -1 */
  | DAminusone;

let dangling_kind_compare dk1 dk2 =>
  switch (dk1, dk2) {
  | (DAuninit, DAuninit) => 0
  | (DAuninit, _) => (-1)
  | (_, DAuninit) => 1
  | (DAaddr_stack_var, DAaddr_stack_var) => 0
  | (DAaddr_stack_var, _) => (-1)
  | (_, DAaddr_stack_var) => 1
  | (DAminusone, DAminusone) => 0
  };


/** position in a path: proc name, node id */
type path_pos = (Procname.t, int);

let path_pos_compare (pn1, nid1) (pn2, nid2) => {
  let n = Procname.compare pn1 pn2;
  if (n != 0) {
    n
  } else {
    int_compare nid1 nid2
  }
};

let path_pos_equal pp1 pp2 => path_pos_compare pp1 pp2 == 0;

type taint_kind =
  | Tk_unverified_SSL_socket
  | Tk_shared_preferences_data
  | Tk_privacy_annotation
  | Tk_integrity_annotation
  | Tk_unknown;

let taint_kind_compare tk1 tk2 =>
  switch (tk1, tk2) {
  | (Tk_unverified_SSL_socket, Tk_unverified_SSL_socket) => 0
  | (Tk_unverified_SSL_socket, _) => (-1)
  | (_, Tk_unverified_SSL_socket) => 1
  | (Tk_shared_preferences_data, Tk_shared_preferences_data) => 0
  | (Tk_shared_preferences_data, _) => 1
  | (_, Tk_shared_preferences_data) => (-1)
  | (Tk_privacy_annotation, Tk_privacy_annotation) => 0
  | (Tk_privacy_annotation, _) => 1
  | (_, Tk_privacy_annotation) => (-1)
  | (Tk_integrity_annotation, Tk_integrity_annotation) => 0
  | (Tk_integrity_annotation, _) => 1
  | (_, Tk_integrity_annotation) => (-1)
  | (Tk_unknown, Tk_unknown) => 0
  };

type taint_info = {taint_source: Procname.t, taint_kind: taint_kind};

let taint_info_compare {taint_source: ts1, taint_kind: tk1} {taint_source: ts2, taint_kind: tk2} =>
  taint_kind_compare tk1 tk2 |> next Procname.compare ts1 ts2;


/** acquire/release action on a resource */
type res_action = {
  ra_kind: res_act_kind, /** kind of action */
  ra_res: resource, /** kind of resource */
  ra_pname: Procname.t, /** name of the procedure used to acquire/release the resource */
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
  | Aresource of res_action /** resource acquire/release */
  | Aautorelease
  | Adangling of dangling_kind /** dangling pointer */
  /** undefined value obtained by calling the given procedure, plus its return value annots */
  | Aundef of Procname.t Typ.item_annotation Location.t path_pos
  | Ataint of taint_info
  | Auntaint of taint_info
  | Alocked
  | Aunlocked
  /** value appeared in second argument of division at given path position */
  | Adiv0 of path_pos
  /** attributed exp is null due to a call to a method with given path as null receiver */
  | Aobjc_null
  /** value was returned from a call to the given procedure, plus the annots of the return value */
  | Aretval of Procname.t Typ.item_annotation
  /** denotes an object registered as an observers to a notification center */
  | Aobserver
  /** denotes an object unsubscribed from observers of a notification center */
  | Aunsubscribed_observer;

let compare (att1: t) (att2: t) :int =>
  switch (att1, att2) {
  | (Aresource ra1, Aresource ra2) =>
    let n = res_act_kind_compare ra1.ra_kind ra2.ra_kind;
    if (n != 0) {
      n
    } else {
      /* ignore other values beside resources: arbitrary merging into one */
      resource_compare
        ra1.ra_res ra2.ra_res
    }
  | (Aresource _, _) => (-1)
  | (_, Aresource _) => 1
  | (Aautorelease, Aautorelease) => 0
  | (Aautorelease, _) => (-1)
  | (_, Aautorelease) => 1
  | (Adangling dk1, Adangling dk2) => dangling_kind_compare dk1 dk2
  | (Adangling _, _) => (-1)
  | (_, Adangling _) => 1
  | (Aundef pn1 _ _ _, Aundef pn2 _ _ _) => Procname.compare pn1 pn2
  | (Ataint ti1, Ataint ti2) => taint_info_compare ti1 ti2
  | (Ataint _, _) => (-1)
  | (_, Ataint _) => 1
  | (Auntaint ti1, Auntaint ti2) => taint_info_compare ti1 ti2
  | (Auntaint _, _) => (-1)
  | (_, Auntaint _) => 1
  | (Alocked, Alocked) => 0
  | (Alocked, _) => (-1)
  | (_, Alocked) => 1
  | (Aunlocked, Aunlocked) => 0
  | (Aunlocked, _) => (-1)
  | (_, Aunlocked) => 1
  | (Adiv0 pp1, Adiv0 pp2) => path_pos_compare pp1 pp2
  | (Adiv0 _, _) => (-1)
  | (_, Adiv0 _) => 1
  | (Aobjc_null, Aobjc_null) => 0
  | (Aobjc_null, _) => (-1)
  | (_, Aobjc_null) => 1
  | (Aretval pn1 annots1, Aretval pn2 annots2) =>
    let n = Procname.compare pn1 pn2;
    if (n != 0) {
      n
    } else {
      Typ.item_annotation_compare annots1 annots2
    }
  | (Aretval _, _) => (-1)
  | (_, Aretval _) => 1
  | (Aobserver, Aobserver) => 0
  | (Aobserver, _) => (-1)
  | (_, Aobserver) => 1
  | (Aunsubscribed_observer, Aunsubscribed_observer) => 0
  | (Aunsubscribed_observer, _) => (-1)
  | (_, Aunsubscribed_observer) => 1
  };

let equal att1 att2 => compare att1 att2 == 0;


/** name of the allocation function for the given memory kind */
let mem_alloc_pname =
  fun
  | Mmalloc => Procname.from_string_c_fun "malloc"
  | Mnew => Procname.from_string_c_fun "new"
  | Mnew_array => Procname.from_string_c_fun "new[]"
  | Mobjc => Procname.from_string_c_fun "alloc";


/** name of the deallocation function for the given memory kind */
let mem_dealloc_pname =
  fun
  | Mmalloc => Procname.from_string_c_fun "free"
  | Mnew => Procname.from_string_c_fun "delete"
  | Mnew_array => Procname.from_string_c_fun "delete[]"
  | Mobjc => Procname.from_string_c_fun "dealloc";


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
  | ACobserver;

let category_compare (ac1: category) (ac2: category) :int => Pervasives.compare ac1 ac2;

let category_equal att1 att2 => category_compare att1 att2 == 0;

let to_category att =>
  switch att {
  | Aresource _
  | Adangling _ => ACresource
  | Ataint _
  | Auntaint _ => ACtaint
  | Alocked
  | Aunlocked => AClock
  | Aautorelease => ACautorelease
  | Adiv0 _ => ACdiv0
  | Aobjc_null => ACobjc_null
  | Aretval _ => ACretval
  | Aundef _ => ACundef
  | Aobserver
  | Aunsubscribed_observer => ACobserver
  };

let is_undef =
  fun
  | Aundef _ => true
  | _ => false;


/** convert the attribute to a string */
let to_string pe =>
  fun
  | Aresource ra => {
      let mk_name = (
        fun
        | Mmalloc => "ma"
        | Mnew => "ne"
        | Mnew_array => "na"
        | Mobjc => "oc"
      );
      let name =
        switch (ra.ra_kind, ra.ra_res) {
        | (Racquire, Rmemory mk) => "MEM" ^ mk_name mk
        | (Racquire, Rfile) => "FILE"
        | (Rrelease, Rmemory mk) => "FREED" ^ mk_name mk
        | (Rrelease, Rfile) => "CLOSED"
        | (_, Rignore) => "IGNORE"
        | (Racquire, Rlock) => "LOCKED"
        | (Rrelease, Rlock) => "UNLOCKED"
        };
      let str_vpath =
        if Config.trace_error {
          pp_to_string (DecompiledExp.pp_vpath pe) ra.ra_vpath
        } else {
          ""
        };
      name ^
        Binop.str pe Lt ^
        Procname.to_string ra.ra_pname ^
        ":" ^
        string_of_int ra.ra_loc.Location.line ^
        Binop.str pe Gt ^
        str_vpath
    }
  | Aautorelease => "AUTORELEASE"
  | Adangling dk => {
      let dks =
        switch dk {
        | DAuninit => "UNINIT"
        | DAaddr_stack_var => "ADDR_STACK"
        | DAminusone => "MINUS1"
        };
      "DANGL" ^ Binop.str pe Lt ^ dks ^ Binop.str pe Gt
    }
  | Aundef pn _ loc _ =>
    "UND" ^
      Binop.str pe Lt ^
      Procname.to_string pn ^
      Binop.str pe Gt ^
      ":" ^
      string_of_int loc.Location.line
  | Ataint {taint_source} => "TAINTED[" ^ Procname.to_string taint_source ^ "]"
  | Auntaint _ => "UNTAINTED"
  | Alocked => "LOCKED"
  | Aunlocked => "UNLOCKED"
  | Adiv0 (_, _) => "DIV0"
  | Aobjc_null => "OBJC_NULL"
  | Aretval pn _ => "RET" ^ Binop.str pe Lt ^ Procname.to_string pn ^ Binop.str pe Gt
  | Aobserver => "OBSERVER"
  | Aunsubscribed_observer => "UNSUBSCRIBED_OBSERVER";


/** dump an attribute */
let d_attribute (a: t) => L.add_print_action (L.PTattribute, Obj.repr a);
