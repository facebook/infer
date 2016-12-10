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
open! IStd;


/** The Smallfoot Intermediate Language: Subtypes */
let module L = Logging;

let module F = Format;

let list_to_string list => {
  let rec aux list =>
    switch list {
    | [] => ""
    | [el, ...rest] =>
      let s = aux rest;
      if (s == "") {
        Typename.name el
      } else {
        Typename.name el ^ ", " ^ s
      }
    };
  if (IList.length list == 0) {
    "( sub )"
  } else {
    "- {" ^ aux list ^ "}"
  }
};

type t' =
  | Exact /** denotes the current type only */
  | Subtypes (list Typename.t)
[@@deriving compare];

let equal_modulo_flag (st1, _) (st2, _) => compare_t' st1 st2 == 0;


/** denotes the current type and a list of types that are not their subtypes  */
type kind =
  | CAST
  | INSTOF
  | NORMAL
[@@deriving compare];

type t = (t', kind) [@@deriving compare];

let module SubtypesPair = {
  type t = (Typename.t, Typename.t) [@@deriving compare];
};

let module SubtypesMap = Caml.Map.Make SubtypesPair;

type subtMap = SubtypesMap.t bool;

let subtMap: ref subtMap = ref SubtypesMap.empty;

let is_interface tenv (class_name: Typename.t) =>
  switch (class_name, Tenv.lookup tenv class_name) {
  | (TN_csu (Class Java) _, Some {fields: [], methods: []}) => true
  | _ => false
  };

let is_root_class class_name =>
  switch class_name {
  | Typename.TN_csu (Csu.Class Csu.Java) _ =>
    Typename.equal class_name Typename.Java.java_lang_object
  | Typename.TN_csu (Csu.Class Csu.CPP) _ => false
  | _ => false
  };


/** check if c1 is a subclass of c2 */
let check_subclass_tenv tenv c1 c2 => {
  let rec check (cn: Typename.t) =>
    Typename.equal cn c2 ||
    is_root_class c2 || (
      switch (cn, Tenv.lookup tenv cn) {
      | (TN_csu (Class _) _, Some {supers}) => IList.exists check supers
      | _ => false
      }
    );
  check c1
};

let check_subtype tenv c1 c2 =>
  try (SubtypesMap.find (c1, c2) !subtMap) {
  | Not_found =>
    let is_subt = check_subclass_tenv tenv c1 c2;
    subtMap := SubtypesMap.add (c1, c2) is_subt !subtMap;
    is_subt
  };

let flag_to_string flag =>
  switch flag {
  | CAST => "(cast)"
  | INSTOF => "(instof)"
  | NORMAL => ""
  };

let pp f (t, flag) =>
  if Config.print_types {
    switch t {
    | Exact => F.fprintf f "%s" (flag_to_string flag)
    | Subtypes list => F.fprintf f "%s" (list_to_string list ^ flag_to_string flag)
    }
  };

let exact = (Exact, NORMAL);

let all_subtypes = Subtypes [];

let subtypes = (all_subtypes, NORMAL);

let subtypes_cast = (all_subtypes, CAST);

let subtypes_instof = (all_subtypes, INSTOF);

let is_cast t => snd t == CAST;

let is_instof t => snd t == INSTOF;

let list_intersect equal l1 l2 => {
  let in_l2 a => IList.mem equal a l2;
  IList.filter in_l2 l1
};

let join_flag flag1 flag2 =>
  switch (flag1, flag2) {
  | (CAST, _) => CAST
  | (_, CAST) => CAST
  | (_, _) => NORMAL
  };

let join (s1, flag1) (s2, flag2) => {
  let s =
    switch (s1, s2) {
    | (Exact, _) => s2
    | (_, Exact) => s1
    | (Subtypes l1, Subtypes l2) => Subtypes (list_intersect Typename.equal l1 l2)
    };
  let flag = join_flag flag1 flag2;
  (s, flag)
};

let update_flag c1 c2 flag flag' =>
  switch flag {
  | INSTOF =>
    if (Typename.equal c1 c2) {
      flag
    } else {
      flag'
    }
  | _ => flag'
  };

let change_flag st_opt c1 c2 flag' =>
  switch st_opt {
  | Some st =>
    switch st {
    | (Exact, flag) =>
      let new_flag = update_flag c1 c2 flag flag';
      Some (Exact, new_flag)
    | (Subtypes t, flag) =>
      let new_flag = update_flag c1 c2 flag flag';
      Some (Subtypes t, new_flag)
    }
  | None => None
  };

let normalize_subtypes t_opt c1 c2 flag1 flag2 => {
  let new_flag = update_flag c1 c2 flag1 flag2;
  switch t_opt {
  | Some t =>
    switch t {
    | Exact => Some (t, new_flag)
    | Subtypes l => Some (Subtypes (IList.sort Typename.compare l), new_flag)
    }
  | None => None
  }
};

let subtypes_to_string t =>
  switch (fst t) {
  | Exact => "ex" ^ flag_to_string (snd t)
  | Subtypes l => list_to_string l ^ flag_to_string (snd t)
  };

/* c is a subtype when it does not appear in the list l of no-subtypes */
let is_subtype tenv c l =>
  try {
    ignore (IList.find (check_subtype tenv c) l);
    false
  } {
  | Not_found => true
  };

let is_strict_subtype tenv c1 c2 => check_subtype tenv c1 c2 && not (Typename.equal c1 c2);

/* checks for redundancies when adding c to l
   Xi in A - { X1,..., Xn } is redundant in two cases:
   1) not (Xi <: A) because removing the subtypes of Xi has no effect unless Xi is a subtype of A
   2) Xi <: Xj because the subtypes of Xi are a subset of the subtypes of Xj */
let check_redundancies tenv c l => {
  let aux (l, add) ci => {
    let (l, should_add) =
      if (check_subtype tenv ci c) {
        (l, true)
      } else if (check_subtype tenv c ci) {
        ([ci, ...l], false)
      } else {
        ([ci, ...l], true)
      };
    (l, add && should_add)
  };
  IList.fold_left aux ([], true) l
};

let rec updates_head f c l =>
  switch l {
  | [] => []
  | [ci, ...rest] =>
    if (is_strict_subtype f ci c) {
      [ci, ...updates_head f c rest]
    } else {
      updates_head f c rest
    }
  };

/* adds the classes of l2 to l1 and checks that no redundancies or inconsistencies will occur
   A - { X1,..., Xn } is inconsistent if A <: Xi for some i */
let rec add_not_subtype tenv c1 l1 l2 =>
  switch l2 {
  | [] => l1
  | [c, ...rest] =>
    if (check_subtype tenv c1 c) {
      add_not_subtype tenv c1 l1 rest
    } else {
      /* checks for inconsistencies */
      let (l1', should_add) = check_redundancies tenv c l1; /* checks for redundancies */
      let rest' = add_not_subtype tenv c1 l1' rest;
      if should_add {
        [c, ...rest']
      } else {
        rest'
      }
    }
  };

let get_subtypes tenv (c1, (st1, flag1): t) (c2, (st2, flag2): t) => {
  let is_sub = check_subtype tenv c1 c2;
  let (pos_st, neg_st) =
    switch (st1, st2) {
    | (Exact, Exact) =>
      if is_sub {
        (Some st1, None)
      } else {
        (None, Some st1)
      }
    | (Exact, Subtypes l2) =>
      if (is_sub && is_subtype tenv c1 l2) {
        (Some st1, None)
      } else {
        (None, Some st1)
      }
    | (Subtypes l1, Exact) =>
      if is_sub {
        (Some st1, None)
      } else {
        let l1' = updates_head tenv c2 l1;
        if (is_subtype tenv c2 l1) {
          (Some (Subtypes l1'), Some (Subtypes (add_not_subtype tenv c1 l1 [c2])))
        } else {
          (None, Some st1)
        }
      }
    | (Subtypes l1, Subtypes l2) =>
      if (is_interface tenv c2 || is_sub) {
        if (is_subtype tenv c1 l2) {
          let l2' = updates_head tenv c1 l2;
          (Some (Subtypes (add_not_subtype tenv c1 l1 l2')), None)
        } else {
          (None, Some st1)
        }
      } else if (
        (is_interface tenv c1 || check_subtype tenv c2 c1) && is_subtype tenv c2 l1
      ) {
        let l1' = updates_head tenv c2 l1;
        (
          Some (Subtypes (add_not_subtype tenv c2 l1' l2)),
          Some (Subtypes (add_not_subtype tenv c1 l1 [c2]))
        )
      } else {
        (None, Some st1)
      }
    };
  (normalize_subtypes pos_st c1 c2 flag1 flag2, normalize_subtypes neg_st c1 c2 flag1 flag2)
};

let case_analysis_basic tenv (c1, st) (c2, (_, flag2)) => {
  let (pos_st, neg_st) =
    if (check_subtype tenv c1 c2) {
      (Some st, None)
    } else if (check_subtype tenv c2 c1) {
      switch st {
      | (Exact, _) =>
        if (Typename.equal c1 c2) {
          (Some st, None)
        } else {
          (None, Some st)
        }
      | (Subtypes _, _) =>
        if (Typename.equal c1 c2) {
          (Some st, None)
        } else {
          (Some st, Some st)
        }
      }
    } else {
      (None, Some st)
    };
  (change_flag pos_st c1 c2 flag2, change_flag neg_st c1 c2 flag2)
};


/** [case_analysis (c1, st1) (c2, st2) f] performs case analysis on [c1 <: c2]
    according to [st1] and [st2]
    where f c1 c2 is true if c1 is a subtype of c2.
    get_subtypes returning a pair:
    - whether [st1] and [st2] admit [c1 <: c2], and in case return the updated subtype [st1]
    - whether [st1] and [st2] admit [not(c1 <: c2)],
    and in case return the updated subtype [st1] */
let case_analysis tenv (c1, st1) (c2, st2) =>
  if Config.subtype_multirange {
    get_subtypes tenv (c1, st1) (c2, st2)
  } else {
    case_analysis_basic tenv (c1, st1) (c2, st2)
  };
