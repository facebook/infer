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


/** The Smallfoot Intermediate Language */
let module L = Logging;

let module F = Format;


/** {2 Programs and Types} */
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


/** Unary operations */
type unop =
  | Neg /** Unary minus */
  | BNot /** Bitwise complement (~) */
  | LNot /** Logical Not (!) */;


/** Binary operations */
type binop =
  | PlusA /** arithmetic + */
  | PlusPI /** pointer + integer */
  | MinusA /** arithmetic - */
  | MinusPI /** pointer - integer */
  | MinusPP /** pointer - pointer */
  | Mult /** * */
  | Div /** / */
  | Mod /** % */
  | Shiftlt /** shift left */
  | Shiftrt /** shift right */
  | Lt /** <  (arithmetic comparison) */
  | Gt /** >  (arithmetic comparison) */
  | Le /** <= (arithmetic comparison) */
  | Ge /** >  (arithmetic comparison) */
  | Eq /** == (arithmetic comparison) */
  | Ne /** != (arithmetic comparison) */
  | BAnd /** bitwise and */
  | BXor /** exclusive-or */
  | BOr /** inclusive-or */
  | LAnd /** logical and. Does not always evaluate both operands. */
  | LOr /** logical or. Does not always evaluate both operands. */
  | PtrFld /** field offset via pointer to field: takes the address of a
               Csu.t and a Cptr_to_fld constant to form an Lfield expression (see prop.ml) */;

type mem_kind =
  | Mmalloc /** memory allocated with malloc */
  | Mnew /** memory allocated with new */
  | Mnew_array /** memory allocated with new[] */
  | Mobjc /** memory allocated with objective-c alloc */;


/** resource that can be allocated */
type resource = | Rmemory of mem_kind | Rfile | Rignore | Rlock;


/** kind of resource action */
type res_act_kind = | Racquire | Rrelease;


/** kind of dangling pointers */
type dangling_kind =
  /** pointer is dangling because it is uninitialized */
  | DAuninit
  /** pointer is dangling because it is the address
      of a stack variable which went out of scope */
  | DAaddr_stack_var
  /** pointer is -1 */
  | DAminusone;


/** position in a path: proc name, node id */
type path_pos = (Procname.t, int);


/** module for subtypes, to be used with Sizeof info */
let module Subtype = {
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
    | Subtypes of (list Typename.t);

  /** denotes the current type and a list of types that are not their subtypes  */
  type kind = | CAST | INSTOF | NORMAL;
  type t = (t', kind);
  let module SubtypesPair = {
    type t = (Typename.t, Typename.t);
    let compare (e1: t) (e2: t) :int => pair_compare Typename.compare Typename.compare e1 e2;
  };
  let module SubtypesMap = Map.Make SubtypesPair;
  type subtMap = SubtypesMap.t bool;
  let subtMap: ref subtMap = ref SubtypesMap.empty;
  let check_subtype f c1 c2 =>
    try (SubtypesMap.find (c1, c2) !subtMap) {
    | Not_found =>
      let is_subt = f c1 c2;
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
  let subtypes_compare l1 l2 => IList.compare Typename.compare l1 l2;
  let compare_flag flag1 flag2 =>
    switch (flag1, flag2) {
    | (CAST, CAST) => 0
    | (INSTOF, INSTOF) => 0
    | (NORMAL, NORMAL) => 0
    | (CAST, _) => (-1)
    | (_, CAST) => 1
    | (INSTOF, NORMAL) => (-1)
    | (NORMAL, INSTOF) => 1
    };
  let compare_subt s1 s2 =>
    switch (s1, s2) {
    | (Exact, Exact) => 0
    | (Exact, _) => (-1)
    | (_, Exact) => 1
    | (Subtypes l1, Subtypes l2) => subtypes_compare l1 l2
    };
  let compare t1 t2 => pair_compare compare_subt compare_flag t1 t2;
  let equal_modulo_flag (st1, _) (st2, _) => compare_subt st1 st2 == 0;
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
  let is_subtype f c l =>
    try {
      ignore (IList.find (f c) l);
      false
    } {
    | Not_found => true
    };
  let is_strict_subtype f c1 c2 => f c1 c2 && not (Typename.equal c1 c2);
  /* checks for redundancies when adding c to l
     Xi in A - { X1,..., Xn } is redundant in two cases:
     1) not (Xi <: A) because removing the subtypes of Xi has no effect unless Xi is a subtype of A
     2) Xi <: Xj because the subtypes of Xi are a subset of the subtypes of Xj */
  let check_redundancies f c l => {
    let aux (l, add) ci => {
      let (l, should_add) =
        if (f ci c) {
          (l, true)
        } else if (f c ci) {
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
  let rec add_not_subtype f c1 l1 l2 =>
    switch l2 {
    | [] => l1
    | [c, ...rest] =>
      if (f c1 c) {
        add_not_subtype f c1 l1 rest
      } else {
        /* checks for inconsistencies */
        let (l1', should_add) = check_redundancies f c l1; /* checks for redundancies */
        let rest' = add_not_subtype f c1 l1' rest;
        if should_add {
          [c, ...rest']
        } else {
          rest'
        }
      }
    };
  let get_subtypes (c1, (st1, flag1)) (c2, (st2, flag2)) f is_interface => {
    let is_sub = f c1 c2;
    let (pos_st, neg_st) =
      switch (st1, st2) {
      | (Exact, Exact) =>
        if is_sub {
          (Some st1, None)
        } else {
          (None, Some st1)
        }
      | (Exact, Subtypes l2) =>
        if (is_sub && is_subtype f c1 l2) {
          (Some st1, None)
        } else {
          (None, Some st1)
        }
      | (Subtypes l1, Exact) =>
        if is_sub {
          (Some st1, None)
        } else {
          let l1' = updates_head f c2 l1;
          if (is_subtype f c2 l1) {
            (Some (Subtypes l1'), Some (Subtypes (add_not_subtype f c1 l1 [c2])))
          } else {
            (None, Some st1)
          }
        }
      | (Subtypes l1, Subtypes l2) =>
        if (is_interface c2 || is_sub) {
          if (is_subtype f c1 l2) {
            let l2' = updates_head f c1 l2;
            (Some (Subtypes (add_not_subtype f c1 l1 l2')), None)
          } else {
            (None, Some st1)
          }
        } else if (
          (is_interface c1 || f c2 c1) && is_subtype f c2 l1
        ) {
          let l1' = updates_head f c2 l1;
          (
            Some (Subtypes (add_not_subtype f c2 l1' l2)),
            Some (Subtypes (add_not_subtype f c1 l1 [c2]))
          )
        } else {
          (None, Some st1)
        }
      };
    (normalize_subtypes pos_st c1 c2 flag1 flag2, normalize_subtypes neg_st c1 c2 flag1 flag2)
  };
  let case_analysis_basic (c1, st) (c2, (_, flag2)) f => {
    let (pos_st, neg_st) =
      if (f c1 c2) {
        (Some st, None)
      } else if (f c2 c1) {
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

  /** [case_analysis (c1, st1) (c2,st2) f] performs case analysis on [c1 <: c2]
      according to [st1] and [st2]
      where f c1 c2 is true if c1 is a subtype of c2.
      get_subtypes returning a pair:
      - whether [st1] and [st2] admit [c1 <: c2], and in case return the updated subtype [st1]
      - whether [st1] and [st2] admit [not(c1 <: c2)],
      and in case return the updated subtype [st1] */
  let case_analysis (c1, st1) (c2, st2) f is_interface => {
    let f = check_subtype f;
    if Config.subtype_multirange {
      get_subtypes (c1, st1) (c2, st2) f is_interface
    } else {
      case_analysis_basic (c1, st1) (c2, st2) f
    }
  };
};


/** Flags for a procedure call */
type call_flags = {
  cf_virtual: bool,
  cf_interface: bool,
  cf_noreturn: bool,
  cf_is_objc_block: bool,
  cf_targets: list Procname.t
};

let cf_default = {
  cf_virtual: false,
  cf_interface: false,
  cf_noreturn: false,
  cf_is_objc_block: false,
  cf_targets: []
};

type taint_kind =
  | Tk_unverified_SSL_socket
  | Tk_shared_preferences_data
  | Tk_privacy_annotation
  | Tk_integrity_annotation
  | Tk_unknown;

type taint_info = {taint_source: Procname.t, taint_kind: taint_kind};


/** expression representing the result of decompilation */
type dexp =
  | Darray of dexp dexp
  | Dbinop of binop dexp dexp
  | Dconst of const
  | Dsizeof of Typ.t (option dexp) Subtype.t
  | Dderef of dexp
  | Dfcall of dexp (list dexp) Location.t call_flags
  | Darrow of dexp Ident.fieldname
  | Ddot of dexp Ident.fieldname
  | Dpvar of Pvar.t
  | Dpvaraddr of Pvar.t
  | Dunop of unop dexp
  | Dunknown
  | Dretcall of dexp (list dexp) Location.t call_flags
/** Value paths: identify an occurrence of a value in a symbolic heap
    each expression represents a path, with Dpvar being the simplest one */
and vpath = option dexp
/** acquire/release action on a resource */
and res_action = {
  ra_kind: res_act_kind, /** kind of action */
  ra_res: resource, /** kind of resource */
  ra_pname: Procname.t, /** name of the procedure used to acquire/release the resource */
  ra_loc: Location.t, /** location of the acquire/release */
  ra_vpath: vpath /** vpath of the resource value */
}
/** Attributes */
and attribute =
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
  /** the exp. is null because of a call to a method with exp as a null receiver */
  | Aobjc_null of exp
  /** value was returned from a call to the given procedure, plus the annots of the return value */
  | Aretval of Procname.t Typ.item_annotation
  /** denotes an object registered as an observers to a notification center */
  | Aobserver
  /** denotes an object unsubscribed from observers of a notification center */
  | Aunsubscribed_observer
and closure = {name: Procname.t, captured_vars: list (exp, Pvar.t, Typ.t)}
/** Constants */
and const =
  | Cint of IntLit.t /** integer constants */
  | Cfun of Procname.t /** function names */
  | Cstr of string /** string constants */
  | Cfloat of float /** float constants */
  | Cattribute of attribute /** attribute used in disequalities to annotate a value */
  | Cclass of Ident.name /** class constant */
  | Cptr_to_fld of Ident.fieldname Typ.t /** pointer to field constant,
                                             and type of the surrounding Csu.t type */
/** dynamically determined length of an array value, if any */
and dynamic_length = option exp
/** Program expressions. */
and exp =
  /** Pure variable: it is not an lvalue */
  | Var of Ident.t
  /** Unary operator with type of the result if known */
  | UnOp of unop exp (option Typ.t)
  /** Binary operator */
  | BinOp of binop exp exp
  /** Exception */
  | Exn of exp
  /** Anonymous function */
  | Closure of closure
  /** Constants */
  | Const of const
  /** Type cast */
  | Cast of Typ.t exp
  /** The address of a program variable */
  | Lvar of Pvar.t
  /** A field offset, the type is the surrounding struct type */
  | Lfield of exp Ident.fieldname Typ.t
  /** An array index offset: [exp1\[exp2\]] */
  | Lindex of exp exp
  /** A sizeof expression. [Sizeof (Tarray elt (Some static_length)) (Some dynamic_length)]
      represents the size of an array value consisting of [dynamic_length] elements of type [elt].
      The [dynamic_length], tracked by symbolic execution, may differ from the [static_length]
      obtained from the type definition, e.g. when an array is over-allocated.  For struct types,
      the [dynamic_length] is that of the final extensible array, if any. */
  | Sizeof of Typ.t dynamic_length Subtype.t;


/** Kind of prune instruction */
type if_kind =
  | Ik_bexp /* boolean expressions, and exp ? exp : exp */
  | Ik_dowhile
  | Ik_for
  | Ik_if
  | Ik_land_lor /* obtained from translation of && or || */
  | Ik_while
  | Ik_switch;


/** Stack operation for symbolic execution on propsets */
type stackop =
  | Push /* copy the curreny propset to the stack */
  | Swap /* swap the current propset and the top of the stack */
  | Pop /* pop the stack and combine with the current propset */;


/** An instruction. */
type instr =
  /** declaration [let x = *lexp:typ] where [typ] is the root type of [lexp] */
  /* note for frontend writers: [x] must be used in a subsequent instruction, otherwise the entire
     `Letderef` instruction may be eliminated by copy-propagation */
  | Letderef of Ident.t exp Typ.t Location.t
  /** assignment [*lexp1:typ = exp2] where [typ] is the root type of [lexp1] */
  | Set of exp Typ.t exp Location.t
  /** prune the state based on [exp=1], the boolean indicates whether true branch */
  | Prune of exp Location.t bool if_kind
  /** [Call (ret_id1..ret_idn, e_fun, arg_ts, loc, call_flags)] represents an instructions
      [ret_id1..ret_idn = e_fun(arg_ts);]
      where n = 0 for void return and n > 1 for struct return */
  | Call of (list Ident.t) exp (list (exp, Typ.t)) Location.t call_flags
  /** nullify stack variable */
  | Nullify of Pvar.t Location.t
  | Abstract of Location.t /** apply abstraction */
  | Remove_temps of (list Ident.t) Location.t /** remove temporaries */
  | Stackop of stackop Location.t /** operation on the stack of propsets */
  | Declare_locals of (list (Pvar.t, Typ.t)) Location.t /** declare local variables */;


/** Check if an instruction is auxiliary, or if it comes from source instructions. */
let instr_is_auxiliary =
  fun
  | Letderef _
  | Set _
  | Prune _
  | Call _ => false
  | Nullify _
  | Abstract _
  | Remove_temps _
  | Stackop _
  | Declare_locals _ => true;


/** offset for an lvalue */
type offset = | Off_fld of Ident.fieldname Typ.t | Off_index of exp;


/** {2 Components of Propositions} */
/** an atom is a pure atomic formula */
type atom =
  | Aeq of exp exp /** equality */
  | Aneq of exp exp /** disequality*/;


/** kind of lseg or dllseg predicates */
type lseg_kind =
  | Lseg_NE /** nonempty (possibly circular) listseg */
  | Lseg_PE /** possibly empty (possibly circular) listseg */;


/** The boolean is true when the pointer was dereferenced without testing for zero. */
type zero_flag = option bool;


/** True when the value was obtained by doing case analysis on null in a procedure call. */
type null_case_flag = bool;


/** instrumentation of heap values */
type inst =
  | Iabstraction
  | Iactual_precondition
  | Ialloc
  | Iformal of zero_flag null_case_flag
  | Iinitial
  | Ilookup
  | Inone
  | Inullify
  | Irearrange of zero_flag null_case_flag int path_pos
  | Itaint
  | Iupdate of zero_flag null_case_flag int path_pos
  | Ireturn_from_call of int
  | Ireturn_from_pointer_wrapper_call of int;


/** structured expressions represent a value of structured type, such as an array or a struct. */
type strexp =
  | Eexp of exp inst /** Base case: expression with instrumentation */
  | Estruct of (list (Ident.fieldname, strexp)) inst /** C structure */
  | Earray of exp (list (exp, strexp)) inst /** Array of given length */
/** There are two conditions imposed / used in the array case.
    First, if some index and value pair appears inside an array
    in a strexp, then the index is less than the length of the array.
    For instance, x |->[10 | e1: v1] implies that e1 <= 9.
    Second, if two indices appear in an array, they should be different.
    For instance, x |->[10 | e1: v1, e2: v2] implies that e1 != e2. */
/** an atomic heap predicate */
and hpred =
  | Hpointsto of exp strexp exp
  /** represents [exp|->strexp:typexp] where [typexp]
      is an expression representing a type, e.h. [sizeof(t)]. */
  | Hlseg of lseg_kind hpara exp exp (list exp)
  /** higher - order predicate for singly - linked lists.
      Should ensure that exp1!= exp2 implies that exp1 is allocated.
      This assumption is used in the rearrangement. The last [exp list] parameter
      is used to denote the shared links by all the nodes in the list. */
  | Hdllseg of lseg_kind hpara_dll exp exp exp exp (list exp)
/** higher-order predicate for doubly-linked lists. */
/** parameter for the higher-order singly-linked list predicate.
    Means "lambda (root,next,svars). Exists evars. body".
    Assume that root, next, svars, evars are disjoint sets of
    primed identifiers, and include all the free primed identifiers in body.
    body should not contain any non - primed identifiers or program
    variables (i.e. pvars). */
and hpara = {
  root: Ident.t,
  next: Ident.t,
  svars: list Ident.t,
  evars: list Ident.t,
  body: list hpred
}
/** parameter for the higher-order doubly-linked list predicates.
    Assume that all the free identifiers in body_dll should belong to
    cell, blink, flink, svars_dll, evars_dll. */
and hpara_dll = {
  cell: Ident.t, /** address cell */
  blink: Ident.t, /** backward link */
  flink: Ident.t, /** forward link */
  svars_dll: list Ident.t,
  evars_dll: list Ident.t,
  body_dll: list hpred
};


/** Return the lhs expression of a hpred */
let hpred_get_lhs h =>
  switch h {
  | Hpointsto e _ _
  | Hlseg _ _ e _ _
  | Hdllseg _ _ e _ _ _ _ => e
  };


/** {2 Comparision and Inspection Functions} */
let has_objc_ref_counter hpred =>
  switch hpred {
  | Hpointsto _ _ (Sizeof (Tstruct struct_typ) _ _) =>
    IList.exists Typ.is_objc_ref_counter_field struct_typ.instance_fields
  | _ => false
  };


/** Returns the zero value of a type, for int, float and ptr types, None othwewise */
let zero_value_of_numerical_type_option typ =>
  switch typ {
  | Typ.Tint _ => Some (Const (Cint IntLit.zero))
  | Typ.Tfloat _ => Some (Const (Cfloat 0.0))
  | Typ.Tptr _ => Some (Const (Cint IntLit.null))
  | _ => None
  };


/** Returns the zero value of a type, for int, float and ptr types, fail otherwise */
let zero_value_of_numerical_type typ => Option.get (zero_value_of_numerical_type_option typ);


/** Make a static local name in objc */
let mk_static_local_name pname vname => pname ^ "_" ^ vname;


/** Check if a pvar is a local static in objc */
let is_static_local_name pname pvar =>
  /* local static name is of the form procname_varname */
  {
    let var_name = Mangled.to_string (Pvar.get_name pvar);
    switch (Str.split_delim (Str.regexp_string pname) var_name) {
    | [_, _] => true
    | _ => false
    }
  };

let exp_is_zero =
  fun
  | Const (Cint n) => IntLit.iszero n
  | _ => false;

let exp_is_null_literal =
  fun
  | Const (Cint n) => IntLit.isnull n
  | _ => false;

let exp_is_this =
  fun
  | Lvar pvar => Pvar.is_this pvar
  | _ => false;

let unop_compare o1 o2 =>
  switch (o1, o2) {
  | (Neg, Neg) => 0
  | (Neg, _) => (-1)
  | (_, Neg) => 1
  | (BNot, BNot) => 0
  | (BNot, _) => (-1)
  | (_, BNot) => 1
  | (LNot, LNot) => 0
  };

let unop_equal o1 o2 => unop_compare o1 o2 == 0;

let binop_compare o1 o2 =>
  switch (o1, o2) {
  | (PlusA, PlusA) => 0
  | (PlusA, _) => (-1)
  | (_, PlusA) => 1
  | (PlusPI, PlusPI) => 0
  | (PlusPI, _) => (-1)
  | (_, PlusPI) => 1
  | (MinusA, MinusA) => 0
  | (MinusA, _) => (-1)
  | (_, MinusA) => 1
  | (MinusPI, MinusPI) => 0
  | (MinusPI, _) => (-1)
  | (_, MinusPI) => 1
  | (MinusPP, MinusPP) => 0
  | (MinusPP, _) => (-1)
  | (_, MinusPP) => 1
  | (Mult, Mult) => 0
  | (Mult, _) => (-1)
  | (_, Mult) => 1
  | (Div, Div) => 0
  | (Div, _) => (-1)
  | (_, Div) => 1
  | (Mod, Mod) => 0
  | (Mod, _) => (-1)
  | (_, Mod) => 1
  | (Shiftlt, Shiftlt) => 0
  | (Shiftlt, _) => (-1)
  | (_, Shiftlt) => 1
  | (Shiftrt, Shiftrt) => 0
  | (Shiftrt, _) => (-1)
  | (_, Shiftrt) => 1
  | (Lt, Lt) => 0
  | (Lt, _) => (-1)
  | (_, Lt) => 1
  | (Gt, Gt) => 0
  | (Gt, _) => (-1)
  | (_, Gt) => 1
  | (Le, Le) => 0
  | (Le, _) => (-1)
  | (_, Le) => 1
  | (Ge, Ge) => 0
  | (Ge, _) => (-1)
  | (_, Ge) => 1
  | (Eq, Eq) => 0
  | (Eq, _) => (-1)
  | (_, Eq) => 1
  | (Ne, Ne) => 0
  | (Ne, _) => (-1)
  | (_, Ne) => 1
  | (BAnd, BAnd) => 0
  | (BAnd, _) => (-1)
  | (_, BAnd) => 1
  | (BXor, BXor) => 0
  | (BXor, _) => (-1)
  | (_, BXor) => 1
  | (BOr, BOr) => 0
  | (BOr, _) => (-1)
  | (_, BOr) => 1
  | (LAnd, LAnd) => 0
  | (LAnd, _) => (-1)
  | (_, LAnd) => 1
  | (LOr, LOr) => 0
  | (LOr, _) => (-1)
  | (_, LOr) => 1
  | (PtrFld, PtrFld) => 0
  };

let binop_equal o1 o2 => binop_compare o1 o2 == 0;


/** This function returns true if the operation is injective
    wrt. each argument: op(e,-) and op(-, e) is injective for all e.
    The return value false means "don't know". */
let binop_injective =
  fun
  | PlusA
  | PlusPI
  | MinusA
  | MinusPI
  | MinusPP => true
  | _ => false;


/** This function returns true if the operation can be inverted. */
let binop_invertible =
  fun
  | PlusA
  | PlusPI
  | MinusA
  | MinusPI => true
  | _ => false;


/** This function inverts an injective binary operator
    with respect to the first argument. It returns an expression [e'] such that
    BinOp([binop], [e'], [exp1]) = [exp2]. If the [binop] operation is not invertible,
    the function raises an exception by calling "assert false". */
let binop_invert bop e1 e2 => {
  let inverted_bop =
    switch bop {
    | PlusA => MinusA
    | PlusPI => MinusPI
    | MinusA => PlusA
    | MinusPI => PlusPI
    | _ => assert false
    };
  BinOp inverted_bop e2 e1
};


/** This function returns true if 0 is the right unit of [binop].
    The return value false means "don't know". */
let binop_is_zero_runit =
  fun
  | PlusA
  | PlusPI
  | MinusA
  | MinusPI
  | MinusPP => true
  | _ => false;

let path_pos_compare (pn1, nid1) (pn2, nid2) => {
  let n = Procname.compare pn1 pn2;
  if (n != 0) {
    n
  } else {
    int_compare nid1 nid2
  }
};

let path_pos_equal pp1 pp2 => path_pos_compare pp1 pp2 == 0;

let mem_kind_to_num =
  fun
  | Mmalloc => 0
  | Mnew => 1
  | Mnew_array => 2
  | Mobjc => 3;


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

let mem_kind_compare mk1 mk2 => int_compare (mem_kind_to_num mk1) (mem_kind_to_num mk2);

let resource_compare r1 r2 => {
  let res_to_num =
    fun
    | Rmemory mk => mem_kind_to_num mk
    | Rfile => 100
    | Rignore => 200
    | Rlock => 300;
  int_compare (res_to_num r1) (res_to_num r2)
};

let res_act_kind_compare rak1 rak2 =>
  switch (rak1, rak2) {
  | (Racquire, Racquire) => 0
  | (Racquire, Rrelease) => (-1)
  | (Rrelease, Racquire) => 1
  | (Rrelease, Rrelease) => 0
  };

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

let taint_info_compare {taint_source: ts1, taint_kind: tk1} {taint_source: ts2, taint_kind: tk2} =>
  taint_kind_compare tk1 tk2 |> next Procname.compare ts1 ts2;


/** Categories of attributes */
type attribute_category =
  | ACresource
  | ACautorelease
  | ACtaint
  | AClock
  | ACdiv0
  | ACobjc_null
  | ACundef
  | ACretval
  | ACobserver;

let attribute_category_compare (ac1: attribute_category) (ac2: attribute_category) :int =>
  Pervasives.compare ac1 ac2;

let attribute_category_equal att1 att2 => attribute_category_compare att1 att2 == 0;

let attribute_to_category att =>
  switch att {
  | Aresource _
  | Adangling _ => ACresource
  | Ataint _
  | Auntaint _ => ACtaint
  | Alocked
  | Aunlocked => AClock
  | Aautorelease => ACautorelease
  | Adiv0 _ => ACdiv0
  | Aobjc_null _ => ACobjc_null
  | Aretval _ => ACretval
  | Aundef _ => ACundef
  | Aobserver
  | Aunsubscribed_observer => ACobserver
  };

let attr_is_undef =
  fun
  | Aundef _ => true
  | _ => false;

let const_kind_equal c1 c2 => {
  let const_kind_number =
    fun
    | Cint _ => 1
    | Cfun _ => 2
    | Cstr _ => 3
    | Cfloat _ => 4
    | Cattribute _ => 5
    | Cclass _ => 7
    | Cptr_to_fld _ => 8;
  const_kind_number c1 == const_kind_number c2
};

let rec dexp_has_tmp_var =
  fun
  | Dpvar pvar
  | Dpvaraddr pvar => Pvar.is_frontend_tmp pvar
  | Dderef dexp
  | Ddot dexp _
  | Darrow dexp _
  | Dunop _ dexp
  | Dsizeof _ (Some dexp) _ => dexp_has_tmp_var dexp
  | Darray dexp1 dexp2
  | Dbinop _ dexp1 dexp2 => dexp_has_tmp_var dexp1 || dexp_has_tmp_var dexp2
  | Dretcall dexp dexp_list _ _
  | Dfcall dexp dexp_list _ _ => dexp_has_tmp_var dexp || IList.exists dexp_has_tmp_var dexp_list
  | Dconst _
  | Dunknown
  | Dsizeof _ None _ => false;

let rec attribute_compare (att1: attribute) (att2: attribute) :int =>
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
  | (Aobjc_null exp1, Aobjc_null exp2) => exp_compare exp1 exp2
  | (Aobjc_null _, _) => (-1)
  | (_, Aobjc_null _) => 1
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
  }
and const_compare (c1: const) (c2: const) :int =>
  switch (c1, c2) {
  | (Cint i1, Cint i2) => IntLit.compare i1 i2
  | (Cint _, _) => (-1)
  | (_, Cint _) => 1
  | (Cfun fn1, Cfun fn2) => Procname.compare fn1 fn2
  | (Cfun _, _) => (-1)
  | (_, Cfun _) => 1
  | (Cstr s1, Cstr s2) => string_compare s1 s2
  | (Cstr _, _) => (-1)
  | (_, Cstr _) => 1
  | (Cfloat f1, Cfloat f2) => float_compare f1 f2
  | (Cfloat _, _) => (-1)
  | (_, Cfloat _) => 1
  | (Cattribute att1, Cattribute att2) => attribute_compare att1 att2
  | (Cattribute _, _) => (-1)
  | (_, Cattribute _) => 1
  | (Cclass c1, Cclass c2) => Ident.name_compare c1 c2
  | (Cclass _, _) => (-1)
  | (_, Cclass _) => 1
  | (Cptr_to_fld fn1 t1, Cptr_to_fld fn2 t2) =>
    let n = Ident.fieldname_compare fn1 fn2;
    if (n != 0) {
      n
    } else {
      Typ.compare t1 t2
    }
  }
/** Compare epressions. Variables come before other expressions. */
and exp_compare (e1: exp) (e2: exp) :int =>
  switch (e1, e2) {
  | (Var id1, Var id2) => Ident.compare id2 id1
  | (Var _, _) => (-1)
  | (_, Var _) => 1
  | (UnOp o1 e1 to1, UnOp o2 e2 to2) =>
    let n = unop_compare o1 o2;
    if (n != 0) {
      n
    } else {
      let n = exp_compare e1 e2;
      if (n != 0) {
        n
      } else {
        opt_compare Typ.compare to1 to2
      }
    }
  | (UnOp _, _) => (-1)
  | (_, UnOp _) => 1
  | (BinOp o1 e1 f1, BinOp o2 e2 f2) =>
    let n = binop_compare o1 o2;
    if (n != 0) {
      n
    } else {
      let n = exp_compare e1 e2;
      if (n != 0) {
        n
      } else {
        exp_compare f1 f2
      }
    }
  | (BinOp _, _) => (-1)
  | (_, BinOp _) => 1
  | (Exn e1, Exn e2) => exp_compare e1 e2
  | (Exn _, _) => (-1)
  | (_, Exn _) => 1
  | (Closure {name: n1, captured_vars: c1}, Closure {name: n2, captured_vars: c2}) =>
    let captured_var_compare acc (e1, pvar1, typ1) (e2, pvar2, typ2) =>
      if (acc != 0) {
        acc
      } else {
        let n = exp_compare e1 e2;
        if (n != 0) {
          n
        } else {
          let n = Pvar.compare pvar1 pvar2;
          if (n != 0) {
            n
          } else {
            Typ.compare typ1 typ2
          }
        }
      };
    let n = Procname.compare n1 n2;
    if (n != 0) {
      n
    } else {
      IList.fold_left2 captured_var_compare 0 c1 c2
    }
  | (Closure _, _) => (-1)
  | (_, Closure _) => 1
  | (Const c1, Const c2) => const_compare c1 c2
  | (Const _, _) => (-1)
  | (_, Const _) => 1
  | (Cast t1 e1, Cast t2 e2) =>
    let n = exp_compare e1 e2;
    if (n != 0) {
      n
    } else {
      Typ.compare t1 t2
    }
  | (Cast _, _) => (-1)
  | (_, Cast _) => 1
  | (Lvar i1, Lvar i2) => Pvar.compare i1 i2
  | (Lvar _, _) => (-1)
  | (_, Lvar _) => 1
  | (Lfield e1 f1 t1, Lfield e2 f2 t2) =>
    let n = exp_compare e1 e2;
    if (n != 0) {
      n
    } else {
      let n = Ident.fieldname_compare f1 f2;
      if (n != 0) {
        n
      } else {
        Typ.compare t1 t2
      }
    }
  | (Lfield _, _) => (-1)
  | (_, Lfield _) => 1
  | (Lindex e1 f1, Lindex e2 f2) =>
    let n = exp_compare e1 e2;
    if (n != 0) {
      n
    } else {
      exp_compare f1 f2
    }
  | (Lindex _, _) => (-1)
  | (_, Lindex _) => 1
  | (Sizeof t1 l1 s1, Sizeof t2 l2 s2) =>
    let n = Typ.compare t1 t2;
    if (n != 0) {
      n
    } else {
      let n = opt_compare exp_compare l1 l2;
      if (n != 0) {
        n
      } else {
        Subtype.compare s1 s2
      }
    }
  };

let const_equal c1 c2 => const_compare c1 c2 == 0;

let exp_equal e1 e2 => exp_compare e1 e2 == 0;

let rec exp_is_array_index_of exp1 exp2 =>
  switch exp1 {
  | Lindex exp _ => exp_is_array_index_of exp exp2
  | _ => exp_equal exp1 exp2
  };

let ident_exp_compare = pair_compare Ident.compare exp_compare;

let ident_exp_equal ide1 ide2 => ident_exp_compare ide1 ide2 == 0;

let exp_list_compare = IList.compare exp_compare;

let exp_list_equal el1 el2 => exp_list_compare el1 el2 == 0;

let attribute_equal att1 att2 => attribute_compare att1 att2 == 0;


/** Compare atoms. Equalities come before disequalities */
let atom_compare a b =>
  if (a === b) {
    0
  } else {
    switch (a, b) {
    | (Aeq e1 e2, Aeq f1 f2) =>
      let n = exp_compare e1 f1;
      if (n != 0) {
        n
      } else {
        exp_compare e2 f2
      }
    | (Aeq _, Aneq _) => (-1)
    | (Aneq _, Aeq _) => 1
    | (Aneq e1 e2, Aneq f1 f2) =>
      let n = exp_compare e1 f1;
      if (n != 0) {
        n
      } else {
        exp_compare e2 f2
      }
    }
  };

let atom_equal x y => atom_compare x y == 0;

let lseg_kind_compare k1 k2 =>
  switch (k1, k2) {
  | (Lseg_NE, Lseg_NE) => 0
  | (Lseg_NE, Lseg_PE) => (-1)
  | (Lseg_PE, Lseg_NE) => 1
  | (Lseg_PE, Lseg_PE) => 0
  };

let lseg_kind_equal k1 k2 => lseg_kind_compare k1 k2 == 0;

/* Comparison for strexps */
let rec strexp_compare se1 se2 =>
  if (se1 === se2) {
    0
  } else {
    switch (se1, se2) {
    | (Eexp e1 _, Eexp e2 _) => exp_compare e1 e2
    | (Eexp _, _) => (-1)
    | (_, Eexp _) => 1
    | (Estruct fel1 _, Estruct fel2 _) => fld_strexp_list_compare fel1 fel2
    | (Estruct _, _) => (-1)
    | (_, Estruct _) => 1
    | (Earray e1 esel1 _, Earray e2 esel2 _) =>
      let n = exp_compare e1 e2;
      if (n != 0) {
        n
      } else {
        exp_strexp_list_compare esel1 esel2
      }
    }
  }
and fld_strexp_compare fse1 fse2 => pair_compare Ident.fieldname_compare strexp_compare fse1 fse2
and fld_strexp_list_compare fsel1 fsel2 => IList.compare fld_strexp_compare fsel1 fsel2
and exp_strexp_compare ese1 ese2 => pair_compare exp_compare strexp_compare ese1 ese2
and exp_strexp_list_compare esel1 esel2 => IList.compare exp_strexp_compare esel1 esel2
/** Comparsion between heap predicates. Hpointsto comes before others. */
and hpred_compare hpred1 hpred2 =>
  if (hpred1 === hpred2) {
    0
  } else {
    switch (hpred1, hpred2) {
    | (Hpointsto e1 _ _, Hlseg _ _ e2 _ _) when exp_compare e2 e1 != 0 => exp_compare e2 e1
    | (Hpointsto e1 _ _, Hdllseg _ _ e2 _ _ _ _) when exp_compare e2 e1 != 0 => exp_compare e2 e1
    | (Hlseg _ _ e1 _ _, Hpointsto e2 _ _) when exp_compare e2 e1 != 0 => exp_compare e2 e1
    | (Hlseg _ _ e1 _ _, Hdllseg _ _ e2 _ _ _ _) when exp_compare e2 e1 != 0 => exp_compare e2 e1
    | (Hdllseg _ _ e1 _ _ _ _, Hpointsto e2 _ _) when exp_compare e2 e1 != 0 => exp_compare e2 e1
    | (Hdllseg _ _ e1 _ _ _ _, Hlseg _ _ e2 _ _) when exp_compare e2 e1 != 0 => exp_compare e2 e1
    | (Hpointsto e1 se1 te1, Hpointsto e2 se2 te2) =>
      let n = exp_compare e2 e1;
      if (n != 0) {
        n
      } else {
        let n = strexp_compare se2 se1;
        if (n != 0) {
          n
        } else {
          exp_compare te2 te1
        }
      }
    | (Hpointsto _, _) => (-1)
    | (_, Hpointsto _) => 1
    | (Hlseg k1 hpar1 e1 f1 el1, Hlseg k2 hpar2 e2 f2 el2) =>
      let n = exp_compare e2 e1;
      if (n != 0) {
        n
      } else {
        let n = lseg_kind_compare k2 k1;
        if (n != 0) {
          n
        } else {
          let n = hpara_compare hpar2 hpar1;
          if (n != 0) {
            n
          } else {
            let n = exp_compare f2 f1;
            if (n != 0) {
              n
            } else {
              exp_list_compare el2 el1
            }
          }
        }
      }
    | (Hlseg _, Hdllseg _) => (-1)
    | (Hdllseg _, Hlseg _) => 1
    | (Hdllseg k1 hpar1 e1 f1 g1 h1 el1, Hdllseg k2 hpar2 e2 f2 g2 h2 el2) =>
      let n = exp_compare e2 e1;
      if (n != 0) {
        n
      } else {
        let n = lseg_kind_compare k2 k1;
        if (n != 0) {
          n
        } else {
          let n = hpara_dll_compare hpar2 hpar1;
          if (n != 0) {
            n
          } else {
            let n = exp_compare f2 f1;
            if (n != 0) {
              n
            } else {
              let n = exp_compare g2 g1;
              if (n != 0) {
                n
              } else {
                let n = exp_compare h2 h1;
                if (n != 0) {
                  n
                } else {
                  exp_list_compare el2 el1
                }
              }
            }
          }
        }
      }
    }
  }
and hpred_list_compare l1 l2 => IList.compare hpred_compare l1 l2
and hpara_compare hp1 hp2 => {
  let n = Ident.compare hp1.root hp2.root;
  if (n != 0) {
    n
  } else {
    let n = Ident.compare hp1.next hp2.next;
    if (n != 0) {
      n
    } else {
      let n = Ident.ident_list_compare hp1.svars hp2.svars;
      if (n != 0) {
        n
      } else {
        let n = Ident.ident_list_compare hp1.evars hp2.evars;
        if (n != 0) {
          n
        } else {
          hpred_list_compare hp1.body hp2.body
        }
      }
    }
  }
}
and hpara_dll_compare hp1 hp2 => {
  let n = Ident.compare hp1.cell hp2.cell;
  if (n != 0) {
    n
  } else {
    let n = Ident.compare hp1.blink hp2.blink;
    if (n != 0) {
      n
    } else {
      let n = Ident.compare hp1.flink hp2.flink;
      if (n != 0) {
        n
      } else {
        let n = Ident.ident_list_compare hp1.svars_dll hp2.svars_dll;
        if (n != 0) {
          n
        } else {
          let n = Ident.ident_list_compare hp1.evars_dll hp2.evars_dll;
          if (n != 0) {
            n
          } else {
            hpred_list_compare hp1.body_dll hp2.body_dll
          }
        }
      }
    }
  }
};

let strexp_equal se1 se2 => strexp_compare se1 se2 == 0;

let hpred_equal hpred1 hpred2 => hpred_compare hpred1 hpred2 == 0;

let hpara_equal hpara1 hpara2 => hpara_compare hpara1 hpara2 == 0;

let hpara_dll_equal hpara1 hpara2 => hpara_dll_compare hpara1 hpara2 == 0;


/** {2 Sets of expressions} */
let module ExpSet = Set.Make {
  type t = exp;
  let compare = exp_compare;
};

let module ExpMap = Map.Make {
  type t = exp;
  let compare = exp_compare;
};

let elist_to_eset es => IList.fold_left (fun set e => ExpSet.add e set) ExpSet.empty es;


/** {2 Sets of heap predicates} */
let module HpredSet = Set.Make {
  type t = hpred;
  let compare = hpred_compare;
};


/** {2 Pretty Printing} */
/** Begin change color if using diff printing, return updated printenv and change status */
let color_pre_wrapper pe f x =>
  if (Config.print_using_diff && pe.pe_kind !== PP_TEXT) {
    let color = pe.pe_cmap_norm (Obj.repr x);
    if (color !== pe.pe_color) {
      (
        if (pe.pe_kind === PP_HTML) {
          Io_infer.Html.pp_start_color
        } else {
          Latex.pp_color
        }
      )
        f color;
      if (color === Red) {
        (/** All subexpressiona red */ {...pe, pe_cmap_norm: colormap_red, pe_color: Red}, true)
      } else {
        ({...pe, pe_color: color}, true)
      }
    } else {
      (pe, false)
    }
  } else {
    (pe, false)
  };


/** Close color annotation if changed */
let color_post_wrapper changed pe f =>
  if changed {
    if (pe.pe_kind === PP_HTML) {
      Io_infer.Html.pp_end_color f ()
    } else {
      Latex.pp_color f pe.pe_color
    }
  };


/** Print a sequence with difference mode if enabled. */
let pp_seq_diff pp pe0 f =>
  if (not Config.print_using_diff) {
    pp_comma_seq pp f
  } else {
    let rec doit =
      fun
      | [] => ()
      | [x] => {
          let (_, changed) = color_pre_wrapper pe0 f x;
          F.fprintf f "%a" pp x;
          color_post_wrapper changed pe0 f
        }
      | [x, ...l] => {
          let (_, changed) = color_pre_wrapper pe0 f x;
          F.fprintf f "%a" pp x;
          color_post_wrapper changed pe0 f;
          F.fprintf f ", ";
          doit l
        };
    doit
  };

let text_binop =
  fun
  | PlusA => "+"
  | PlusPI => "+"
  | MinusA
  | MinusPP => "-"
  | MinusPI => "-"
  | Mult => "*"
  | Div => "/"
  | Mod => "%"
  | Shiftlt => "<<"
  | Shiftrt => ">>"
  | Lt => "<"
  | Gt => ">"
  | Le => "<="
  | Ge => ">="
  | Eq => "=="
  | Ne => "!="
  | BAnd => "&"
  | BXor => "^"
  | BOr => "|"
  | LAnd => "&&"
  | LOr => "||"
  | PtrFld => "_ptrfld_";


/** String representation of unary operator. */
let str_unop =
  fun
  | Neg => "-"
  | BNot => "~"
  | LNot => "!";


/** Pretty print a binary operator. */
let str_binop pe binop =>
  switch pe.pe_kind {
  | PP_HTML =>
    switch binop {
    | Ge => " &gt;= "
    | Le => " &lt;= "
    | Gt => " &gt; "
    | Lt => " &lt; "
    | Shiftlt => " &lt;&lt; "
    | Shiftrt => " &gt;&gt; "
    | _ => text_binop binop
    }
  | PP_LATEX =>
    switch binop {
    | Ge => " \\geq "
    | Le => " \\leq "
    | _ => text_binop binop
    }
  | _ => text_binop binop
  };

let java () => !Config.curr_language == Config.Java;

let eradicate_java () => Config.eradicate && java ();


/** convert a dexp to a string */
let rec dexp_to_string =
  fun
  | Darray de1 de2 => dexp_to_string de1 ^ "[" ^ dexp_to_string de2 ^ "]"
  | Dbinop op de1 de2 => "(" ^ dexp_to_string de1 ^ str_binop pe_text op ^ dexp_to_string de2 ^ ")"
  | Dconst (Cfun pn) => Procname.to_simplified_string pn
  | Dconst c => exp_to_string (Const c)
  | Dderef de => "*" ^ dexp_to_string de
  | Dfcall fun_dexp args _ {cf_virtual: isvirtual} => {
      let pp_arg fmt de => F.fprintf fmt "%s" (dexp_to_string de);
      let pp_args fmt des =>
        if (eradicate_java ()) {
          if (des != []) {
            F.fprintf fmt "..."
          }
        } else {
          pp_comma_seq pp_arg fmt des
        };
      let pp_fun fmt => (
        fun
        | Dconst (Cfun pname) => {
            let s =
              switch pname {
              | Procname.Java pname_java => Procname.java_get_method pname_java
              | _ => Procname.to_string pname
              };
            F.fprintf fmt "%s" s
          }
        | de => F.fprintf fmt "%s" (dexp_to_string de)
      );
      let (receiver, args') =
        switch args {
        | [Dpvar pv, ...args'] when isvirtual && Pvar.is_this pv => (None, args')
        | [a, ...args'] when isvirtual => (Some a, args')
        | _ => (None, args)
        };
      let pp fmt () => {
        let pp_receiver fmt => (
          fun
          | None => ()
          | Some arg => F.fprintf fmt "%a." pp_arg arg
        );
        F.fprintf fmt "%a%a(%a)" pp_receiver receiver pp_fun fun_dexp pp_args args'
      };
      pp_to_string pp ()
    }
  | Darrow (Dpvar pv) f when Pvar.is_this pv =>
    /* this->fieldname */
    Ident.fieldname_to_simplified_string f
  | Darrow de f =>
    if (Ident.fieldname_is_hidden f) {
      dexp_to_string de
    } else if (java ()) {
      dexp_to_string de ^ "." ^ Ident.fieldname_to_flat_string f
    } else {
      dexp_to_string de ^ "->" ^ Ident.fieldname_to_string f
    }
  | Ddot (Dpvar _) fe when eradicate_java () =>
    /* static field access */
    Ident.fieldname_to_simplified_string fe
  | Ddot de f =>
    if (Ident.fieldname_is_hidden f) {
      "&" ^ dexp_to_string de
    } else if (java ()) {
      dexp_to_string de ^ "." ^ Ident.fieldname_to_flat_string f
    } else {
      dexp_to_string de ^ "." ^ Ident.fieldname_to_string f
    }
  | Dpvar pv => Mangled.to_string (Pvar.get_name pv)
  | Dpvaraddr pv => {
      let s =
        if (eradicate_java ()) {
          Pvar.get_simplified_name pv
        } else {
          Mangled.to_string (Pvar.get_name pv)
        };
      let ampersand =
        if (eradicate_java ()) {
          ""
        } else {
          "&"
        };
      ampersand ^ s
    }
  | Dunop op de => str_unop op ^ dexp_to_string de
  | Dsizeof typ _ _ => pp_to_string (Typ.pp_full pe_text) typ
  | Dunknown => "unknown"
  | Dretcall de _ _ _ => "returned by " ^ dexp_to_string de
/** Pretty print a dexp. */
and pp_dexp fmt de => F.fprintf fmt "%s" (dexp_to_string de)
/** Pretty print a value path */
and pp_vpath pe fmt vpath => {
  let pp fmt =>
    fun
    | Some de => pp_dexp fmt de
    | None => ();
  if (pe.pe_kind === PP_HTML) {
    F.fprintf
      fmt
      " %a{vpath: %a}%a"
      Io_infer.Html.pp_start_color
      Orange
      pp
      vpath
      Io_infer.Html.pp_end_color
      ()
  } else {
    F.fprintf fmt "%a" pp vpath
  }
}
/** convert the attribute to a string */
and attribute_to_string pe =>
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
          pp_to_string (pp_vpath pe) ra.ra_vpath
        } else {
          ""
        };
      name ^
        str_binop pe Lt ^
        Procname.to_string ra.ra_pname ^
        ":" ^
        string_of_int ra.ra_loc.Location.line ^
        str_binop pe Gt ^
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
      "DANGL" ^ str_binop pe Lt ^ dks ^ str_binop pe Gt
    }
  | Aundef pn _ loc _ =>
    "UND" ^
      str_binop pe Lt ^
      Procname.to_string pn ^
      str_binop pe Gt ^
      ":" ^
      string_of_int loc.Location.line
  | Ataint {taint_source} => "TAINTED[" ^ Procname.to_string taint_source ^ "]"
  | Auntaint _ => "UNTAINTED"
  | Alocked => "LOCKED"
  | Aunlocked => "UNLOCKED"
  | Adiv0 (_, _) => "DIV0"
  | Aobjc_null exp => {
      let info_s =
        switch exp {
        | Lvar var => "FORMAL " ^ Pvar.to_string var
        | Lfield _ => "FIELD " ^ exp_to_string exp
        | _ => ""
        };
      "OBJC_NULL[" ^ info_s ^ "]"
    }
  | Aretval pn _ => "RET" ^ str_binop pe Lt ^ Procname.to_string pn ^ str_binop pe Gt
  | Aobserver => "OBSERVER"
  | Aunsubscribed_observer => "UNSUBSCRIBED_OBSERVER"
and pp_const pe f =>
  fun
  | Cint i => IntLit.pp f i
  | Cfun fn =>
    switch pe.pe_kind {
    | PP_HTML => F.fprintf f "_fun_%s" (Escape.escape_xml (Procname.to_string fn))
    | _ => F.fprintf f "_fun_%s" (Procname.to_string fn)
    }
  | Cstr s => F.fprintf f "\"%s\"" (String.escaped s)
  | Cfloat v => F.fprintf f "%f" v
  | Cattribute att => F.fprintf f "%s" (attribute_to_string pe att)
  | Cclass c => F.fprintf f "%a" Ident.pp_name c
  | Cptr_to_fld fn _ => F.fprintf f "__fld_%a" Ident.pp_fieldname fn
/** Pretty print an expression. */
and _pp_exp pe0 pp_t f e0 => {
  let (pe, changed) = color_pre_wrapper pe0 f e0;
  let e =
    switch pe.pe_obj_sub {
    | Some sub => Obj.obj (sub (Obj.repr e0)) /* apply object substitution to expression */
    | None => e0
    };
  if (not (exp_equal e0 e)) {
    switch e {
    | Lvar pvar => Pvar.pp_value pe f pvar
    | _ => assert false
    }
  } else {
    let pp_exp = _pp_exp pe pp_t;
    let print_binop_stm_output e1 op e2 =>
      switch op {
      | Eq
      | Ne
      | PlusA
      | Mult => F.fprintf f "(%a %s %a)" pp_exp e2 (str_binop pe op) pp_exp e1
      | Lt => F.fprintf f "(%a %s %a)" pp_exp e2 (str_binop pe Gt) pp_exp e1
      | Gt => F.fprintf f "(%a %s %a)" pp_exp e2 (str_binop pe Lt) pp_exp e1
      | Le => F.fprintf f "(%a %s %a)" pp_exp e2 (str_binop pe Ge) pp_exp e1
      | Ge => F.fprintf f "(%a %s %a)" pp_exp e2 (str_binop pe Le) pp_exp e1
      | _ => F.fprintf f "(%a %s %a)" pp_exp e1 (str_binop pe op) pp_exp e2
      };
    switch e {
    | Var id => (Ident.pp pe) f id
    | Const c => F.fprintf f "%a" (pp_const pe) c
    | Cast typ e => F.fprintf f "(%a)%a" pp_t typ pp_exp e
    | UnOp op e _ => F.fprintf f "%s%a" (str_unop op) pp_exp e
    | BinOp op (Const c) e2 when Config.smt_output => print_binop_stm_output (Const c) op e2
    | BinOp op e1 e2 => F.fprintf f "(%a %s %a)" pp_exp e1 (str_binop pe op) pp_exp e2
    | Exn e => F.fprintf f "EXN %a" pp_exp e
    | Closure {name, captured_vars} =>
      let id_exps = IList.map (fun (id_exp, _, _) => id_exp) captured_vars;
      F.fprintf f "(%a)" (pp_comma_seq pp_exp) [Const (Cfun name), ...id_exps]
    | Lvar pv => Pvar.pp pe f pv
    | Lfield e fld _ => F.fprintf f "%a.%a" pp_exp e Ident.pp_fieldname fld
    | Lindex e1 e2 => F.fprintf f "%a[%a]" pp_exp e1 pp_exp e2
    | Sizeof t l s =>
      let pp_len f l => Option.map_default (F.fprintf f "[%a]" pp_exp) () l;
      F.fprintf f "sizeof(%a%a%a)" pp_t t pp_len l Subtype.pp s
    }
  };
  color_post_wrapper changed pe0 f
}
and pp_exp pe f e => _pp_exp pe (Typ.pp pe) f e
/** Convert an expression to a string */
and exp_to_string e => pp_to_string (pp_exp pe_text) e;


/** dump an expression. */
let d_exp (e: exp) => L.add_print_action (L.PTexp, Obj.repr e);


/** Pretty print a list of expressions. */
let pp_exp_list pe f expl => (pp_seq (pp_exp pe)) f expl;


/** dump a list of expressions. */
let d_exp_list (el: list exp) => L.add_print_action (L.PTexp_list, Obj.repr el);

let pp_texp pe f =>
  fun
  | Sizeof t l s => {
      let pp_len f l => Option.map_default (F.fprintf f "[%a]" (pp_exp pe)) () l;
      F.fprintf f "%a%a%a" (Typ.pp pe) t pp_len l Subtype.pp s
    }
  | e => (pp_exp pe) f e;


/** Pretty print a type with all the details. */
let pp_texp_full pe f =>
  fun
  | Sizeof t l s => {
      let pp_len f l => Option.map_default (F.fprintf f "[%a]" (pp_exp pe)) () l;
      F.fprintf f "%a%a%a" (Typ.pp_full pe) t pp_len l Subtype.pp s
    }
  | e => (_pp_exp pe) (Typ.pp_full pe) f e;


/** Dump a type expression with all the details. */
let d_texp_full (te: exp) => L.add_print_action (L.PTtexp_full, Obj.repr te);


/** Pretty print an offset */
let pp_offset pe f =>
  fun
  | Off_fld fld _ => F.fprintf f "%a" Ident.pp_fieldname fld
  | Off_index exp => F.fprintf f "%a" (pp_exp pe) exp;


/** dump an offset. */
let d_offset (off: offset) => L.add_print_action (L.PToff, Obj.repr off);


/** Pretty print a list of offsets */
let rec pp_offset_list pe f =>
  fun
  | [] => ()
  | [off1, off2] => F.fprintf f "%a.%a" (pp_offset pe) off1 (pp_offset pe) off2
  | [off, ...off_list] => F.fprintf f "%a.%a" (pp_offset pe) off (pp_offset_list pe) off_list;


/** Dump a list of offsets */
let d_offset_list (offl: list offset) => L.add_print_action (L.PToff_list, Obj.repr offl);

let pp_exp_typ pe f (e, t) => F.fprintf f "%a:%a" (pp_exp pe) e (Typ.pp pe) t;


/** Get the location of the instruction */
let instr_get_loc =
  fun
  | Letderef _ _ _ loc
  | Set _ _ _ loc
  | Prune _ loc _ _
  | Call _ _ _ loc _
  | Nullify _ loc
  | Abstract loc
  | Remove_temps _ loc
  | Stackop _ loc
  | Declare_locals _ loc => loc;


/** get the expressions occurring in the instruction */
let instr_get_exps =
  fun
  | Letderef id e _ _ => [Var id, e]
  | Set e1 _ e2 _ => [e1, e2]
  | Prune cond _ _ _ => [cond]
  | Call ret_ids e _ _ _ => [e, ...(IList.map (fun id => Var id)) ret_ids]
  | Nullify pvar _ => [Lvar pvar]
  | Abstract _ => []
  | Remove_temps temps _ => IList.map (fun id => Var id) temps
  | Stackop _ => []
  | Declare_locals _ => [];


/** Pretty print call flags */
let pp_call_flags f cf => {
  if cf.cf_virtual {
    F.fprintf f " virtual"
  };
  if cf.cf_noreturn {
    F.fprintf f " noreturn"
  }
};


/** Pretty print an instruction. */
let pp_instr pe0 f instr => {
  let (pe, changed) = color_pre_wrapper pe0 f instr;
  switch instr {
  | Letderef id e t loc =>
    F.fprintf f "%a=*%a:%a %a" (Ident.pp pe) id (pp_exp pe) e (Typ.pp pe) t Location.pp loc
  | Set e1 t e2 loc =>
    F.fprintf f "*%a:%a=%a %a" (pp_exp pe) e1 (Typ.pp pe) t (pp_exp pe) e2 Location.pp loc
  | Prune cond loc true_branch _ =>
    F.fprintf f "PRUNE(%a, %b); %a" (pp_exp pe) cond true_branch Location.pp loc
  | Call ret_ids e arg_ts loc cf =>
    switch ret_ids {
    | [] => ()
    | _ => F.fprintf f "%a=" (pp_comma_seq (Ident.pp pe)) ret_ids
    };
    F.fprintf
      f
      "%a(%a)%a %a"
      (pp_exp pe)
      e
      (pp_comma_seq (pp_exp_typ pe))
      arg_ts
      pp_call_flags
      cf
      Location.pp
      loc
  | Nullify pvar loc => F.fprintf f "NULLIFY(%a); %a" (Pvar.pp pe) pvar Location.pp loc
  | Abstract loc => F.fprintf f "APPLY_ABSTRACTION; %a" Location.pp loc
  | Remove_temps temps loc =>
    F.fprintf f "REMOVE_TEMPS(%a); %a" (Ident.pp_list pe) temps Location.pp loc
  | Stackop stackop loc =>
    let s =
      switch stackop {
      | Push => "Push"
      | Swap => "Swap"
      | Pop => "Pop"
      };
    F.fprintf f "STACKOP.%s; %a" s Location.pp loc
  | Declare_locals ptl loc =>
    let pp_typ fmt (pvar, _) => F.fprintf fmt "%a" (Pvar.pp pe) pvar;
    F.fprintf f "DECLARE_LOCALS(%a); %a" (pp_comma_seq pp_typ) ptl Location.pp loc
  };
  color_post_wrapper changed pe0 f
};


/** Check if a pvar is a local pointing to a block in objc */
let is_block_pvar pvar => Typ.has_block_prefix (Mangled.to_string (Pvar.get_name pvar));

/* A block pvar used to explain retain cycles */
let block_pvar = Pvar.mk (Mangled.from_string "block") (Procname.from_string_c_fun "");


/** Dump an instruction. */
let d_instr (i: instr) => L.add_print_action (L.PTinstr, Obj.repr i);

let rec pp_instr_list pe f =>
  fun
  | [] => F.fprintf f ""
  | [i, ...is] => F.fprintf f "%a;@\n%a" (pp_instr pe) i (pp_instr_list pe) is;


/** Dump a list of instructions. */
let d_instr_list (il: list instr) => L.add_print_action (L.PTinstr_list, Obj.repr il);

let pp_atom pe0 f a => {
  let (pe, changed) = color_pre_wrapper pe0 f a;
  switch a {
  | Aeq (BinOp op e1 e2) (Const (Cint i)) when IntLit.isone i =>
    switch pe.pe_kind {
    | PP_TEXT
    | PP_HTML => F.fprintf f "%a" (pp_exp pe) (BinOp op e1 e2)
    | PP_LATEX => F.fprintf f "%a" (pp_exp pe) (BinOp op e1 e2)
    }
  | Aeq e1 e2 =>
    switch pe.pe_kind {
    | PP_TEXT
    | PP_HTML => F.fprintf f "%a = %a" (pp_exp pe) e1 (pp_exp pe) e2
    | PP_LATEX => F.fprintf f "%a{=}%a" (pp_exp pe) e1 (pp_exp pe) e2
    }
  | Aneq (Const (Cattribute _) as ea) e
  | Aneq e (Const (Cattribute _) as ea) => F.fprintf f "%a(%a)" (pp_exp pe) ea (pp_exp pe) e
  | Aneq e1 e2 =>
    switch pe.pe_kind {
    | PP_TEXT
    | PP_HTML => F.fprintf f "%a != %a" (pp_exp pe) e1 (pp_exp pe) e2
    | PP_LATEX => F.fprintf f "%a{\\neq}%a" (pp_exp pe) e1 (pp_exp pe) e2
    }
  };
  color_post_wrapper changed pe0 f
};


/** dump an atom */
let d_atom (a: atom) => L.add_print_action (L.PTatom, Obj.repr a);

let pp_lseg_kind f =>
  fun
  | Lseg_NE => F.fprintf f "ne"
  | Lseg_PE => F.fprintf f "";


/** Print a *-separated sequence. */
let rec pp_star_seq pp f =>
  fun
  | [] => ()
  | [x] => F.fprintf f "%a" pp x
  | [x, ...l] => F.fprintf f "%a * %a" pp x (pp_star_seq pp) l;


/********* START OF MODULE Predicates **********/
/** Module Predicates records the occurrences of predicates as parameters
    of (doubly -)linked lists and Epara. Provides unique numbering
    for predicates and an iterator. */
let module Predicates: {
  /** predicate environment */
  type env;

  /** create an empty predicate environment */
  let empty_env: unit => env;

  /** return true if the environment is empty */
  let is_empty: env => bool;

  /** return the id of the hpara */
  let get_hpara_id: env => hpara => int;

  /** return the id of the hpara_dll */
  let get_hpara_dll_id: env => hpara_dll => int;

  /** [iter env f f_dll] iterates [f] and [f_dll] on all the hpara and hpara_dll,
      passing the unique id to the functions. The iterator can only be used once. */
  let iter: env => (int => hpara => unit) => (int => hpara_dll => unit) => unit;

  /** Process one hpred, updating the predicate environment */
  let process_hpred: env => hpred => unit;
} = {
  /** hash tables for hpara */
  let module HparaHash = Hashtbl.Make {
    type t = hpara;
    let equal = hpara_equal;
    let hash = Hashtbl.hash;
  };

  /** hash tables for hpara_dll */
  let module HparaDllHash = Hashtbl.Make {
    type t = hpara_dll;
    let equal = hpara_dll_equal;
    let hash = Hashtbl.hash;
  };

  /** Map each visited hpara to a unique number and a boolean denoting whether it has been emitted,
      also keep a list of hparas still to be emitted. Same for hpara_dll. */
  type env = {
    mutable num: int,
    hash: HparaHash.t (int, bool),
    mutable todo: list hpara,
    hash_dll: HparaDllHash.t (int, bool),
    mutable todo_dll: list hpara_dll
  };

  /** return true if the environment is empty */
  let is_empty env => env.num == 0;

  /** return the id of the hpara */
  let get_hpara_id env hpara => fst (HparaHash.find env.hash hpara);

  /** return the id of the hpara_dll */
  let get_hpara_dll_id env hpara_dll => fst (HparaDllHash.find env.hash_dll hpara_dll);

  /** Process one hpara, updating the map from hparas to numbers, and the todo list */
  let process_hpara env hpara =>
    if (not (HparaHash.mem env.hash hpara)) {
      HparaHash.add env.hash hpara (env.num, false);
      env.num = env.num + 1;
      env.todo = env.todo @ [hpara]
    };

  /** Process one hpara_dll, updating the map from hparas to numbers, and the todo list */
  let process_hpara_dll env hpara_dll =>
    if (not (HparaDllHash.mem env.hash_dll hpara_dll)) {
      HparaDllHash.add env.hash_dll hpara_dll (env.num, false);
      env.num = env.num + 1;
      env.todo_dll = env.todo_dll @ [hpara_dll]
    };

  /** Process a sexp, updating env */
  let rec process_sexp env =>
    fun
    | Eexp _ => ()
    | Earray _ esel _ => IList.iter (fun (_, se) => process_sexp env se) esel
    | Estruct fsel _ => IList.iter (fun (_, se) => process_sexp env se) fsel;

  /** Process one hpred, updating env */
  let rec process_hpred env =>
    fun
    | Hpointsto _ se _ => process_sexp env se
    | Hlseg _ hpara _ _ _ => {
        IList.iter (process_hpred env) hpara.body;
        process_hpara env hpara
      }
    | Hdllseg _ hpara_dll _ _ _ _ _ => {
        IList.iter (process_hpred env) hpara_dll.body_dll;
        process_hpara_dll env hpara_dll
      };

  /** create an empty predicate environment */
  let empty_env () => {
    num: 0,
    hash: HparaHash.create 3,
    todo: [],
    hash_dll: HparaDllHash.create 3,
    todo_dll: []
  };

  /** iterator for predicates which are marked as todo in env,
      unless they have been visited already.
      This can in turn extend the todo list for the nested predicates,
      which are then visited as well.
      Can be applied only once, as it destroys the todo list */
  let iter (env: env) f f_dll =>
    while (env.todo !== [] || env.todo_dll !== []) {
      if (env.todo !== []) {
        let hpara = IList.hd env.todo;
        let () = env.todo = IList.tl env.todo;
        let (n, emitted) = HparaHash.find env.hash hpara;
        if (not emitted) {
          f n hpara
        }
      } else if (
        env.todo_dll !== []
      ) {
        let hpara_dll = IList.hd env.todo_dll;
        let () = env.todo_dll = IList.tl env.todo_dll;
        let (n, emitted) = HparaDllHash.find env.hash_dll hpara_dll;
        if (not emitted) {
          f_dll n hpara_dll
        }
      }
    };
};


/********* END OF MODULE Predicates **********/
let pp_texp_simple pe =>
  switch pe.pe_opt {
  | PP_SIM_DEFAULT => pp_texp pe
  | PP_SIM_WITH_TYP => pp_texp_full pe
  };

let inst_abstraction = Iabstraction;

let inst_actual_precondition = Iactual_precondition;

let inst_alloc = Ialloc;

let inst_formal = Iformal None false; /** for formal parameters */

let inst_initial = Iinitial; /** for initial values */

let inst_lookup = Ilookup;

let inst_none = Inone;

let inst_nullify = Inullify;

let inst_rearrange b loc pos => Irearrange (Some b) false loc.Location.line pos;

let inst_taint = Itaint;

let inst_update loc pos => Iupdate None false loc.Location.line pos;


/** update the location of the instrumentation */
let inst_new_loc loc inst =>
  switch inst {
  | Iabstraction => inst
  | Iactual_precondition => inst
  | Ialloc => inst
  | Iformal _ => inst
  | Iinitial => inst
  | Ilookup => inst
  | Inone => inst
  | Inullify => inst
  | Irearrange zf ncf _ pos => Irearrange zf ncf loc.Location.line pos
  | Itaint => inst
  | Iupdate zf ncf _ pos => Iupdate zf ncf loc.Location.line pos
  | Ireturn_from_call _ => Ireturn_from_call loc.Location.line
  | Ireturn_from_pointer_wrapper_call _ => Ireturn_from_pointer_wrapper_call loc.Location.line
  };


/** return a string representing the inst */
let inst_to_string inst => {
  let zero_flag_to_string =
    fun
    | Some true => "(z)"
    | _ => "";
  let null_case_flag_to_string ncf =>
    if ncf {
      "(ncf)"
    } else {
      ""
    };
  switch inst {
  | Iabstraction => "abstraction"
  | Iactual_precondition => "actual_precondition"
  | Ialloc => "alloc"
  | Iformal zf ncf => "formal" ^ zero_flag_to_string zf ^ null_case_flag_to_string ncf
  | Iinitial => "initial"
  | Ilookup => "lookup"
  | Inone => "none"
  | Inullify => "nullify"
  | Irearrange zf ncf n _ =>
    "rearrange:" ^ zero_flag_to_string zf ^ null_case_flag_to_string ncf ^ string_of_int n
  | Itaint => "taint"
  | Iupdate zf ncf n _ =>
    "update:" ^ zero_flag_to_string zf ^ null_case_flag_to_string ncf ^ string_of_int n
  | Ireturn_from_call n => "return_from_call: " ^ string_of_int n
  | Ireturn_from_pointer_wrapper_call n => "Ireturn_from_pointer_wrapper_call: " ^ string_of_int n
  }
};


/** join of instrumentations */
let inst_partial_join inst1 inst2 => {
  let fail () => {
    L.d_strln ("inst_partial_join failed on " ^ inst_to_string inst1 ^ " " ^ inst_to_string inst2);
    raise IList.Fail
  };
  if (inst1 == inst2) {
    inst1
  } else {
    switch (inst1, inst2) {
    | (_, Inone)
    | (Inone, _) => inst_none
    | (_, Ialloc)
    | (Ialloc, _) => fail ()
    | (_, Iinitial)
    | (Iinitial, _) => fail ()
    | (_, Iupdate _)
    | (Iupdate _, _) => fail ()
    | _ => inst_none
    }
  }
};


/** meet of instrumentations */
let inst_partial_meet inst1 inst2 =>
  if (inst1 == inst2) {
    inst1
  } else {
    inst_none
  };


/** Return the zero flag of the inst */
let inst_zero_flag =
  fun
  | Iabstraction => None
  | Iactual_precondition => None
  | Ialloc => None
  | Iformal zf _ => zf
  | Iinitial => None
  | Ilookup => None
  | Inone => None
  | Inullify => None
  | Irearrange zf _ _ _ => zf
  | Itaint => None
  | Iupdate zf _ _ _ => zf
  | Ireturn_from_call _
  | Ireturn_from_pointer_wrapper_call _ => None;


/** Set the null case flag of the inst. */
let inst_set_null_case_flag =
  fun
  | Iformal zf false => Iformal zf true
  | Irearrange zf false n pos => Irearrange zf true n pos
  | Iupdate zf false n pos => Iupdate zf true n pos
  | inst => inst;


/** Get the null case flag of the inst. */
let inst_get_null_case_flag =
  fun
  | Iupdate _ ncf _ _ => Some ncf
  | _ => None;


/** Update [inst_old] to [inst_new] preserving the zero flag */
let update_inst inst_old inst_new => {
  let combine_zero_flags z1 z2 =>
    switch (z1, z2) {
    | (Some b1, Some b2) => Some (b1 || b2)
    | (Some b, None) => Some b
    | (None, Some b) => Some b
    | (None, None) => None
    };
  switch inst_new {
  | Iabstraction => inst_new
  | Iactual_precondition => inst_new
  | Ialloc => inst_new
  | Iformal zf ncf =>
    let zf' = combine_zero_flags (inst_zero_flag inst_old) zf;
    Iformal zf' ncf
  | Iinitial => inst_new
  | Ilookup => inst_new
  | Inone => inst_new
  | Inullify => inst_new
  | Irearrange zf ncf n pos =>
    let zf' = combine_zero_flags (inst_zero_flag inst_old) zf;
    Irearrange zf' ncf n pos
  | Itaint => inst_new
  | Iupdate zf ncf n pos =>
    let zf' = combine_zero_flags (inst_zero_flag inst_old) zf;
    Iupdate zf' ncf n pos
  | Ireturn_from_call _ => inst_new
  | Ireturn_from_pointer_wrapper_call _ => inst_new
  }
};


/** describe an instrumentation with a string */
let pp_inst pe f inst => {
  let str = inst_to_string inst;
  if (pe.pe_kind === PP_HTML) {
    F.fprintf f " %a%s%a" Io_infer.Html.pp_start_color Orange str Io_infer.Html.pp_end_color ()
  } else {
    F.fprintf f "%s%s%s" (str_binop pe Lt) str (str_binop pe Gt)
  }
};

let pp_inst_if_trace pe f inst =>
  if Config.trace_error {
    pp_inst pe f inst
  };


/** pretty print a strexp with an optional predicate env */
let rec pp_sexp_env pe0 envo f se => {
  let (pe, changed) = color_pre_wrapper pe0 f se;
  switch se {
  | Eexp e inst => F.fprintf f "%a%a" (pp_exp pe) e (pp_inst_if_trace pe) inst
  | Estruct fel inst =>
    switch pe.pe_kind {
    | PP_TEXT
    | PP_HTML =>
      let pp_diff f (n, se) => F.fprintf f "%a:%a" Ident.pp_fieldname n (pp_sexp_env pe envo) se;
      F.fprintf f "{%a}%a" (pp_seq_diff pp_diff pe) fel (pp_inst_if_trace pe) inst
    | PP_LATEX =>
      let pp_diff f (n, se) =>
        F.fprintf f "%a:%a" (Ident.pp_fieldname_latex Latex.Boldface) n (pp_sexp_env pe envo) se;
      F.fprintf f "\\{%a\\}%a" (pp_seq_diff pp_diff pe) fel (pp_inst_if_trace pe) inst
    }
  | Earray len nel inst =>
    let pp_diff f (i, se) => F.fprintf f "%a:%a" (pp_exp pe) i (pp_sexp_env pe envo) se;
    F.fprintf f "[%a|%a]%a" (pp_exp pe) len (pp_seq_diff pp_diff pe) nel (pp_inst_if_trace pe) inst
  };
  color_post_wrapper changed pe0 f
}
/** Pretty print an hpred with an optional predicate env */
and pp_hpred_env pe0 envo f hpred => {
  let (pe, changed) = color_pre_wrapper pe0 f hpred;
  switch hpred {
  | Hpointsto e se te =>
    let pe' =
      switch (e, se) {
      | (Lvar pvar, Eexp (Var _) _) when not (Pvar.is_global pvar) =>
        {...pe, pe_obj_sub: None} /* dont use obj sub on the var defining it */
      | _ => pe
      };
    switch pe'.pe_kind {
    | PP_TEXT
    | PP_HTML =>
      F.fprintf f "%a|->%a:%a" (pp_exp pe') e (pp_sexp_env pe' envo) se (pp_texp_simple pe') te
    | PP_LATEX => F.fprintf f "%a\\mapsto %a" (pp_exp pe') e (pp_sexp_env pe' envo) se
    }
  | Hlseg k hpara e1 e2 elist =>
    switch pe.pe_kind {
    | PP_TEXT
    | PP_HTML =>
      F.fprintf
        f
        "lseg%a(%a,%a,[%a],%a)"
        pp_lseg_kind
        k
        (pp_exp pe)
        e1
        (pp_exp pe)
        e2
        (pp_comma_seq (pp_exp pe))
        elist
        (pp_hpara_env pe envo)
        hpara
    | PP_LATEX =>
      F.fprintf
        f
        "\\textsf{lseg}_{%a}(%a,%a,[%a],%a)"
        pp_lseg_kind
        k
        (pp_exp pe)
        e1
        (pp_exp pe)
        e2
        (pp_comma_seq (pp_exp pe))
        elist
        (pp_hpara_env pe envo)
        hpara
    }
  | Hdllseg k hpara_dll iF oB oF iB elist =>
    switch pe.pe_kind {
    | PP_TEXT
    | PP_HTML =>
      F.fprintf
        f
        "dllseg%a(%a,%a,%a,%a,[%a],%a)"
        pp_lseg_kind
        k
        (pp_exp pe)
        iF
        (pp_exp pe)
        oB
        (pp_exp pe)
        oF
        (pp_exp pe)
        iB
        (pp_comma_seq (pp_exp pe))
        elist
        (pp_hpara_dll_env pe envo)
        hpara_dll
    | PP_LATEX =>
      F.fprintf
        f
        "\\textsf{dllseg}_{%a}(%a,%a,%a,%a,[%a],%a)"
        pp_lseg_kind
        k
        (pp_exp pe)
        iF
        (pp_exp pe)
        oB
        (pp_exp pe)
        oF
        (pp_exp pe)
        iB
        (pp_comma_seq (pp_exp pe))
        elist
        (pp_hpara_dll_env pe envo)
        hpara_dll
    }
  };
  color_post_wrapper changed pe0 f
}
and pp_hpara_env pe envo f hpara =>
  switch envo {
  | None =>
    let (r, n, svars, evars, b) = (hpara.root, hpara.next, hpara.svars, hpara.evars, hpara.body);
    F.fprintf
      f
      "lam [%a,%a,%a]. exists [%a]. %a"
      (Ident.pp pe)
      r
      (Ident.pp pe)
      n
      (pp_seq (Ident.pp pe))
      svars
      (pp_seq (Ident.pp pe))
      evars
      (pp_star_seq (pp_hpred_env pe envo))
      b
  | Some env => F.fprintf f "P%d" (Predicates.get_hpara_id env hpara)
  }
and pp_hpara_dll_env pe envo f hpara_dll =>
  switch envo {
  | None =>
    let (iF, oB, oF, svars, evars, b) = (
      hpara_dll.cell,
      hpara_dll.blink,
      hpara_dll.flink,
      hpara_dll.svars_dll,
      hpara_dll.evars_dll,
      hpara_dll.body_dll
    );
    F.fprintf
      f
      "lam [%a,%a,%a,%a]. exists [%a]. %a"
      (Ident.pp pe)
      iF
      (Ident.pp pe)
      oB
      (Ident.pp pe)
      oF
      (pp_seq (Ident.pp pe))
      svars
      (pp_seq (Ident.pp pe))
      evars
      (pp_star_seq (pp_hpred_env pe envo))
      b
  | Some env => F.fprintf f "P%d" (Predicates.get_hpara_dll_id env hpara_dll)
  };


/** pretty print a strexp */
let pp_sexp pe f => pp_sexp_env pe None f;


/** pretty print a hpara */
let pp_hpara pe f => pp_hpara_env pe None f;


/** pretty print a hpara_dll */
let pp_hpara_dll pe f => pp_hpara_dll_env pe None f;


/** pretty print a hpred */
let pp_hpred pe f => pp_hpred_env pe None f;


/** dump a strexp. */
let d_sexp (se: strexp) => L.add_print_action (L.PTsexp, Obj.repr se);


/** Pretty print a list of expressions. */
let pp_sexp_list pe f sel =>
  F.fprintf f "%a" (pp_seq (fun f se => F.fprintf f "%a" (pp_sexp pe) se)) sel;


/** dump a list of expressions. */
let d_sexp_list (sel: list strexp) => L.add_print_action (L.PTsexp_list, Obj.repr sel);

let rec pp_hpara_list pe f =>
  fun
  | [] => ()
  | [para] => F.fprintf f "PRED: %a" (pp_hpara pe) para
  | [para, ...paras] => F.fprintf f "PRED: %a@\n@\n%a" (pp_hpara pe) para (pp_hpara_list pe) paras;

let rec pp_hpara_dll_list pe f =>
  fun
  | [] => ()
  | [para] => F.fprintf f "PRED: %a" (pp_hpara_dll pe) para
  | [para, ...paras] =>
    F.fprintf f "PRED: %a@\n@\n%a" (pp_hpara_dll pe) para (pp_hpara_dll_list pe) paras;


/** dump a hpred. */
let d_hpred (hpred: hpred) => L.add_print_action (L.PThpred, Obj.repr hpred);


/** {2 Functions for traversing SIL data types} */
let rec strexp_expmap (f: (exp, option inst) => (exp, option inst)) => {
  let fe e => fst (f (e, None));
  let fei (e, inst) =>
    switch (f (e, Some inst)) {
    | (e', None) => (e', inst)
    | (e', Some inst') => (e', inst')
    };
  fun
  | Eexp e inst => {
      let (e', inst') = fei (e, inst);
      Eexp e' inst'
    }
  | Estruct fld_se_list inst => {
      let f_fld_se (fld, se) => (fld, strexp_expmap f se);
      Estruct (IList.map f_fld_se fld_se_list) inst
    }
  | Earray len idx_se_list inst => {
      let len' = fe len;
      let f_idx_se (idx, se) => {
        let idx' = fe idx;
        (idx', strexp_expmap f se)
      };
      Earray len' (IList.map f_idx_se idx_se_list) inst
    }
};

let hpred_expmap (f: (exp, option inst) => (exp, option inst)) => {
  let fe e => fst (f (e, None));
  fun
  | Hpointsto e se te => {
      let e' = fe e;
      let se' = strexp_expmap f se;
      let te' = fe te;
      Hpointsto e' se' te'
    }
  | Hlseg k hpara root next shared => {
      let root' = fe root;
      let next' = fe next;
      let shared' = IList.map fe shared;
      Hlseg k hpara root' next' shared'
    }
  | Hdllseg k hpara iF oB oF iB shared => {
      let iF' = fe iF;
      let oB' = fe oB;
      let oF' = fe oF;
      let iB' = fe iB;
      let shared' = IList.map fe shared;
      Hdllseg k hpara iF' oB' oF' iB' shared'
    }
};

let rec strexp_instmap (f: inst => inst) strexp =>
  switch strexp {
  | Eexp e inst => Eexp e (f inst)
  | Estruct fld_se_list inst =>
    let f_fld_se (fld, se) => (fld, strexp_instmap f se);
    Estruct (IList.map f_fld_se fld_se_list) (f inst)
  | Earray len idx_se_list inst =>
    let f_idx_se (idx, se) => (idx, strexp_instmap f se);
    Earray len (IList.map f_idx_se idx_se_list) (f inst)
  }
and hpara_instmap (f: inst => inst) hpara => {
  ...hpara,
  body: IList.map (hpred_instmap f) hpara.body
}
and hpara_dll_instmap (f: inst => inst) hpara_dll => {
  ...hpara_dll,
  body_dll: IList.map (hpred_instmap f) hpara_dll.body_dll
}
and hpred_instmap (fn: inst => inst) (hpred: hpred) :hpred =>
  switch hpred {
  | Hpointsto e se te =>
    let se' = strexp_instmap fn se;
    Hpointsto e se' te
  | Hlseg k hpara e f el => Hlseg k (hpara_instmap fn hpara) e f el
  | Hdllseg k hpar_dll e f g h el => Hdllseg k (hpara_dll_instmap fn hpar_dll) e f g h el
  };

let hpred_list_expmap (f: (exp, option inst) => (exp, option inst)) (hlist: list hpred) =>
  IList.map (hpred_expmap f) hlist;

let atom_expmap (f: exp => exp) =>
  fun
  | Aeq e1 e2 => Aeq (f e1) (f e2)
  | Aneq e1 e2 => Aneq (f e1) (f e2);

let atom_list_expmap (f: exp => exp) (alist: list atom) => IList.map (atom_expmap f) alist;


/** {2 Function for computing lexps in sigma} */
let hpred_get_lexp acc =>
  fun
  | Hpointsto e _ _ => [e, ...acc]
  | Hlseg _ _ e _ _ => [e, ...acc]
  | Hdllseg _ _ e1 _ _ e2 _ => [e1, e2, ...acc];

let hpred_list_get_lexps (filter: exp => bool) (hlist: list hpred) :list exp => {
  let lexps = IList.fold_left hpred_get_lexp [] hlist;
  IList.filter filter lexps
};


/** {2 Utility Functions for Expressions} */
/** Turn an expression representing a type into the type it represents
    If not a sizeof, return the default type if given, otherwise raise an exception */
let texp_to_typ default_opt =>
  fun
  | Sizeof t _ _ => t
  | _ => Typ.unsome "texp_to_typ" default_opt;


/** Return the root of [lexp]. */
let rec root_of_lexp lexp =>
  switch lexp {
  | Var _ => lexp
  | Const _ => lexp
  | Cast _ e => root_of_lexp e
  | UnOp _
  | BinOp _
  | Exn _
  | Closure _ => lexp
  | Lvar _ => lexp
  | Lfield e _ _ => root_of_lexp e
  | Lindex e _ => root_of_lexp e
  | Sizeof _ => lexp
  };


/** Checks whether an expression denotes a location by pointer arithmetic.
    Currently, catches array - indexing expressions such as a[i] only. */
let rec exp_pointer_arith =
  fun
  | Lfield e _ _ => exp_pointer_arith e
  | Lindex _ => true
  | _ => false;

let exp_get_undefined footprint =>
  Var (
    Ident.create_fresh (
      if footprint {
        Ident.kfootprint
      } else {
        Ident.kprimed
      }
    )
  );


/** Create integer constant */
let exp_int i => Const (Cint i);


/** Create float constant */
let exp_float v => Const (Cfloat v);


/** Integer constant 0 */
let exp_zero = exp_int IntLit.zero;


/** Null constant */
let exp_null = exp_int IntLit.null;


/** Integer constant 1 */
let exp_one = exp_int IntLit.one;


/** Integer constant -1 */
let exp_minus_one = exp_int IntLit.minus_one;


/** Create integer constant corresponding to the boolean value */
let exp_bool b =>
  if b {
    exp_one
  } else {
    exp_zero
  };


/** Create expresstion [e1 == e2] */
let exp_eq e1 e2 => BinOp Eq e1 e2;


/** Create expresstion [e1 != e2] */
let exp_ne e1 e2 => BinOp Ne e1 e2;


/** Create expression [e1 <= e2] */
let exp_le e1 e2 => BinOp Le e1 e2;


/** Create expression [e1 < e2] */
let exp_lt e1 e2 => BinOp Lt e1 e2;


/** {2 Functions for computing program variables} */
let rec exp_fpv =
  fun
  | Var _ => []
  | Exn e => exp_fpv e
  | Closure {captured_vars} => IList.map (fun (_, pvar, _) => pvar) captured_vars
  | Const _ => []
  | Cast _ e
  | UnOp _ e _ => exp_fpv e
  | BinOp _ e1 e2 => exp_fpv e1 @ exp_fpv e2
  | Lvar name => [name]
  | Lfield e _ _ => exp_fpv e
  | Lindex e1 e2 => exp_fpv e1 @ exp_fpv e2
  /* TODO: Sizeof length expressions may contain variables, do not ignore them. */
  /* | Sizeof _ None _ => [] */
  /* | Sizeof _ (Some l) _ => exp_fpv l */
  | Sizeof _ _ _ => []
and exp_list_fpv el => IList.flatten (IList.map exp_fpv el);

let atom_fpv =
  fun
  | Aeq e1 e2 => exp_fpv e1 @ exp_fpv e2
  | Aneq e1 e2 => exp_fpv e1 @ exp_fpv e2;

let rec strexp_fpv =
  fun
  | Eexp e _ => exp_fpv e
  | Estruct fld_se_list _ => {
      let f (_, se) => strexp_fpv se;
      IList.flatten (IList.map f fld_se_list)
    }
  | Earray len idx_se_list _ => {
      let fpv_in_len = exp_fpv len;
      let f (idx, se) => exp_fpv idx @ strexp_fpv se;
      fpv_in_len @ IList.flatten (IList.map f idx_se_list)
    }
and hpred_fpv =
  fun
  | Hpointsto base se te => exp_fpv base @ strexp_fpv se @ exp_fpv te
  | Hlseg _ para e1 e2 elist => {
      let fpvars_in_elist = exp_list_fpv elist;
      hpara_fpv para @ /* This set has to be empty. */  exp_fpv e1 @ exp_fpv e2 @ fpvars_in_elist
    }
  | Hdllseg _ para e1 e2 e3 e4 elist => {
      let fpvars_in_elist = exp_list_fpv elist;
      hpara_dll_fpv para @
        /* This set has to be empty. */
        exp_fpv e1 @
        exp_fpv e2 @
        exp_fpv e3 @
        exp_fpv e4 @
        fpvars_in_elist
    }
/** hpara should not contain any program variables.
    This is because it might cause problems when we do interprocedural
    analysis. In interprocedural analysis, we should consider the issue
    of scopes of program variables. */
and hpara_fpv para => {
  let fpvars_in_body = IList.flatten (IList.map hpred_fpv para.body);
  switch fpvars_in_body {
  | [] => []
  | _ => assert false
  }
}
/** hpara_dll should not contain any program variables.
    This is because it might cause problems when we do interprocedural
    analysis. In interprocedural analysis, we should consider the issue
    of scopes of program variables. */
and hpara_dll_fpv para => {
  let fpvars_in_body = IList.flatten (IList.map hpred_fpv para.body_dll);
  switch fpvars_in_body {
  | [] => []
  | _ => assert false
  }
};


/** {2 Functions for computing free non-program variables} */
/** Type of free variables. These include primed, normal and footprint variables.
    We keep a count of how many types the variables appear. */
type fav = ref (list Ident.t);

let fav_new () => ref [];


/** Emptyness check. */
let fav_is_empty fav =>
  switch !fav {
  | [] => true
  | _ => false
  };


/** Check whether a predicate holds for all elements. */
let fav_for_all fav predicate => IList.for_all predicate !fav;


/** Check whether a predicate holds for some elements. */
let fav_exists fav predicate => IList.exists predicate !fav;


/** flag to indicate whether fav's are stored in duplicate form.
    Only to be used with fav_to_list */
let fav_duplicates = ref false;


/** extend [fav] with a [id] */
let (++) fav id =>
  if (!fav_duplicates || not (IList.exists (Ident.equal id) !fav)) {
    fav := [id, ...!fav]
  };


/** extend [fav] with ident list [idl] */
let (+++) fav idl => IList.iter (fun id => fav ++ id) idl;


/** add identity lists to fav */
let ident_list_fav_add idl fav => fav +++ idl;


/** Convert a list to a fav. */
let fav_from_list l => {
  let fav = fav_new ();
  let _ = IList.iter (fun id => fav ++ id) l;
  fav
};

let rec remove_duplicates_from_sorted special_equal =>
  fun
  | [] => []
  | [x] => [x]
  | [x, y, ...l] =>
    if (special_equal x y) {
      remove_duplicates_from_sorted special_equal [y, ...l]
    } else {
      [x, ...remove_duplicates_from_sorted special_equal [y, ...l]]
    };


/** Convert a [fav] to a list of identifiers while preserving the order
    that the identifiers were added to [fav]. */
let fav_to_list fav => IList.rev !fav;


/** Pretty print a fav. */
let pp_fav pe f fav => (pp_seq (Ident.pp pe)) f (fav_to_list fav);


/** Copy a [fav]. */
let fav_copy fav => ref (IList.map (fun x => x) !fav);


/** Turn a xxx_fav_add function into a xxx_fav function */
let fav_imperative_to_functional f x => {
  let fav = fav_new ();
  let _ = f fav x;
  fav
};


/** [fav_filter_ident fav f] only keeps [id] if [f id] is true. */
let fav_filter_ident fav filter => fav := IList.filter filter !fav;


/** Like [fav_filter_ident] but return a copy. */
let fav_copy_filter_ident fav filter => ref (IList.filter filter !fav);


/** checks whether every element in l1 appears l2 **/
let rec ident_sorted_list_subset l1 l2 =>
  switch (l1, l2) {
  | ([], _) => true
  | ([_, ..._], []) => false
  | ([id1, ...l1], [id2, ...l2]) =>
    let n = Ident.compare id1 id2;
    if (n == 0) {
      ident_sorted_list_subset l1 [id2, ...l2]
    } else if (n > 0) {
      ident_sorted_list_subset [id1, ...l1] l2
    } else {
      false
    }
  };


/** [fav_subset_ident fav1 fav2] returns true if every ident in [fav1]
    is in [fav2].*/
let fav_subset_ident fav1 fav2 => ident_sorted_list_subset (fav_to_list fav1) (fav_to_list fav2);

let fav_mem fav id => IList.exists (Ident.equal id) !fav;

let rec exp_fav_add fav =>
  fun
  | Var id => fav ++ id
  | Exn e => exp_fav_add fav e
  | Closure {captured_vars} => IList.iter (fun (e, _, _) => exp_fav_add fav e) captured_vars
  | Const (Cint _ | Cfun _ | Cstr _ | Cfloat _ | Cattribute _ | Cclass _ | Cptr_to_fld _) => ()
  | Cast _ e
  | UnOp _ e _ => exp_fav_add fav e
  | BinOp _ e1 e2 => {
      exp_fav_add fav e1;
      exp_fav_add fav e2
    }
  | Lvar _ => () /* do nothing since we only count non-program variables */
  | Lfield e _ _ => exp_fav_add fav e
  | Lindex e1 e2 => {
      exp_fav_add fav e1;
      exp_fav_add fav e2
    }
  /* TODO: Sizeof length expressions may contain variables, do not ignore them. */
  /* | Sizeof _ None _ => () */
  /* | Sizeof _ (Some l) _ => exp_fav_add fav l; */
  | Sizeof _ _ _ => ();

let exp_fav = fav_imperative_to_functional exp_fav_add;

let exp_fav_list e => fav_to_list (exp_fav e);

let ident_in_exp id e => {
  let fav = fav_new ();
  exp_fav_add fav e;
  fav_mem fav id
};

let atom_fav_add fav =>
  fun
  | Aeq e1 e2
  | Aneq e1 e2 => {
      exp_fav_add fav e1;
      exp_fav_add fav e2
    };

let atom_fav = fav_imperative_to_functional atom_fav_add;


/** Atoms do not contain binders */
let atom_av_add = atom_fav_add;

let rec strexp_fav_add fav =>
  fun
  | Eexp e _ => exp_fav_add fav e
  | Estruct fld_se_list _ => IList.iter (fun (_, se) => strexp_fav_add fav se) fld_se_list
  | Earray len idx_se_list _ => {
      exp_fav_add fav len;
      IList.iter
        (
          fun (e, se) => {
            exp_fav_add fav e;
            strexp_fav_add fav se
          }
        )
        idx_se_list
    };

let hpred_fav_add fav =>
  fun
  | Hpointsto base sexp te => {
      exp_fav_add fav base;
      strexp_fav_add fav sexp;
      exp_fav_add fav te
    }
  | Hlseg _ _ e1 e2 elist => {
      exp_fav_add fav e1;
      exp_fav_add fav e2;
      IList.iter (exp_fav_add fav) elist
    }
  | Hdllseg _ _ e1 e2 e3 e4 elist => {
      exp_fav_add fav e1;
      exp_fav_add fav e2;
      exp_fav_add fav e3;
      exp_fav_add fav e4;
      IList.iter (exp_fav_add fav) elist
    };

let hpred_fav = fav_imperative_to_functional hpred_fav_add;


/** This function should be used before adding a new
    index to Earray. The [exp] is the newly created
    index. This function "cleans" [exp] according to whether it is
    the footprint or current part of the prop.
    The function faults in the re - execution mode, as an internal check of the tool. */
let array_clean_new_index footprint_part new_idx => {
  if (footprint_part && not !Config.footprint) {
    assert false
  };
  let fav = exp_fav new_idx;
  if (footprint_part && fav_exists fav (fun id => not (Ident.is_footprint id))) {
    L.d_warning (
      "Array index " ^
        exp_to_string new_idx ^ " has non-footprint vars: replaced by fresh footprint var"
    );
    L.d_ln ();
    let id = Ident.create_fresh Ident.kfootprint;
    Var id
  } else {
    new_idx
  }
};


/** {2 Functions for computing all free or bound non-program variables} */
let exp_av_add = exp_fav_add; /** Expressions do not bind variables */

let strexp_av_add = strexp_fav_add; /** Structured expressions do not bind variables */

let rec hpara_av_add fav para => {
  IList.iter (hpred_av_add fav) para.body;
  fav ++ para.root;
  fav ++ para.next;
  fav +++ para.svars;
  fav +++ para.evars
}
and hpara_dll_av_add fav para => {
  IList.iter (hpred_av_add fav) para.body_dll;
  fav ++ para.cell;
  fav ++ para.blink;
  fav ++ para.flink;
  fav +++ para.svars_dll;
  fav +++ para.evars_dll
}
and hpred_av_add fav =>
  fun
  | Hpointsto base se te => {
      exp_av_add fav base;
      strexp_av_add fav se;
      exp_av_add fav te
    }
  | Hlseg _ para e1 e2 elist => {
      hpara_av_add fav para;
      exp_av_add fav e1;
      exp_av_add fav e2;
      IList.iter (exp_av_add fav) elist
    }
  | Hdllseg _ para e1 e2 e3 e4 elist => {
      hpara_dll_av_add fav para;
      exp_av_add fav e1;
      exp_av_add fav e2;
      exp_av_add fav e3;
      exp_av_add fav e4;
      IList.iter (exp_av_add fav) elist
    };

let hpara_shallow_av_add fav para => {
  IList.iter (hpred_fav_add fav) para.body;
  fav ++ para.root;
  fav ++ para.next;
  fav +++ para.svars;
  fav +++ para.evars
};

let hpara_dll_shallow_av_add fav para => {
  IList.iter (hpred_fav_add fav) para.body_dll;
  fav ++ para.cell;
  fav ++ para.blink;
  fav ++ para.flink;
  fav +++ para.svars_dll;
  fav +++ para.evars_dll
};


/** Variables in hpara, excluding bound vars in the body */
let hpara_shallow_av = fav_imperative_to_functional hpara_shallow_av_add;


/** Variables in hpara_dll, excluding bound vars in the body */
let hpara_dll_shallow_av = fav_imperative_to_functional hpara_dll_shallow_av_add;


/** {2 Functions for Substitution} */
let rec reverse_with_base base =>
  fun
  | [] => base
  | [x, ...l] => reverse_with_base [x, ...base] l;

let sorted_list_merge compare l1_in l2_in => {
  let rec merge acc l1 l2 =>
    switch (l1, l2) {
    | ([], l2) => reverse_with_base l2 acc
    | (l1, []) => reverse_with_base l1 acc
    | ([x1, ...l1'], [x2, ...l2']) =>
      if (compare x1 x2 <= 0) {
        merge [x1, ...acc] l1' l2
      } else {
        merge [x2, ...acc] l1 l2'
      }
    };
  merge [] l1_in l2_in
};

let rec sorted_list_check_consecutives f =>
  fun
  | []
  | [_] => false
  | [x1, ...[x2, ..._] as l] =>
    if (f x1 x2) {
      true
    } else {
      sorted_list_check_consecutives f l
    };


/** substitution */
type subst = list (Ident.t, exp);


/** Comparison between substitutions. */
let rec sub_compare (sub1: subst) (sub2: subst) =>
  if (sub1 === sub2) {
    0
  } else {
    switch (sub1, sub2) {
    | ([], []) => 0
    | ([], [_, ..._]) => (-1)
    | ([(i1, e1), ...sub1'], [(i2, e2), ...sub2']) =>
      let n = Ident.compare i1 i2;
      if (n != 0) {
        n
      } else {
        let n = exp_compare e1 e2;
        if (n != 0) {
          n
        } else {
          sub_compare sub1' sub2'
        }
      }
    | ([_, ..._], []) => 1
    }
  };


/** Equality for substitutions. */
let sub_equal sub1 sub2 => sub_compare sub1 sub2 == 0;

let sub_check_duplicated_ids sub => {
  let f (id1, _) (id2, _) => Ident.equal id1 id2;
  sorted_list_check_consecutives f sub
};


/** Create a substitution from a list of pairs.
    For all (id1, e1), (id2, e2) in the input list,
    if id1 = id2, then e1 = e2. */
let sub_of_list sub => {
  let sub' = IList.sort ident_exp_compare sub;
  let sub'' = remove_duplicates_from_sorted ident_exp_equal sub';
  if (sub_check_duplicated_ids sub'') {
    assert false
  };
  sub'
};


/** like sub_of_list, but allow duplicate ids and only keep the first occurrence */
let sub_of_list_duplicates sub => {
  let sub' = IList.sort ident_exp_compare sub;
  let rec remove_duplicate_ids =
    fun
    | [(id1, e1), (id2, e2), ...l] =>
      if (Ident.equal id1 id2) {
        remove_duplicate_ids [(id1, e1), ...l]
      } else {
        [(id1, e1), ...remove_duplicate_ids [(id2, e2), ...l]]
      }
    | l => l;
  remove_duplicate_ids sub'
};


/** Convert a subst to a list of pairs. */
let sub_to_list sub => sub;


/** The empty substitution. */
let sub_empty = sub_of_list [];


/** Join two substitutions into one.
    For all id in dom(sub1) cap dom(sub2), sub1(id) = sub2(id). */
let sub_join sub1 sub2 => {
  let sub = sorted_list_merge ident_exp_compare sub1 sub2;
  let sub' = remove_duplicates_from_sorted ident_exp_equal sub;
  if (sub_check_duplicated_ids sub') {
    assert false
  };
  sub
};


/** Compute the common id-exp part of two inputs [subst1] and [subst2].
    The first component of the output is this common part.
    The second and third components are the remainder of [subst1]
    and [subst2], respectively. */
let sub_symmetric_difference sub1_in sub2_in => {
  let rec diff sub_common sub1_only sub2_only sub1 sub2 =>
    switch (sub1, sub2) {
    | ([], _)
    | (_, []) =>
      let sub1_only' = reverse_with_base sub1 sub1_only;
      let sub2_only' = reverse_with_base sub2 sub2_only;
      let sub_common = reverse_with_base [] sub_common;
      (sub_common, sub1_only', sub2_only')
    | ([id_e1, ...sub1'], [id_e2, ...sub2']) =>
      let n = ident_exp_compare id_e1 id_e2;
      if (n == 0) {
        diff [id_e1, ...sub_common] sub1_only sub2_only sub1' sub2'
      } else if (n < 0) {
        diff sub_common [id_e1, ...sub1_only] sub2_only sub1' sub2
      } else {
        diff sub_common sub1_only [id_e2, ...sub2_only] sub1 sub2'
      }
    };
  diff [] [] [] sub1_in sub2_in
};


/** [sub_find filter sub] returns the expression associated to the first identifier
    that satisfies [filter]. Raise [Not_found] if there isn't one. */
let sub_find filter (sub: subst) => snd (IList.find (fun (i, _) => filter i) sub);


/** [sub_filter filter sub] restricts the domain of [sub] to the
    identifiers satisfying [filter]. */
let sub_filter filter (sub: subst) => IList.filter (fun (i, _) => filter i) sub;


/** [sub_filter_pair filter sub] restricts the domain of [sub] to the
    identifiers satisfying [filter(id, sub(id))]. */
let sub_filter_pair = IList.filter;


/** [sub_range_partition filter sub] partitions [sub] according to
    whether range expressions satisfy [filter]. */
let sub_range_partition filter (sub: subst) => IList.partition (fun (_, e) => filter e) sub;


/** [sub_domain_partition filter sub] partitions [sub] according to
    whether domain identifiers satisfy [filter]. */
let sub_domain_partition filter (sub: subst) => IList.partition (fun (i, _) => filter i) sub;


/** Return the list of identifiers in the domain of the substitution. */
let sub_domain sub => IList.map fst sub;


/** Return the list of expressions in the range of the substitution. */
let sub_range sub => IList.map snd sub;


/** [sub_range_map f sub] applies [f] to the expressions in the range of [sub]. */
let sub_range_map f sub => sub_of_list (IList.map (fun (i, e) => (i, f e)) sub);


/** [sub_map f g sub] applies the renaming [f] to identifiers in the domain
    of [sub] and the substitution [g] to the expressions in the range of [sub]. */
let sub_map f g sub => sub_of_list (IList.map (fun (i, e) => (f i, g e)) sub);

let mem_sub id sub => IList.exists (fun (id1, _) => Ident.equal id id1) sub;


/** Extend substitution and return [None] if not possible. */
let extend_sub sub id exp :option subst => {
  let compare (id1, _) (id2, _) => Ident.compare id1 id2;
  if (mem_sub id sub) {
    None
  } else {
    Some (sorted_list_merge compare sub [(id, exp)])
  }
};


/** Free auxilary variables in the domain and range of the
    substitution. */
let sub_fav_add fav (sub: subst) =>
  IList.iter
    (
      fun (id, e) => {
        fav ++ id;
        exp_fav_add fav e
      }
    )
    sub;

let sub_fpv (sub: subst) => IList.flatten (IList.map (fun (_, e) => exp_fpv e) sub);


/** Substitutions do not contain binders */
let sub_av_add = sub_fav_add;

let rec exp_sub_ids (f: Ident.t => exp) exp =>
  switch exp {
  | Var id => f id
  | Lvar _ => exp
  | Exn e =>
    let e' = exp_sub_ids f e;
    if (e' === e) {
      exp
    } else {
      Exn e'
    }
  | Closure c =>
    let captured_vars =
      IList.map_changed
        (
          fun ((e, pvar, typ) as captured) => {
            let e' = exp_sub_ids f e;
            if (e' === e) {
              captured
            } else {
              (e', pvar, typ)
            }
          }
        )
        c.captured_vars;
    if (captured_vars === c.captured_vars) {
      exp
    } else {
      Closure {...c, captured_vars}
    }
  | Const (Cattribute (Aobjc_null e)) =>
    let e' = exp_sub_ids f e;
    if (e' === e) {
      exp
    } else {
      Const (Cattribute (Aobjc_null e'))
    }
  | Const (Cint _ | Cfun _ | Cstr _ | Cfloat _ | Cattribute _ | Cclass _ | Cptr_to_fld _) => exp
  | Cast t e =>
    let e' = exp_sub_ids f e;
    if (e' === e) {
      exp
    } else {
      Cast t e'
    }
  | UnOp op e typ_opt =>
    let e' = exp_sub_ids f e;
    if (e' === e) {
      exp
    } else {
      UnOp op e' typ_opt
    }
  | BinOp op e1 e2 =>
    let e1' = exp_sub_ids f e1;
    let e2' = exp_sub_ids f e2;
    if (e1' === e1 && e2' === e2) {
      exp
    } else {
      BinOp op e1' e2'
    }
  | Lfield e fld typ =>
    let e' = exp_sub_ids f e;
    if (e' === e) {
      exp
    } else {
      Lfield e' fld typ
    }
  | Lindex e1 e2 =>
    let e1' = exp_sub_ids f e1;
    let e2' = exp_sub_ids f e2;
    if (e1' === e1 && e2' === e2) {
      exp
    } else {
      Lindex e1' e2'
    }
  | Sizeof t l_opt s =>
    switch l_opt {
    | Some l =>
      let l' = exp_sub_ids f l;
      if (l' === l) {
        exp
      } else {
        Sizeof t (Some l') s
      }
    | None => exp
    }
  };

let rec apply_sub subst id =>
  switch subst {
  | [] => Var id
  | [(i, e), ...l] =>
    if (Ident.equal i id) {
      e
    } else {
      apply_sub l id
    }
  };

let exp_sub (subst: subst) e => exp_sub_ids (apply_sub subst) e;


/** apply [f] to id's in [instr]. if [sub_id_binders] is false, [f] is only applied to bound id's */
let instr_sub_ids sub_id_binders::sub_id_binders (f: Ident.t => exp) instr => {
  let sub_id id =>
    switch (exp_sub_ids f (Var id)) {
    | Var id' => id'
    | _ => id
    };
  switch instr {
  | Letderef id rhs_exp typ loc =>
    let id' =
      if sub_id_binders {
        sub_id id
      } else {
        id
      };
    let rhs_exp' = exp_sub_ids f rhs_exp;
    if (id' === id && rhs_exp' === rhs_exp) {
      instr
    } else {
      Letderef id' rhs_exp' typ loc
    }
  | Set lhs_exp typ rhs_exp loc =>
    let lhs_exp' = exp_sub_ids f lhs_exp;
    let rhs_exp' = exp_sub_ids f rhs_exp;
    if (lhs_exp' === lhs_exp && rhs_exp' === rhs_exp) {
      instr
    } else {
      Set lhs_exp' typ rhs_exp' loc
    }
  | Call ret_ids fun_exp actuals call_flags loc =>
    let ret_ids' =
      if sub_id_binders {
        IList.map_changed sub_id ret_ids
      } else {
        ret_ids
      };
    let fun_exp' = exp_sub_ids f fun_exp;
    let actuals' =
      IList.map_changed
        (
          fun ((actual, typ) as actual_pair) => {
            let actual' = exp_sub_ids f actual;
            if (actual' === actual) {
              actual_pair
            } else {
              (actual', typ)
            }
          }
        )
        actuals;
    if (ret_ids' === ret_ids && fun_exp' === fun_exp && actuals' === actuals) {
      instr
    } else {
      Call ret_ids' fun_exp' actuals' call_flags loc
    }
  | Prune exp loc true_branch if_kind =>
    let exp' = exp_sub_ids f exp;
    if (exp' === exp) {
      instr
    } else {
      Prune exp' loc true_branch if_kind
    }
  | Remove_temps ids loc =>
    let ids' = IList.map_changed sub_id ids;
    if (ids' === ids) {
      instr
    } else {
      Remove_temps ids' loc
    }
  | Nullify _
  | Abstract _
  | Declare_locals _
  | Stackop _ => instr
  }
};


/** apply [subst] to all id's in [instr], including binder id's */
let instr_sub (subst: subst) instr => instr_sub_ids sub_id_binders::true (apply_sub subst) instr;

let call_flags_compare cflag1 cflag2 =>
  bool_compare cflag1.cf_virtual cflag2.cf_virtual |>
    next bool_compare cflag1.cf_interface cflag2.cf_interface |>
    next bool_compare cflag1.cf_noreturn cflag2.cf_noreturn |>
    next bool_compare cflag1.cf_is_objc_block cflag2.cf_is_objc_block;

let exp_typ_compare (exp1, typ1) (exp2, typ2) => {
  let n = exp_compare exp1 exp2;
  if (n != 0) {
    n
  } else {
    Typ.compare typ1 typ2
  }
};

let instr_compare instr1 instr2 =>
  switch (instr1, instr2) {
  | (Letderef id1 e1 t1 loc1, Letderef id2 e2 t2 loc2) =>
    let n = Ident.compare id1 id2;
    if (n != 0) {
      n
    } else {
      let n = exp_compare e1 e2;
      if (n != 0) {
        n
      } else {
        let n = Typ.compare t1 t2;
        if (n != 0) {
          n
        } else {
          Location.compare loc1 loc2
        }
      }
    }
  | (Letderef _, _) => (-1)
  | (_, Letderef _) => 1
  | (Set e11 t1 e21 loc1, Set e12 t2 e22 loc2) =>
    let n = exp_compare e11 e12;
    if (n != 0) {
      n
    } else {
      let n = Typ.compare t1 t2;
      if (n != 0) {
        n
      } else {
        let n = exp_compare e21 e22;
        if (n != 0) {
          n
        } else {
          Location.compare loc1 loc2
        }
      }
    }
  | (Set _, _) => (-1)
  | (_, Set _) => 1
  | (Prune cond1 loc1 true_branch1 ik1, Prune cond2 loc2 true_branch2 ik2) =>
    let n = exp_compare cond1 cond2;
    if (n != 0) {
      n
    } else {
      let n = Location.compare loc1 loc2;
      if (n != 0) {
        n
      } else {
        let n = bool_compare true_branch1 true_branch2;
        if (n != 0) {
          n
        } else {
          Pervasives.compare ik1 ik2
        }
      }
    }
  | (Prune _, _) => (-1)
  | (_, Prune _) => 1
  | (Call ret_ids1 e1 arg_ts1 loc1 cf1, Call ret_ids2 e2 arg_ts2 loc2 cf2) =>
    let n = IList.compare Ident.compare ret_ids1 ret_ids2;
    if (n != 0) {
      n
    } else {
      let n = exp_compare e1 e2;
      if (n != 0) {
        n
      } else {
        let n = IList.compare exp_typ_compare arg_ts1 arg_ts2;
        if (n != 0) {
          n
        } else {
          let n = Location.compare loc1 loc2;
          if (n != 0) {
            n
          } else {
            call_flags_compare cf1 cf2
          }
        }
      }
    }
  | (Call _, _) => (-1)
  | (_, Call _) => 1
  | (Nullify pvar1 loc1, Nullify pvar2 loc2) =>
    let n = Pvar.compare pvar1 pvar2;
    if (n != 0) {
      n
    } else {
      Location.compare loc1 loc2
    }
  | (Nullify _, _) => (-1)
  | (_, Nullify _) => 1
  | (Abstract loc1, Abstract loc2) => Location.compare loc1 loc2
  | (Abstract _, _) => (-1)
  | (_, Abstract _) => 1
  | (Remove_temps temps1 loc1, Remove_temps temps2 loc2) =>
    let n = IList.compare Ident.compare temps1 temps2;
    if (n != 0) {
      n
    } else {
      Location.compare loc1 loc2
    }
  | (Remove_temps _, _) => (-1)
  | (_, Remove_temps _) => 1
  | (Stackop stackop1 loc1, Stackop stackop2 loc2) =>
    let n = Pervasives.compare stackop1 stackop2;
    if (n != 0) {
      n
    } else {
      Location.compare loc1 loc2
    }
  | (Stackop _, _) => (-1)
  | (_, Stackop _) => 1
  | (Declare_locals ptl1 loc1, Declare_locals ptl2 loc2) =>
    let pt_compare (pv1, t1) (pv2, t2) => {
      let n = Pvar.compare pv1 pv2;
      if (n != 0) {
        n
      } else {
        Typ.compare t1 t2
      }
    };
    let n = IList.compare pt_compare ptl1 ptl2;
    if (n != 0) {
      n
    } else {
      Location.compare loc1 loc2
    }
  };


/** compare expressions from different procedures without considering loc's, ident's, and pvar's.
    the [exp_map] param gives a mapping of names used in the procedure of [e1] to names used in the
    procedure of [e2] */
let rec exp_compare_structural e1 e2 exp_map => {
  let compare_exps_with_map e1 e2 exp_map =>
    try {
      let e1_mapping = ExpMap.find e1 exp_map;
      (exp_compare e1_mapping e2, exp_map)
    } {
    | Not_found =>
      /* assume e1 and e2 equal, enforce by adding to [exp_map] */
      (0, ExpMap.add e1 e2 exp_map)
    };
  switch (e1, e2) {
  | (Var _, Var _) => compare_exps_with_map e1 e2 exp_map
  | (UnOp o1 e1 to1, UnOp o2 e2 to2) =>
    let n = unop_compare o1 o2;
    if (n != 0) {
      (n, exp_map)
    } else {
      let (n, exp_map) = exp_compare_structural e1 e2 exp_map;
      (
        if (n != 0) {
          n
        } else {
          opt_compare Typ.compare to1 to2
        },
        exp_map
      )
    }
  | (BinOp o1 e1 f1, BinOp o2 e2 f2) =>
    let n = binop_compare o1 o2;
    if (n != 0) {
      (n, exp_map)
    } else {
      let (n, exp_map) = exp_compare_structural e1 e2 exp_map;
      if (n != 0) {
        (n, exp_map)
      } else {
        exp_compare_structural f1 f2 exp_map
      }
    }
  | (Cast t1 e1, Cast t2 e2) =>
    let (n, exp_map) = exp_compare_structural e1 e2 exp_map;
    (
      if (n != 0) {
        n
      } else {
        Typ.compare t1 t2
      },
      exp_map
    )
  | (Lvar _, Lvar _) => compare_exps_with_map e1 e2 exp_map
  | (Lfield e1 f1 t1, Lfield e2 f2 t2) =>
    let (n, exp_map) = exp_compare_structural e1 e2 exp_map;
    (
      if (n != 0) {
        n
      } else {
        let n = Ident.fieldname_compare f1 f2;
        if (n != 0) {
          n
        } else {
          Typ.compare t1 t2
        }
      },
      exp_map
    )
  | (Lindex e1 f1, Lindex e2 f2) =>
    let (n, exp_map) = exp_compare_structural e1 e2 exp_map;
    if (n != 0) {
      (n, exp_map)
    } else {
      exp_compare_structural f1 f2 exp_map
    }
  | _ => (exp_compare e1 e2, exp_map)
  }
};

let exp_typ_compare_structural (e1, t1) (e2, t2) exp_map => {
  let (n, exp_map) = exp_compare_structural e1 e2 exp_map;
  (
    if (n != 0) {
      n
    } else {
      Typ.compare t1 t2
    },
    exp_map
  )
};


/** compare instructions from different procedures without considering loc's, ident's, and pvar's.
    the [exp_map] param gives a mapping of names used in the procedure of [instr1] to identifiers
    used in the procedure of [instr2] */
let instr_compare_structural instr1 instr2 exp_map => {
  let id_list_compare_structural ids1 ids2 exp_map => {
    let n = Pervasives.compare (IList.length ids1) (IList.length ids2);
    if (n != 0) {
      (n, exp_map)
    } else {
      IList.fold_left2
        (
          fun (n, exp_map) id1 id2 =>
            if (n != 0) {
              (n, exp_map)
            } else {
              exp_compare_structural (Var id1) (Var id2) exp_map
            }
        )
        (0, exp_map)
        ids1
        ids2
    }
  };
  switch (instr1, instr2) {
  | (Letderef id1 e1 t1 _, Letderef id2 e2 t2 _) =>
    let (n, exp_map) = exp_compare_structural (Var id1) (Var id2) exp_map;
    if (n != 0) {
      (n, exp_map)
    } else {
      let (n, exp_map) = exp_compare_structural e1 e2 exp_map;
      (
        if (n != 0) {
          n
        } else {
          Typ.compare t1 t2
        },
        exp_map
      )
    }
  | (Set e11 t1 e21 _, Set e12 t2 e22 _) =>
    let (n, exp_map) = exp_compare_structural e11 e12 exp_map;
    if (n != 0) {
      (n, exp_map)
    } else {
      let n = Typ.compare t1 t2;
      if (n != 0) {
        (n, exp_map)
      } else {
        exp_compare_structural e21 e22 exp_map
      }
    }
  | (Prune cond1 _ true_branch1 ik1, Prune cond2 _ true_branch2 ik2) =>
    let (n, exp_map) = exp_compare_structural cond1 cond2 exp_map;
    (
      if (n != 0) {
        n
      } else {
        let n = bool_compare true_branch1 true_branch2;
        if (n != 0) {
          n
        } else {
          Pervasives.compare ik1 ik2
        }
      },
      exp_map
    )
  | (Call ret_ids1 e1 arg_ts1 _ cf1, Call ret_ids2 e2 arg_ts2 _ cf2) =>
    let args_compare_structural args1 args2 exp_map => {
      let n = Pervasives.compare (IList.length args1) (IList.length args2);
      if (n != 0) {
        (n, exp_map)
      } else {
        IList.fold_left2
          (
            fun (n, exp_map) arg1 arg2 =>
              if (n != 0) {
                (n, exp_map)
              } else {
                exp_typ_compare_structural arg1 arg2 exp_map
              }
          )
          (0, exp_map)
          args1
          args2
      }
    };
    let (n, exp_map) = id_list_compare_structural ret_ids1 ret_ids2 exp_map;
    if (n != 0) {
      (n, exp_map)
    } else {
      let (n, exp_map) = exp_compare_structural e1 e2 exp_map;
      if (n != 0) {
        (n, exp_map)
      } else {
        let (n, exp_map) = args_compare_structural arg_ts1 arg_ts2 exp_map;
        (
          if (n != 0) {
            n
          } else {
            call_flags_compare cf1 cf2
          },
          exp_map
        )
      }
    }
  | (Nullify pvar1 _, Nullify pvar2 _) => exp_compare_structural (Lvar pvar1) (Lvar pvar2) exp_map
  | (Abstract _, Abstract _) => (0, exp_map)
  | (Remove_temps temps1 _, Remove_temps temps2 _) =>
    id_list_compare_structural temps1 temps2 exp_map
  | (Stackop stackop1 _, Stackop stackop2 _) => (Pervasives.compare stackop1 stackop2, exp_map)
  | (Declare_locals ptl1 _, Declare_locals ptl2 _) =>
    let n = Pervasives.compare (IList.length ptl1) (IList.length ptl2);
    if (n != 0) {
      (n, exp_map)
    } else {
      IList.fold_left2
        (
          fun (n, exp_map) (pv1, t1) (pv2, t2) =>
            if (n != 0) {
              (n, exp_map)
            } else {
              let (n, exp_map) = exp_compare_structural (Lvar pv1) (Lvar pv2) exp_map;
              if (n != 0) {
                (n, exp_map)
              } else {
                (Typ.compare t1 t2, exp_map)
              }
            }
        )
        (0, exp_map)
        ptl1
        ptl2
    }
  | _ => (instr_compare instr1 instr2, exp_map)
  }
};

let atom_sub subst => atom_expmap (exp_sub subst);

let hpred_sub subst => {
  let f (e, inst_opt) => (exp_sub subst e, inst_opt);
  hpred_expmap f
};


/** {2 Functions for replacing occurrences of expressions.} */
let exp_replace_exp epairs e =>
  try {
    let (_, e') = IList.find (fun (e1, _) => exp_equal e e1) epairs;
    e'
  } {
  | Not_found => e
  };

let atom_replace_exp epairs =>
  fun
  | Aeq e1 e2 => {
      let e1' = exp_replace_exp epairs e1;
      let e2' = exp_replace_exp epairs e2;
      Aeq e1' e2'
    }
  | Aneq e1 e2 => {
      let e1' = exp_replace_exp epairs e1;
      let e2' = exp_replace_exp epairs e2;
      Aneq e1' e2'
    };

let rec strexp_replace_exp epairs =>
  fun
  | Eexp e inst => Eexp (exp_replace_exp epairs e) inst
  | Estruct fsel inst => {
      let f (fld, se) => (fld, strexp_replace_exp epairs se);
      Estruct (IList.map f fsel) inst
    }
  | Earray len isel inst => {
      let len' = exp_replace_exp epairs len;
      let f (idx, se) => {
        let idx' = exp_replace_exp epairs idx;
        (idx', strexp_replace_exp epairs se)
      };
      Earray len' (IList.map f isel) inst
    };

let hpred_replace_exp epairs =>
  fun
  | Hpointsto root se te => {
      let root_repl = exp_replace_exp epairs root;
      let strexp_repl = strexp_replace_exp epairs se;
      let te_repl = exp_replace_exp epairs te;
      Hpointsto root_repl strexp_repl te_repl
    }
  | Hlseg k para root next shared => {
      let root_repl = exp_replace_exp epairs root;
      let next_repl = exp_replace_exp epairs next;
      let shared_repl = IList.map (exp_replace_exp epairs) shared;
      Hlseg k para root_repl next_repl shared_repl
    }
  | Hdllseg k para e1 e2 e3 e4 shared => {
      let e1' = exp_replace_exp epairs e1;
      let e2' = exp_replace_exp epairs e2;
      let e3' = exp_replace_exp epairs e3;
      let e4' = exp_replace_exp epairs e4;
      let shared_repl = IList.map (exp_replace_exp epairs) shared;
      Hdllseg k para e1' e2' e3' e4' shared_repl
    };


/** {2 Compaction} */
let module ExpHash = Hashtbl.Make {
  type t = exp;
  let equal = exp_equal;
  let hash = Hashtbl.hash;
};

let module HpredHash = Hashtbl.Make {
  type t = hpred;
  let equal = hpred_equal;
  let hash = Hashtbl.hash;
};

type sharing_env = {exph: ExpHash.t exp, hpredh: HpredHash.t hpred};


/** Create a sharing env to store canonical representations */
let create_sharing_env () => {exph: ExpHash.create 3, hpredh: HpredHash.create 3};


/** Return a canonical representation of the exp */
let exp_compact sh e =>
  try (ExpHash.find sh.exph e) {
  | Not_found =>
    ExpHash.add sh.exph e e;
    e
  };

let rec sexp_compact sh se =>
  switch se {
  | Eexp e inst => Eexp (exp_compact sh e) inst
  | Estruct fsel inst => Estruct (IList.map (fun (f, se) => (f, sexp_compact sh se)) fsel) inst
  | Earray _ => se
  };


/** Return a compact representation of the hpred */
let _hpred_compact sh hpred =>
  switch hpred {
  | Hpointsto e1 se e2 =>
    let e1' = exp_compact sh e1;
    let e2' = exp_compact sh e2;
    let se' = sexp_compact sh se;
    Hpointsto e1' se' e2'
  | Hlseg _ => hpred
  | Hdllseg _ => hpred
  };

let hpred_compact sh hpred =>
  try (HpredHash.find sh.hpredh hpred) {
  | Not_found =>
    let hpred' = _hpred_compact sh hpred;
    HpredHash.add sh.hpredh hpred' hpred';
    hpred'
  };


/** {2 Functions for constructing or destructing entities in this module} */
/** Extract the ids and pvars from an expression */
let exp_get_vars exp => {
  let rec exp_get_vars_ exp vars =>
    switch exp {
    | Lvar pvar => (fst vars, [pvar, ...snd vars])
    | Var id => ([id, ...fst vars], snd vars)
    | Cast _ e
    | UnOp _ e _
    | Lfield e _ _
    | Exn e => exp_get_vars_ e vars
    | BinOp _ e1 e2
    | Lindex e1 e2 => exp_get_vars_ e1 vars |> exp_get_vars_ e2
    | Closure {captured_vars} =>
      IList.fold_left
        (fun vars_acc (captured_exp, _, _) => exp_get_vars_ captured_exp vars_acc)
        vars
        captured_vars
    | Const (Cint _ | Cfun _ | Cstr _ | Cfloat _ | Cattribute _ | Cclass _ | Cptr_to_fld _) => vars
    /* TODO: Sizeof length expressions may contain variables, do not ignore them. */
    /* | Sizeof _ None _ => vars */
    /* | Sizeof _ (Some l) _ => exp_get_vars_ l vars */
    | Sizeof _ _ _ => vars
    };
  exp_get_vars_ exp ([], [])
};


/** Compute the offset list of an expression */
let exp_get_offsets exp => {
  let rec f offlist_past e =>
    switch e {
    | Var _
    | Const _
    | UnOp _
    | BinOp _
    | Exn _
    | Closure _
    | Lvar _
    | Sizeof _ None _ => offlist_past
    | Sizeof _ (Some l) _ => f offlist_past l
    | Cast _ sub_exp => f offlist_past sub_exp
    | Lfield sub_exp fldname typ => f [Off_fld fldname typ, ...offlist_past] sub_exp
    | Lindex sub_exp e => f [Off_index e, ...offlist_past] sub_exp
    };
  f [] exp
};

let exp_add_offsets exp offsets => {
  let rec f acc =>
    fun
    | [] => acc
    | [Off_fld fld typ, ...offs'] => f (Lfield acc fld typ) offs'
    | [Off_index e, ...offs'] => f (Lindex acc e) offs';
  f exp offsets
};


/** Convert all the lseg's in sigma to nonempty lsegs. */
let sigma_to_sigma_ne sigma :list (list atom, list hpred) =>
  if Config.nelseg {
    let f eqs_sigma_list hpred =>
      switch hpred {
      | Hpointsto _
      | Hlseg Lseg_NE _ _ _ _
      | Hdllseg Lseg_NE _ _ _ _ _ _ =>
        let g (eqs, sigma) => (eqs, [hpred, ...sigma]);
        IList.map g eqs_sigma_list
      | Hlseg Lseg_PE para e1 e2 el =>
        let g (eqs, sigma) => [
          ([Aeq e1 e2, ...eqs], sigma),
          (eqs, [Hlseg Lseg_NE para e1 e2 el, ...sigma])
        ];
        IList.flatten (IList.map g eqs_sigma_list)
      | Hdllseg Lseg_PE para_dll e1 e2 e3 e4 el =>
        let g (eqs, sigma) => [
          ([Aeq e1 e3, Aeq e2 e4, ...eqs], sigma),
          (eqs, [Hdllseg Lseg_NE para_dll e1 e2 e3 e4 el, ...sigma])
        ];
        IList.flatten (IList.map g eqs_sigma_list)
      };
    IList.fold_left f [([], [])] sigma
  } else {
    [([], sigma)]
  };


/** [hpara_instantiate para e1 e2 elist] instantiates [para] with [e1],
    [e2] and [elist]. If [para = lambda (x, y, xs). exists zs. b],
    then the result of the instantiation is [b\[e1 / x, e2 / y, elist / xs, _zs'/ zs\]]
    for some fresh [_zs'].*/
let hpara_instantiate para e1 e2 elist => {
  let subst_for_svars = {
    let g id e => (id, e);
    try (IList.map2 g para.svars elist) {
    | Invalid_argument _ => assert false
    }
  };
  let ids_evars = {
    let g _ => Ident.create_fresh Ident.kprimed;
    IList.map g para.evars
  };
  let subst_for_evars = {
    let g id id' => (id, Var id');
    try (IList.map2 g para.evars ids_evars) {
    | Invalid_argument _ => assert false
    }
  };
  let subst = sub_of_list (
    [(para.root, e1), (para.next, e2), ...subst_for_svars] @ subst_for_evars
  );
  (ids_evars, IList.map (hpred_sub subst) para.body)
};


/** [hpara_dll_instantiate para cell blink flink  elist] instantiates [para] with [cell],
    [blink], [flink], and [elist]. If [para = lambda (x, y, z, xs). exists zs. b],
    then the result of the instantiation is
    [b\[cell / x, blink / y, flink / z, elist / xs, _zs'/ zs\]]
    for some fresh [_zs'].*/
let hpara_dll_instantiate (para: hpara_dll) cell blink flink elist => {
  let subst_for_svars = {
    let g id e => (id, e);
    try (IList.map2 g para.svars_dll elist) {
    | Invalid_argument _ => assert false
    }
  };
  let ids_evars = {
    let g _ => Ident.create_fresh Ident.kprimed;
    IList.map g para.evars_dll
  };
  let subst_for_evars = {
    let g id id' => (id, Var id');
    try (IList.map2 g para.evars_dll ids_evars) {
    | Invalid_argument _ => assert false
    }
  };
  let subst = sub_of_list (
    [(para.cell, cell), (para.blink, blink), (para.flink, flink), ...subst_for_svars] @ subst_for_evars
  );
  (ids_evars, IList.map (hpred_sub subst) para.body_dll)
};

let custom_error = Pvar.mk_global (Mangled.from_string "INFER_CUSTOM_ERROR");
