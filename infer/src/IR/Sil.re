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

/** Kind of prune instruction */
type if_kind =
  | Ik_bexp /* boolean expressions, and exp ? exp : exp */
  | Ik_dowhile
  | Ik_for
  | Ik_if
  | Ik_land_lor /* obtained from translation of && or || */
  | Ik_while
  | Ik_switch;


/** An instruction. */
type instr =
  /** Load a value from the heap into an identifier.
      [x = *lexp:typ] where
        [lexp] is an expression denoting a heap address
        [typ] is the root type of [lexp]. */
  /* Note for frontend writers:
     [x] must be used in a subsequent instruction, otherwise the entire
     `Load` instruction may be eliminated by copy-propagation. */
  | Load Ident.t Exp.t Typ.t Location.t
  /** Store the value of an expression into the heap.
      [*lexp1:typ = exp2] where
        [lexp1] is an expression denoting a heap address
        [typ] is the root type of [lexp1]
        [exp2] is the expression whose value is store. */
  | Store Exp.t Typ.t Exp.t Location.t
  /** prune the state based on [exp=1], the boolean indicates whether true branch */
  | Prune Exp.t Location.t bool if_kind
  /** [Call (ret_id, e_fun, arg_ts, loc, call_flags)] represents an instruction
      [ret_id = e_fun(arg_ts);]. The return value is ignored when [ret_id = None]. */
  | Call (option (Ident.t, Typ.t)) Exp.t (list (Exp.t, Typ.t)) Location.t CallFlags.t
  /** nullify stack variable */
  | Nullify Pvar.t Location.t
  | Abstract Location.t /** apply abstraction */
  | Remove_temps (list Ident.t) Location.t /** remove temporaries */
  | Declare_locals (list (Pvar.t, Typ.t)) Location.t /** declare local variables */;

let skip_instr = Remove_temps [] Location.dummy;


/** Check if an instruction is auxiliary, or if it comes from source instructions. */
let instr_is_auxiliary =
  fun
  | Load _
  | Store _
  | Prune _
  | Call _ => false
  | Nullify _
  | Abstract _
  | Remove_temps _
  | Declare_locals _ => true;


/** offset for an lvalue */
type offset =
  | Off_fld Ident.fieldname Typ.t
  | Off_index Exp.t;


/** {2 Components of Propositions} */

/** an atom is a pure atomic formula */
type atom =
  | Aeq Exp.t Exp.t /** equality */
  | Aneq Exp.t Exp.t /** disequality */
  | Apred PredSymb.t (list Exp.t) /** predicate symbol applied to exps */
  | Anpred PredSymb.t (list Exp.t) /** negated predicate symbol applied to exps */;


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
  | Iformal zero_flag null_case_flag
  | Iinitial
  | Ilookup
  | Inone
  | Inullify
  | Irearrange zero_flag null_case_flag int PredSymb.path_pos
  | Itaint
  | Iupdate zero_flag null_case_flag int PredSymb.path_pos
  | Ireturn_from_call int;


/** structured expressions represent a value of structured type, such as an array or a struct. */
type strexp =
  | Eexp Exp.t inst /** Base case: expression with instrumentation */
  | Estruct (list (Ident.fieldname, strexp)) inst /** C structure */
  /** Array of given length
      There are two conditions imposed / used in the array case.
      First, if some index and value pair appears inside an array
      in a strexp, then the index is less than the length of the array.
      For instance, x |->[10 | e1: v1] implies that e1 <= 9.
      Second, if two indices appear in an array, they should be different.
      For instance, x |->[10 | e1: v1, e2: v2] implies that e1 != e2. */
  | Earray Exp.t (list (Exp.t, strexp)) inst;


/** an atomic heap predicate */
type hpred =
  | Hpointsto Exp.t strexp Exp.t
  /** represents [exp|->strexp:typexp] where [typexp]
      is an expression representing a type, e.h. [sizeof(t)]. */
  | Hlseg lseg_kind hpara Exp.t Exp.t (list Exp.t)
  /** higher - order predicate for singly - linked lists.
      Should ensure that exp1!= exp2 implies that exp1 is allocated.
      This assumption is used in the rearrangement. The last [exp list] parameter
      is used to denote the shared links by all the nodes in the list. */
  | Hdllseg lseg_kind hpara_dll Exp.t Exp.t Exp.t Exp.t (list Exp.t)
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
let has_objc_ref_counter tenv hpred =>
  switch hpred {
  | Hpointsto _ _ (Sizeof (Tstruct name) _ _) =>
    switch (Tenv.lookup tenv name) {
    | Some {fields} => IList.exists StructTyp.is_objc_ref_counter_field fields
    | _ => false
    }
  | _ => false
  };


/** Returns the zero value of a type, for int, float and ptr types, None othwewise */
let zero_value_of_numerical_type_option typ =>
  switch typ {
  | Typ.Tint _ => Some (Exp.Const (Cint IntLit.zero))
  | Typ.Tfloat _ => Some (Exp.Const (Cfloat 0.0))
  | Typ.Tptr _ => Some (Exp.Const (Cint IntLit.null))
  | _ => None
  };


/** Returns the zero value of a type, for int, float and ptr types, fail otherwise */
let zero_value_of_numerical_type typ => Option.get (zero_value_of_numerical_type_option typ);


/** Make a static local name in objc */
let mk_static_local_name pname vname => pname ^ "_" ^ vname;


/** Check if a pvar is a local static in objc */
let is_static_local_name pname pvar => {
  /* local static name is of the form procname_varname */
  let var_name = Mangled.to_string (Pvar.get_name pvar);
  switch (Str.split_delim (Str.regexp_string pname) var_name) {
  | [_, _] => true
  | _ => false
  }
};

let ident_exp_compare = pair_compare Ident.compare Exp.compare;

let ident_exp_equal ide1 ide2 => ident_exp_compare ide1 ide2 == 0;

let exp_list_compare = IList.compare Exp.compare;


/** Compare atoms. Equalities come before disequalities */
let atom_compare a b =>
  if (a === b) {
    0
  } else {
    switch (a, b) {
    | (Aeq e1 e2, Aeq f1 f2) =>
      let n = Exp.compare e1 f1;
      if (n != 0) {
        n
      } else {
        Exp.compare e2 f2
      }
    | (Aeq _, _) => (-1)
    | (_, Aeq _) => 1
    | (Aneq e1 e2, Aneq f1 f2) =>
      let n = Exp.compare e1 f1;
      if (n != 0) {
        n
      } else {
        Exp.compare e2 f2
      }
    | (Aneq _, _) => (-1)
    | (_, Aneq _) => 1
    | (Apred a1 es1, Apred a2 es2) =>
      let n = PredSymb.compare a1 a2;
      if (n != 0) {
        n
      } else {
        IList.compare Exp.compare es1 es2
      }
    | (Apred _, _) => (-1)
    | (_, Apred _) => 1
    | (Anpred a1 es1, Anpred a2 es2) =>
      let n = PredSymb.compare a1 a2;
      if (n != 0) {
        n
      } else {
        IList.compare Exp.compare es1 es2
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
    | (Eexp e1 _, Eexp e2 _) => Exp.compare e1 e2
    | (Eexp _, _) => (-1)
    | (_, Eexp _) => 1
    | (Estruct fel1 _, Estruct fel2 _) => fld_strexp_list_compare fel1 fel2
    | (Estruct _, _) => (-1)
    | (_, Estruct _) => 1
    | (Earray e1 esel1 _, Earray e2 esel2 _) =>
      let n = Exp.compare e1 e2;
      if (n != 0) {
        n
      } else {
        exp_strexp_list_compare esel1 esel2
      }
    }
  }
and fld_strexp_compare fse1 fse2 => pair_compare Ident.fieldname_compare strexp_compare fse1 fse2
and fld_strexp_list_compare fsel1 fsel2 => IList.compare fld_strexp_compare fsel1 fsel2
and exp_strexp_compare ese1 ese2 => pair_compare Exp.compare strexp_compare ese1 ese2
and exp_strexp_list_compare esel1 esel2 => IList.compare exp_strexp_compare esel1 esel2;


/** Comparsion between heap predicates. Hpointsto comes before others. */
let rec hpred_compare hpred1 hpred2 =>
  if (hpred1 === hpred2) {
    0
  } else {
    switch (hpred1, hpred2) {
    | (Hpointsto e1 _ _, Hlseg _ _ e2 _ _) when Exp.compare e2 e1 != 0 => Exp.compare e2 e1
    | (Hpointsto e1 _ _, Hdllseg _ _ e2 _ _ _ _) when Exp.compare e2 e1 != 0 => Exp.compare e2 e1
    | (Hlseg _ _ e1 _ _, Hpointsto e2 _ _) when Exp.compare e2 e1 != 0 => Exp.compare e2 e1
    | (Hlseg _ _ e1 _ _, Hdllseg _ _ e2 _ _ _ _) when Exp.compare e2 e1 != 0 => Exp.compare e2 e1
    | (Hdllseg _ _ e1 _ _ _ _, Hpointsto e2 _ _) when Exp.compare e2 e1 != 0 => Exp.compare e2 e1
    | (Hdllseg _ _ e1 _ _ _ _, Hlseg _ _ e2 _ _) when Exp.compare e2 e1 != 0 => Exp.compare e2 e1
    | (Hpointsto e1 se1 te1, Hpointsto e2 se2 te2) =>
      let n = Exp.compare e2 e1;
      if (n != 0) {
        n
      } else {
        let n = strexp_compare se2 se1;
        if (n != 0) {
          n
        } else {
          Exp.compare te2 te1
        }
      }
    | (Hpointsto _, _) => (-1)
    | (_, Hpointsto _) => 1
    | (Hlseg k1 hpar1 e1 f1 el1, Hlseg k2 hpar2 e2 f2 el2) =>
      let n = Exp.compare e2 e1;
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
            let n = Exp.compare f2 f1;
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
      let n = Exp.compare e2 e1;
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
            let n = Exp.compare f2 f1;
            if (n != 0) {
              n
            } else {
              let n = Exp.compare g2 g1;
              if (n != 0) {
                n
              } else {
                let n = Exp.compare h2 h1;
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
let elist_to_eset es => IList.fold_left (fun set e => Exp.Set.add e set) Exp.Set.empty es;


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


/** Pretty print an expression. */
let pp_exp_printenv pe0 f e0 => {
  let (pe, changed) = color_pre_wrapper pe0 f e0;
  let e =
    switch pe.pe_obj_sub {
    | Some sub => Obj.obj (sub (Obj.repr e0)) /* apply object substitution to expression */
    | None => e0
    };
  if (not (Exp.equal e0 e)) {
    switch e {
    | Exp.Lvar pvar => Pvar.pp_value pe f pvar
    | _ => assert false
    }
  } else {
    Exp.pp_printenv pe Typ.pp f e
  };
  color_post_wrapper changed pe0 f
};


/** dump an expression. */
let d_exp (e: Exp.t) => L.add_print_action (L.PTexp, Obj.repr e);


/** Pretty print a list of expressions. */
let pp_exp_list pe f expl => (pp_seq (pp_exp_printenv pe)) f expl;


/** dump a list of expressions. */
let d_exp_list (el: list Exp.t) => L.add_print_action (L.PTexp_list, Obj.repr el);

let pp_texp pe f =>
  fun
  | Exp.Sizeof t l s => {
      let pp_len f l => Option.map_default (F.fprintf f "[%a]" (pp_exp_printenv pe)) () l;
      F.fprintf f "%a%a%a" (Typ.pp pe) t pp_len l Subtype.pp s
    }
  | e => (pp_exp_printenv pe) f e;


/** Pretty print a type with all the details. */
let pp_texp_full pe f =>
  fun
  | Exp.Sizeof t l s => {
      let pp_len f l => Option.map_default (F.fprintf f "[%a]" (pp_exp_printenv pe)) () l;
      F.fprintf f "%a%a%a" (Typ.pp_full pe) t pp_len l Subtype.pp s
    }
  | e => Exp.pp_printenv pe Typ.pp_full f e;


/** Dump a type expression with all the details. */
let d_texp_full (te: Exp.t) => L.add_print_action (L.PTtexp_full, Obj.repr te);


/** Pretty print an offset */
let pp_offset pe f =>
  fun
  | Off_fld fld _ => F.fprintf f "%a" Ident.pp_fieldname fld
  | Off_index exp => F.fprintf f "%a" (pp_exp_printenv pe) exp;


/** Convert an offset to a string */
let offset_to_string e => pp_to_string (pp_offset pe_text) e;


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

let pp_exp_typ pe f (e, t) => F.fprintf f "%a:%a" (pp_exp_printenv pe) e (Typ.pp pe) t;


/** Get the location of the instruction */
let instr_get_loc =
  fun
  | Load _ _ _ loc
  | Store _ _ _ loc
  | Prune _ loc _ _
  | Call _ _ _ loc _
  | Nullify _ loc
  | Abstract loc
  | Remove_temps _ loc
  | Declare_locals _ loc => loc;


/** get the expressions occurring in the instruction */
let instr_get_exps =
  fun
  | Load id e _ _ => [Exp.Var id, e]
  | Store e1 _ e2 _ => [e1, e2]
  | Prune cond _ _ _ => [cond]
  | Call ret_id e _ _ _ => [e, ...Option.map_default (fun (id, _) => [Exp.Var id]) [] ret_id]
  | Nullify pvar _ => [Exp.Lvar pvar]
  | Abstract _ => []
  | Remove_temps temps _ => IList.map (fun id => Exp.Var id) temps
  | Declare_locals _ => [];


/** Pretty print an instruction. */
let pp_instr pe0 f instr => {
  let (pe, changed) = color_pre_wrapper pe0 f instr;
  switch instr {
  | Load id e t loc =>
    F.fprintf
      f "%a=*%a:%a %a" (Ident.pp pe) id (pp_exp_printenv pe) e (Typ.pp pe) t Location.pp loc
  | Store e1 t e2 loc =>
    F.fprintf
      f
      "*%a:%a=%a %a"
      (pp_exp_printenv pe)
      e1
      (Typ.pp pe)
      t
      (pp_exp_printenv pe)
      e2
      Location.pp
      loc
  | Prune cond loc true_branch _ =>
    F.fprintf f "PRUNE(%a, %b); %a" (pp_exp_printenv pe) cond true_branch Location.pp loc
  | Call ret_id e arg_ts loc cf =>
    switch ret_id {
    | None => ()
    | Some (id, _) => F.fprintf f "%a=" (Ident.pp pe) id
    };
    F.fprintf
      f
      "%a(%a)%a %a"
      (pp_exp_printenv pe)
      e
      (pp_comma_seq (pp_exp_typ pe))
      arg_ts
      CallFlags.pp
      cf
      Location.pp
      loc
  | Nullify pvar loc => F.fprintf f "NULLIFY(%a); %a" (Pvar.pp pe) pvar Location.pp loc
  | Abstract loc => F.fprintf f "APPLY_ABSTRACTION; %a" Location.pp loc
  | Remove_temps temps loc =>
    F.fprintf f "REMOVE_TEMPS(%a); %a" (Ident.pp_list pe) temps Location.pp loc
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
    | PP_HTML => F.fprintf f "%a" (pp_exp_printenv pe) (Exp.BinOp op e1 e2)
    | PP_LATEX => F.fprintf f "%a" (pp_exp_printenv pe) (Exp.BinOp op e1 e2)
    }
  | Aeq e1 e2 =>
    switch pe.pe_kind {
    | PP_TEXT
    | PP_HTML => F.fprintf f "%a = %a" (pp_exp_printenv pe) e1 (pp_exp_printenv pe) e2
    | PP_LATEX => F.fprintf f "%a{=}%a" (pp_exp_printenv pe) e1 (pp_exp_printenv pe) e2
    }
  | Aneq e1 e2 =>
    switch pe.pe_kind {
    | PP_TEXT
    | PP_HTML => F.fprintf f "%a != %a" (pp_exp_printenv pe) e1 (pp_exp_printenv pe) e2
    | PP_LATEX => F.fprintf f "%a{\\neq}%a" (pp_exp_printenv pe) e1 (pp_exp_printenv pe) e2
    }
  | Apred a es =>
    F.fprintf f "%s(%a)" (PredSymb.to_string pe a) (pp_comma_seq (pp_exp_printenv pe)) es
  | Anpred a es =>
    F.fprintf f "!%s(%a)" (PredSymb.to_string pe a) (pp_comma_seq (pp_exp_printenv pe)) es
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
  };


/** return a string representing the inst */
let inst_to_string inst => {
  let zero_flag_to_string =
    fun
    | Some true => "(z)"
    | _ => "";
  let null_case_flag_to_string ncf => if ncf {"(ncf)"} else {""};
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
  | Ireturn_from_call _ => None;


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
  }
};


/** describe an instrumentation with a string */
let pp_inst pe f inst => {
  let str = inst_to_string inst;
  if (pe.pe_kind === PP_HTML) {
    F.fprintf f " %a%s%a" Io_infer.Html.pp_start_color Orange str Io_infer.Html.pp_end_color ()
  } else {
    F.fprintf f "%s%s%s" (Binop.str pe Lt) str (Binop.str pe Gt)
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
  | Eexp e inst => F.fprintf f "%a%a" (pp_exp_printenv pe) e (pp_inst_if_trace pe) inst
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
    let pp_diff f (i, se) => F.fprintf f "%a:%a" (pp_exp_printenv pe) i (pp_sexp_env pe envo) se;
    F.fprintf
      f
      "[%a|%a]%a"
      (pp_exp_printenv pe)
      len
      (pp_seq_diff pp_diff pe)
      nel
      (pp_inst_if_trace pe)
      inst
  };
  color_post_wrapper changed pe0 f
};


/** Pretty print an hpred with an optional predicate env */
let rec pp_hpred_env pe0 envo f hpred => {
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
      F.fprintf
        f "%a|->%a:%a" (pp_exp_printenv pe') e (pp_sexp_env pe' envo) se (pp_texp_simple pe') te
    | PP_LATEX => F.fprintf f "%a\\mapsto %a" (pp_exp_printenv pe') e (pp_sexp_env pe' envo) se
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
        (pp_exp_printenv pe)
        e1
        (pp_exp_printenv pe)
        e2
        (pp_comma_seq (pp_exp_printenv pe))
        elist
        (pp_hpara_env pe envo)
        hpara
    | PP_LATEX =>
      F.fprintf
        f
        "\\textsf{lseg}_{%a}(%a,%a,[%a],%a)"
        pp_lseg_kind
        k
        (pp_exp_printenv pe)
        e1
        (pp_exp_printenv pe)
        e2
        (pp_comma_seq (pp_exp_printenv pe))
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
        (pp_exp_printenv pe)
        iF
        (pp_exp_printenv pe)
        oB
        (pp_exp_printenv pe)
        oF
        (pp_exp_printenv pe)
        iB
        (pp_comma_seq (pp_exp_printenv pe))
        elist
        (pp_hpara_dll_env pe envo)
        hpara_dll
    | PP_LATEX =>
      F.fprintf
        f
        "\\textsf{dllseg}_{%a}(%a,%a,%a,%a,[%a],%a)"
        pp_lseg_kind
        k
        (pp_exp_printenv pe)
        iF
        (pp_exp_printenv pe)
        oB
        (pp_exp_printenv pe)
        oF
        (pp_exp_printenv pe)
        iB
        (pp_comma_seq (pp_exp_printenv pe))
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
let rec strexp_expmap (f: (Exp.t, option inst) => (Exp.t, option inst)) => {
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

let hpred_expmap (f: (Exp.t, option inst) => (Exp.t, option inst)) => {
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
  };

let rec hpara_instmap (f: inst => inst) hpara => {
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

let hpred_list_expmap (f: (Exp.t, option inst) => (Exp.t, option inst)) (hlist: list hpred) =>
  IList.map (hpred_expmap f) hlist;

let atom_expmap (f: Exp.t => Exp.t) =>
  fun
  | Aeq e1 e2 => Aeq (f e1) (f e2)
  | Aneq e1 e2 => Aneq (f e1) (f e2)
  | Apred a es => Apred a (IList.map f es)
  | Anpred a es => Anpred a (IList.map f es);

let atom_list_expmap (f: Exp.t => Exp.t) (alist: list atom) => IList.map (atom_expmap f) alist;


/** {2 Function for computing lexps in sigma} */
let hpred_get_lexp acc =>
  fun
  | Hpointsto e _ _ => [e, ...acc]
  | Hlseg _ _ e _ _ => [e, ...acc]
  | Hdllseg _ _ e1 _ _ e2 _ => [e1, e2, ...acc];

let hpred_list_get_lexps (filter: Exp.t => bool) (hlist: list hpred) :list Exp.t => {
  let lexps = IList.fold_left hpred_get_lexp [] hlist;
  IList.filter filter lexps
};


/** {2 Functions for computing program variables} */
let rec exp_fpv e =>
  switch (e: Exp.t) {
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
  };

let exp_list_fpv el => IList.flatten (IList.map exp_fpv el);

let atom_fpv =
  fun
  | Aeq e1 e2 => exp_fpv e1 @ exp_fpv e2
  | Aneq e1 e2 => exp_fpv e1 @ exp_fpv e2
  | Apred _ es
  | Anpred _ es => IList.fold_left (fun fpv e => IList.rev_append (exp_fpv e) fpv) [] es;

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
    };

let rec hpred_fpv =
  fun
  | Hpointsto base se te => exp_fpv base @ strexp_fpv se @ exp_fpv te
  | Hlseg _ para e1 e2 elist => {
      let fpvars_in_elist = exp_list_fpv elist;
      hpara_fpv para @ /* This set has to be empty. */ exp_fpv e1 @ exp_fpv e2 @ fpvars_in_elist
    }
  | Hdllseg _ para e1 e2 e3 e4 elist => {
      let fpvars_in_elist = exp_list_fpv elist;
      hpara_dll_fpv para @
      /* This set has to be empty. */
      exp_fpv e1 @ exp_fpv e2 @ exp_fpv e3 @ exp_fpv e4 @ fpvars_in_elist
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

let rec exp_fav_add fav e =>
  switch (e: Exp.t) {
  | Var id => fav ++ id
  | Exn e => exp_fav_add fav e
  | Closure {captured_vars} => IList.iter (fun (e, _, _) => exp_fav_add fav e) captured_vars
  | Const (Cint _ | Cfun _ | Cstr _ | Cfloat _ | Cclass _ | Cptr_to_fld _) => ()
  | Cast _ e
  | UnOp _ e _ => exp_fav_add fav e
  | BinOp _ e1 e2 =>
    exp_fav_add fav e1;
    exp_fav_add fav e2
  | Lvar _ => () /* do nothing since we only count non-program variables */
  | Lfield e _ _ => exp_fav_add fav e
  | Lindex e1 e2 =>
    exp_fav_add fav e1;
    exp_fav_add fav e2
  /* TODO: Sizeof length expressions may contain variables, do not ignore them. */
  /* | Sizeof _ None _ => () */
  /* | Sizeof _ (Some l) _ => exp_fav_add fav l; */
  | Sizeof _ _ _ => ()
  };

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
    }
  | Apred _ es
  | Anpred _ es => IList.iter (fun e => exp_fav_add fav e) es;

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
      Exp.to_string new_idx ^ " has non-footprint vars: replaced by fresh footprint var"
    );
    L.d_ln ();
    let id = Ident.create_fresh Ident.kfootprint;
    Exp.Var id
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
type subst = list (Ident.t, Exp.t);


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
        let n = Exp.compare e1 e2;
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

let rec exp_sub_ids (f: Ident.t => Exp.t) exp =>
  switch (exp: Exp.t) {
  | Var id => f id
  | Lvar _ => exp
  | Exn e =>
    let e' = exp_sub_ids f e;
    if (e' === e) {
      exp
    } else {
      Exp.Exn e'
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
      Exp.Closure {...c, captured_vars}
    }
  | Const (Cint _ | Cfun _ | Cstr _ | Cfloat _ | Cclass _ | Cptr_to_fld _) => exp
  | Cast t e =>
    let e' = exp_sub_ids f e;
    if (e' === e) {
      exp
    } else {
      Exp.Cast t e'
    }
  | UnOp op e typ_opt =>
    let e' = exp_sub_ids f e;
    if (e' === e) {
      exp
    } else {
      Exp.UnOp op e' typ_opt
    }
  | BinOp op e1 e2 =>
    let e1' = exp_sub_ids f e1;
    let e2' = exp_sub_ids f e2;
    if (e1' === e1 && e2' === e2) {
      exp
    } else {
      Exp.BinOp op e1' e2'
    }
  | Lfield e fld typ =>
    let e' = exp_sub_ids f e;
    if (e' === e) {
      exp
    } else {
      Exp.Lfield e' fld typ
    }
  | Lindex e1 e2 =>
    let e1' = exp_sub_ids f e1;
    let e2' = exp_sub_ids f e2;
    if (e1' === e1 && e2' === e2) {
      exp
    } else {
      Exp.Lindex e1' e2'
    }
  | Sizeof t l_opt s =>
    switch l_opt {
    | Some l =>
      let l' = exp_sub_ids f l;
      if (l' === l) {
        exp
      } else {
        Exp.Sizeof t (Some l') s
      }
    | None => exp
    }
  };

let rec apply_sub subst id =>
  switch subst {
  | [] => Exp.Var id
  | [(i, e), ...l] =>
    if (Ident.equal i id) {
      e
    } else {
      apply_sub l id
    }
  };

let exp_sub (subst: subst) e => exp_sub_ids (apply_sub subst) e;


/** apply [f] to id's in [instr]. if [sub_id_binders] is false, [f] is only applied to bound id's */
let instr_sub_ids sub_id_binders::sub_id_binders (f: Ident.t => Exp.t) instr => {
  let sub_id id =>
    switch (exp_sub_ids f (Var id)) {
    | Var id' when not (Ident.equal id id') => id'
    | _ => id
    };
  switch instr {
  | Load id rhs_exp typ loc =>
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
      Load id' rhs_exp' typ loc
    }
  | Store lhs_exp typ rhs_exp loc =>
    let lhs_exp' = exp_sub_ids f lhs_exp;
    let rhs_exp' = exp_sub_ids f rhs_exp;
    if (lhs_exp' === lhs_exp && rhs_exp' === rhs_exp) {
      instr
    } else {
      Store lhs_exp' typ rhs_exp' loc
    }
  | Call ret_id fun_exp actuals call_flags loc =>
    let ret_id' =
      if sub_id_binders {
        switch ret_id {
        | Some (id, typ) =>
          let id' = sub_id id;
          Ident.equal id id' ? ret_id : Some (id', typ)
        | None => None
        }
      } else {
        ret_id
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
    if (ret_id' === ret_id && fun_exp' === fun_exp && actuals' === actuals) {
      instr
    } else {
      Call ret_id' fun_exp' actuals' call_flags loc
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
  | Declare_locals _ => instr
  }
};


/** apply [subst] to all id's in [instr], including binder id's */
let instr_sub (subst: subst) instr => instr_sub_ids sub_id_binders::true (apply_sub subst) instr;

let id_typ_compare (id1, typ1) (id2, typ2) => {
  let n = Ident.compare id1 id2;
  if (n != 0) {
    n
  } else {
    Typ.compare typ1 typ2
  }
};

let exp_typ_compare (exp1, typ1) (exp2, typ2) => {
  let n = Exp.compare exp1 exp2;
  if (n != 0) {
    n
  } else {
    Typ.compare typ1 typ2
  }
};

let instr_compare instr1 instr2 =>
  switch (instr1, instr2) {
  | (Load id1 e1 t1 loc1, Load id2 e2 t2 loc2) =>
    let n = Ident.compare id1 id2;
    if (n != 0) {
      n
    } else {
      let n = Exp.compare e1 e2;
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
  | (Load _, _) => (-1)
  | (_, Load _) => 1
  | (Store e11 t1 e21 loc1, Store e12 t2 e22 loc2) =>
    let n = Exp.compare e11 e12;
    if (n != 0) {
      n
    } else {
      let n = Typ.compare t1 t2;
      if (n != 0) {
        n
      } else {
        let n = Exp.compare e21 e22;
        if (n != 0) {
          n
        } else {
          Location.compare loc1 loc2
        }
      }
    }
  | (Store _, _) => (-1)
  | (_, Store _) => 1
  | (Prune cond1 loc1 true_branch1 ik1, Prune cond2 loc2 true_branch2 ik2) =>
    let n = Exp.compare cond1 cond2;
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
  | (Call ret_id1 e1 arg_ts1 loc1 cf1, Call ret_id2 e2 arg_ts2 loc2 cf2) =>
    let n = opt_compare id_typ_compare ret_id1 ret_id2;
    if (n != 0) {
      n
    } else {
      let n = Exp.compare e1 e2;
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
            CallFlags.compare cf1 cf2
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
      let e1_mapping = Exp.Map.find e1 exp_map;
      (Exp.compare e1_mapping e2, exp_map)
    } {
    | Not_found =>
      /* assume e1 and e2 equal, enforce by adding to [exp_map] */
      (0, Exp.Map.add e1 e2 exp_map)
    };
  switch (e1: Exp.t, e2: Exp.t) {
  | (Var _, Var _) => compare_exps_with_map e1 e2 exp_map
  | (UnOp o1 e1 to1, UnOp o2 e2 to2) =>
    let n = Unop.compare o1 o2;
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
    let n = Binop.compare o1 o2;
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
  | _ => (Exp.compare e1 e2, exp_map)
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
  let id_typ_opt_compare_structural id_typ1 id_typ2 exp_map => {
    let id_typ_compare_structural (id1, typ1) (id2, typ2) => {
      let (n, exp_map) = exp_compare_structural (Var id1) (Var id2) exp_map;
      if (n != 0) {
        (n, exp_map)
      } else {
        (Typ.compare typ1 typ2, exp_map)
      }
    };
    switch (id_typ1, id_typ2) {
    | (Some it1, Some it2) => id_typ_compare_structural it1 it2
    | (None, None) => (0, exp_map)
    | (None, _) => ((-1), exp_map)
    | (_, None) => (1, exp_map)
    }
  };
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
  | (Load id1 e1 t1 _, Load id2 e2 t2 _) =>
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
  | (Store e11 t1 e21 _, Store e12 t2 e22 _) =>
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
  | (Call ret_id1 e1 arg_ts1 _ cf1, Call ret_id2 e2 arg_ts2 _ cf2) =>
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
    let (n, exp_map) = id_typ_opt_compare_structural ret_id1 ret_id2 exp_map;
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
            CallFlags.compare cf1 cf2
          },
          exp_map
        )
      }
    }
  | (Nullify pvar1 _, Nullify pvar2 _) => exp_compare_structural (Lvar pvar1) (Lvar pvar2) exp_map
  | (Abstract _, Abstract _) => (0, exp_map)
  | (Remove_temps temps1 _, Remove_temps temps2 _) =>
    id_list_compare_structural temps1 temps2 exp_map
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
    let (_, e') = IList.find (fun (e1, _) => Exp.equal e e1) epairs;
    e'
  } {
  | Not_found => e
  };

let atom_replace_exp epairs atom => atom_expmap (fun e => exp_replace_exp epairs e) atom;

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
let module HpredHash = Hashtbl.Make {
  type t = hpred;
  let equal = hpred_equal;
  let hash = Hashtbl.hash;
};

type sharing_env = {exph: Exp.Hash.t Exp.t, hpredh: HpredHash.t hpred};


/** Create a sharing env to store canonical representations */
let create_sharing_env () => {exph: Exp.Hash.create 3, hpredh: HpredHash.create 3};


/** Return a canonical representation of the exp */
let exp_compact sh e =>
  try (Exp.Hash.find sh.exph e) {
  | Not_found =>
    Exp.Hash.add sh.exph e e;
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

/** Compute the offset list of an expression */
let exp_get_offsets exp => {
  let rec f offlist_past e =>
    switch (e: Exp.t) {
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
    | [Off_fld fld typ, ...offs'] => f (Exp.Lfield acc fld typ) offs'
    | [Off_index e, ...offs'] => f (Exp.Lindex acc e) offs';
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
    let g id id' => (id, Exp.Var id');
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
    let g id id' => (id, Exp.Var id');
    try (IList.map2 g para.evars_dll ids_evars) {
    | Invalid_argument _ => assert false
    }
  };
  let subst = sub_of_list (
    [(para.cell, cell), (para.blink, blink), (para.flink, flink), ...subst_for_svars] @ subst_for_evars
  );
  (ids_evars, IList.map (hpred_sub subst) para.body_dll)
};

let custom_error = Pvar.mk_global (Mangled.from_string "INFER_CUSTOM_ERROR") DB.source_file_empty;
