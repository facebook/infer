(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

open Javalib_pack
open Sawja_pack

module NodeTbl = Cfg.NodeHash

type jump_kind =
  | Next
  | Jump of int
  | Exit

type meth_kind =
  | Normal
  | Init

(** data  *)
type icfg = {
  tenv : Tenv.t;
  cg : Cg.t;
  cfg : Cfg.cfg;
}

type t =
  { icfg : icfg;
    procdesc : Cfg.Procdesc.t;
    impl : JBir.t;
    mutable var_map : (Pvar.t * Sil.typ * Sil.typ) JBir.VarMap.t;
    if_jumps : int NodeTbl.t;
    goto_jumps : (int, jump_kind) Hashtbl.t;
    cn : JBasics.class_name;
    meth_kind : meth_kind;
    node : JCode.jcode Javalib.interface_or_class;
    program : JClasspath.program;
    never_null_matcher: Inferconfig.NeverReturnNull.matcher;
  }

let create_context never_null_matcher icfg procdesc impl cn meth_kind node program =
  { icfg = icfg;
    procdesc = procdesc;
    impl = impl;
    var_map = JBir.VarMap.empty;
    if_jumps = NodeTbl.create 10;
    goto_jumps = Hashtbl.create 10;
    cn = cn;
    meth_kind = meth_kind;
    node = node;
    program = program;
    never_null_matcher = never_null_matcher;
  }

let get_icfg context = context.icfg
let get_cfg context = context.icfg.cfg
let get_cg context = context.icfg.cg
let get_tenv context = context.icfg.tenv
let get_procdesc context = context.procdesc
let get_cn context = context.cn
let get_node context = context.node
let get_program context = context.program
let get_impl context = context.impl
let get_var_map context = context.var_map
let set_var_map context var_map = context.var_map <- var_map
let get_meth_kind context = context.meth_kind
let get_never_null_matcher context = context.never_null_matcher

let get_or_set_pvar_type context var typ =
  let var_map = get_var_map context in
  try
    let (pvar, otyp, _) = (JBir.VarMap.find var var_map) in
    let tenv = get_tenv context in
    if Prover.Subtyping_check.check_subtype tenv typ otyp ||
       Prover.Subtyping_check.check_subtype tenv otyp typ then
      set_var_map context (JBir.VarMap.add var (pvar, otyp, typ) var_map)
    else set_var_map context (JBir.VarMap.add var (pvar, typ, typ) var_map);
    (pvar, typ)
  with Not_found ->
    let procname = (Cfg.Procdesc.get_proc_name (get_procdesc context)) in
    let varname = Mangled.from_string (JBir.var_name_g var) in
    let pvar = Pvar.mk varname procname in
    set_var_map context (JBir.VarMap.add var (pvar, typ, typ) var_map);
    (pvar, typ)

let set_pvar context var typ = fst (get_or_set_pvar_type context var typ)

let reset_pvar_type context =
  let var_map = get_var_map context in
  let aux var item =
    match item with (pvar, otyp, _) ->
      set_var_map context (JBir.VarMap.add var (pvar, otyp, otyp) var_map) in
  JBir.VarMap.iter aux var_map

let get_var_type context var =
  try
    let (_, _, otyp) = JBir.VarMap.find var (get_var_map context) in
    Some otyp
  with Not_found -> None

let get_if_jumps context = context.if_jumps
let get_goto_jumps context = context.goto_jumps

let add_if_jump context node pc =
  NodeTbl.add (get_if_jumps context) node pc

let get_if_jump context node =
  try
    Some (NodeTbl.find (get_if_jumps context) node)
  with Not_found -> None

let add_goto_jump context pc jump =
  Hashtbl.add (get_goto_jumps context) pc jump

let get_goto_jump context pc =
  try
    Hashtbl.find (get_goto_jumps context) pc
  with Not_found -> Next

let is_goto_jump context pc =
  try
    match Hashtbl.find (get_goto_jumps context) pc with
    | Jump _ -> true
    | _ -> false
  with Not_found -> false

let exn_node_table = Procname.Hash.create 100

let reset_exn_node_table () =
  Procname.Hash.clear exn_node_table

let add_exn_node procname (exn_node : Cfg.Node.t) =
  Procname.Hash.add exn_node_table procname exn_node

let get_exn_node procdesc =
  try
    Some (Procname.Hash.find exn_node_table (Cfg.Procdesc.get_proc_name procdesc))
  with Not_found -> None
