(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Llair
open Llair2TextualType

let builtin_qual_proc_name name : Textual.QualifiedProcName.t =
  { enclosing_class= Enclosing (Textual.TypeName.of_string "$builtins")
  ; name= Textual.ProcName.of_string name }


let reg_to_var_name reg = Textual.VarName.of_string (Reg.name reg)

let reg_to_id reg = Textual.Ident.of_int (Reg.id reg)

let reg_to_annot_typ reg = to_annotated_textual_typ (Reg.typ reg)

let to_textual_loc {Loc.line; col} = Textual.Location.Known {line; col}

let translate_llair_globals globals =
  let to_textual_global global =
    let global = global.GlobalDefn.name in
    let global_name = Global.name global in
    let name = Textual.VarName.of_string global_name in
    let typ = to_textual_typ (Global.typ global) in
    Textual.Global.{name; typ; attributes= []}
  in
  let globals = StdUtils.iarray_to_list globals in
  List.map ~f:to_textual_global globals


let to_qualified_proc_name ?loc func_name =
  let func_name = FuncName.name func_name in
  let loc = Option.map ~f:to_textual_loc loc in
  Textual.QualifiedProcName.
    {enclosing_class= TopLevel; name= Textual.ProcName.of_string ?loc func_name}


let to_result_type func_name =
  let typ = FuncName.typ func_name in
  to_annotated_textual_typ typ


let to_formals func =
  let to_textual_formal formal = reg_to_var_name formal in
  let to_textual_formal_type formal = reg_to_annot_typ formal in
  let llair_formals = StdUtils.iarray_to_list func.Llair.formals in
  let formals = List.map ~f:to_textual_formal llair_formals in
  let formals_types = List.map ~f:to_textual_formal_type llair_formals in
  (formals, formals_types)


let to_locals func =
  let to_textual_local local =
    let local_name = reg_to_var_name local in
    let typ = reg_to_annot_typ local in
    (local_name, typ)
  in
  let locals = Reg.Set.to_list func.Llair.locals in
  List.map ~f:to_textual_local locals


let block_to_node_name block =
  let name = block.Llair.lbl in
  Textual.NodeName.of_string name


let to_textual_arith_exp_builtin (op : Llair.Exp.op2) typ =
  match (op, typ) with
  | Add, Llair.Typ.Integer _ ->
      "__sil_plusa_int"
  | Sub, Llair.Typ.Integer _ ->
      "__sil_minusa_int"
  | Mul, Llair.Typ.Integer _ ->
      "__sil_mult_int"
  | Div, Llair.Typ.Integer _ ->
      "__sil_divi"
  | Div, Llair.Typ.Float _ ->
      "__sil_divf"
  | Rem, Llair.Typ.Integer _ ->
      "__sil_mod"
  | _ ->
      assert false


let to_textual_bool_exp_builtin (op : Llair.Exp.op2) =
  match op with
  | Eq ->
      "__sil_eq"
  | Dq ->
      "__sil_ne"
  | Gt ->
      "__sil_gt"
  | Ge ->
      "__sil_ge"
  | Le ->
      "__sil_le"
  | _ ->
      assert false


(* TODO: translate expressions *)
let rec to_textual_exp ?generate_typ_exp (exp : Llair.Exp.t) : Textual.Exp.t =
  match exp with
  | Integer {data; typ} ->
      if Option.is_some generate_typ_exp then Textual.Exp.Typ (to_textual_typ typ)
      else if NS.Z.is_false data then Textual.Exp.Const Null
      else Textual.Exp.Const (Int data)
  | Float {data; typ} ->
      if Option.is_some generate_typ_exp then Textual.Exp.Typ (to_textual_typ typ)
      else Textual.Exp.Const (Float (Float.of_string data))
  | FuncName {name} ->
      Textual.Exp.Const (Str name)
  | Reg {id; name; typ} ->
      (* TODO: find a way to figure out when to use the id and when to use the name. In Llair Reg is used everywhere
         but in textual we sometimes need Var and sometimes LVar. *)
      Textual.Exp.Var (reg_to_id (Reg.mk typ id name))
  | Global {name} ->
      Textual.Exp.Lvar (Textual.VarName.of_string name)
  | Ap1 (Select n, Struct {name}, exp) ->
      let typ_name = Textual.TypeName.of_string name in
      Textual.Exp.Field
        { exp= to_textual_exp exp
        ; field=
            { enclosing_class= typ_name
            ; name= Textual.FieldName.of_string (Llair2TextualType.field_of_pos n) } }
  | Ap1 ((Convert _ | Signed _ | Unsigned _), dst_typ, exp) ->
      (* Signed is the translation of llvm's trunc and SExt and Unsigned is the translation of ZExt, all different types of cast,
         and convert translates other types of cast *)
      let exp = to_textual_exp exp in
      let typ = to_textual_typ dst_typ in
      let proc = Textual.ProcDecl.cast_name in
      Call {proc; args= [Textual.Exp.Typ typ; exp]; kind= Textual.Exp.NonVirtual}
  | Ap1 (Splat, _, _) ->
      (* [splat exp] initialises every element of an array with the element exp, so to be precise it
         needs to be translated as a loop. We translate here to a non-deterministic value for the array *)
      let proc = builtin_qual_proc_name "llvm_nondet" in
      Call {proc; args= []; kind= Textual.Exp.NonVirtual}
  | Ap2 (((Add | Sub | Mul | Div | Rem) as op), typ, e1, e2) ->
      let proc = builtin_qual_proc_name (to_textual_arith_exp_builtin op typ) in
      let exp1 = to_textual_exp e1 in
      let exp2 = to_textual_exp e2 in
      Call {proc; args= [exp1; exp2]; kind= Textual.Exp.NonVirtual}
  | Ap2 (((Eq | Dq | Gt | Ge | Le) as op), _, e1, e2) ->
      let proc = builtin_qual_proc_name (to_textual_bool_exp_builtin op) in
      let exp1 = to_textual_exp e1 in
      let exp2 = to_textual_exp e2 in
      Call {proc; args= [exp1; exp2]; kind= Textual.Exp.NonVirtual}
  | _ ->
      assert false


let to_textual_bool_exp exp = Textual.BoolExp.Exp (to_textual_exp exp)

let to_textual_call_aux ~kind ?exp_opt proc return ?generate_typ_exp args loc =
  let loc = to_textual_loc loc in
  let id = Option.map return ~f:(fun reg -> reg_to_id reg) in
  let args = List.map ~f:(to_textual_exp ?generate_typ_exp) args in
  let args = List.append (Option.to_list exp_opt) args in
  Textual.Instr.Let {id; exp= Call {proc; args; kind}; loc}


let to_textual_call (call : 'a Llair.call) =
  let proc, kind, exp_opt =
    match call.callee with
    | Direct {func} ->
        (to_qualified_proc_name func.Llair.name, Textual.Exp.NonVirtual, None)
    | Indirect {ptr} ->
        let proc = builtin_qual_proc_name "llvm_dynamic_call" in
        (proc, Textual.Exp.NonVirtual, Some (to_textual_exp ptr))
    | Intrinsic intrinsic ->
        let proc = builtin_qual_proc_name (Llair.Intrinsic.to_name intrinsic) in
        (proc, Textual.Exp.NonVirtual, None)
  in
  let args = StdUtils.iarray_to_list call.actuals in
  to_textual_call_aux ~kind ?exp_opt proc call.areturn args call.loc


let to_textual_builtin return name args loc =
  let proc = builtin_qual_proc_name name in
  to_textual_call_aux ~kind:Textual.Exp.NonVirtual proc return args loc


let cmnd_to_instrs block =
  let to_instr inst =
    match inst with
    | Load {reg; ptr; loc} ->
        let loc = to_textual_loc loc in
        let id = reg_to_id reg in
        let exp = to_textual_exp ptr in
        Textual.Instr.Load {id; exp; typ= None; loc}
    | Store {ptr; exp; loc} ->
        let loc = to_textual_loc loc in
        let exp1 = to_textual_exp ptr in
        let exp2 = to_textual_exp exp in
        Textual.Instr.Store {exp1; typ= None; exp2; loc}
    | Alloc {reg; loc} ->
        to_textual_builtin (Some reg) "llvm_alloc" [] loc
    | Free {ptr; loc} ->
        let proc = Textual.ProcDecl.free_name in
        to_textual_call_aux ~kind:Textual.Exp.NonVirtual proc None [ptr] loc
    | Nondet {reg; loc} ->
        to_textual_builtin reg "llvm_nondet" [] loc
    | Builtin {reg; name; args; loc} when Llair.Builtin.equal name `malloc -> (
        let proc = Textual.ProcDecl.malloc_name in
        match StdUtils.iarray_to_list args with
        | [((Llair.Exp.Integer _ | Llair.Exp.Float _) as exp)] ->
            to_textual_call_aux ~generate_typ_exp:(Some true) ~kind:Textual.Exp.NonVirtual proc reg
              [exp] loc
        | _ ->
            assert false )
    | Builtin {reg; name; args; loc} ->
        let name = Llair.Builtin.to_name name in
        let args = StdUtils.iarray_to_list args in
        to_textual_builtin reg name args loc
    | Move {reg_exps: (Reg.t * Exp.t) NS.iarray; loc} ->
        let reg_exps = StdUtils.iarray_to_list reg_exps in
        let exps = List.concat_map ~f:(fun (reg, exp) -> [Reg.to_exp reg; exp]) reg_exps in
        to_textual_builtin None "llvm_move" exps loc
    | AtomicRMW {reg; ptr; exp; loc} ->
        to_textual_builtin (Some reg) "llvm_atomicRMW" [ptr; exp] loc
    | AtomicCmpXchg {reg; ptr; cmp; exp; loc} ->
        to_textual_builtin (Some reg) "llvm_atomicCmpXchg" [ptr; cmp; exp] loc
  in
  let call_instr_opt =
    match block.term with Call call -> Some (to_textual_call call) | _ -> None
  in
  let instrs = List.map ~f:to_instr (StdUtils.iarray_to_list block.cmnd) in
  List.append instrs (Option.to_list call_instr_opt)


let rec to_textual_jump_and_succs ~seen_nodes jump =
  let block = jump.dst in
  let node_label = block_to_node_name block in
  let node_label, succs =
    (* If we've seen this node, stop the recursion *)
    if Textual.NodeName.Set.mem node_label seen_nodes then (node_label, Textual.Node.Set.empty)
    else
      let node, nodes = block_to_node_and_succs ~seen_nodes jump.dst in
      (node.label, nodes)
  in
  let node_call = Textual.Terminator.{label= node_label; ssa_args= []} in
  (Textual.Terminator.Jump [node_call], succs)


and to_terminator_and_succs ~seen_nodes term =
  let no_succs = Textual.Node.Set.empty in
  match term with
  | Call call ->
      to_textual_jump_and_succs ~seen_nodes call.return
  | Return {exp= Some exp} ->
      (Textual.Terminator.Ret (to_textual_exp exp), no_succs)
  | Return {exp= None} ->
      (Textual.Terminator.Ret (Textual.Exp.Typ Textual.Typ.Void), no_succs)
  | Throw {exc} ->
      (Textual.Terminator.Throw (to_textual_exp exc), no_succs)
  | Switch {key; tbl; els} -> (
    match StdUtils.iarray_to_list tbl with
    | [(exp, zero_jump)] when Exp.equal exp Exp.false_ ->
        (* if then else *)
        let bexp = to_textual_bool_exp key in
        let else_, zero_nodes = to_textual_jump_and_succs ~seen_nodes zero_jump in
        let then_, els_nodes = to_textual_jump_and_succs ~seen_nodes els in
        let term = Textual.Terminator.If {bexp; then_; else_} in
        let nodes = Textual.Node.Set.union zero_nodes els_nodes in
        (term, nodes)
    | [] when Exp.equal key Exp.false_ ->
        (* goto *)
        to_textual_jump_and_succs ~seen_nodes els
    | _ ->
        (Textual.Terminator.Unreachable, no_succs (* TODO translate Switch *)) )
  | Iswitch _ | Abort _ | Unreachable ->
      (Textual.Terminator.Unreachable, no_succs)


(* TODO still various parts of the node left to be translated *)
and block_to_node_and_succs ~seen_nodes (block : Llair.block) : Textual.Node.t * Textual.Node.Set.t
    =
  let node_name = block_to_node_name block in
  let terminator, succs =
    to_terminator_and_succs ~seen_nodes:(Textual.NodeName.Set.add node_name seen_nodes) block.term
  in
  let node =
    Textual.Node.
      { label= node_name
      ; ssa_parameters= []
      ; exn_succs= []
      ; last= terminator
      ; instrs= cmnd_to_instrs block
      ; last_loc= Textual.Location.Unknown
      ; label_loc= Textual.Location.Unknown }
  in
  (* We add the nodes here to make sure they always get added even in the case of recursive jumps *)
  (node, Textual.Node.Set.add node succs)


let func_to_nodes func =
  let _, nodes = block_to_node_and_succs ~seen_nodes:Textual.NodeName.Set.empty func.Llair.entry in
  Textual.Node.Set.to_list nodes


let translate_llair_functions functions =
  let function_to_formal proc_descs (func_name, func) =
    let formals_, formals_types = to_formals func in
    let locals = to_locals func in
    let qualified_name = to_qualified_proc_name func_name ~loc:func.Llair.loc in
    let result_type = to_result_type func_name in
    let procdecl =
      Textual.ProcDecl.
        {qualified_name; result_type; attributes= []; formals_types= Some formals_types}
    in
    let nodes = func_to_nodes func in
    Textual.ProcDesc.
      { params= formals_
      ; locals
      ; procdecl
      ; start= block_to_node_name func.Llair.entry
      ; nodes
      ; exit_loc= Unknown (* TODO: get this location *) }
    :: proc_descs
  in
  let values = FuncName.Map.to_list functions in
  List.fold values ~f:function_to_formal ~init:[]


let translate sourcefile (llair_program : Llair.Program.t) : Textual.Module.t =
  let globals = translate_llair_globals llair_program.Llair.globals in
  (* We'll build the procdesc partially until we have all the pieces required in Textual
     and can add them to the list of declarations *)
  let proc_descs = translate_llair_functions llair_program.Llair.functions in
  let proc_decls =
    List.map ~f:(fun Textual.ProcDesc.{procdecl} -> Textual.Module.Procdecl procdecl) proc_descs
  in
  let proc_desc_declarations =
    List.map ~f:(fun proc_desc -> Textual.Module.Proc proc_desc) proc_descs
  in
  let globals = List.map ~f:(fun global -> Textual.Module.Global global) globals in
  let structs =
    List.map
      ~f:(fun (_, struct_) -> Textual.Module.Struct struct_)
      (Textual.TypeName.Map.bindings !Llair2TextualType.structMap)
  in
  let decls = List.append proc_decls globals in
  let decls = List.append decls proc_desc_declarations in
  let decls = List.append decls structs in
  Textual.Module.{attrs= []; decls; sourcefile}
