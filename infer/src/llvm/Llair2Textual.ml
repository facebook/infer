(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Llair
module Type = Llair2TextualType
module L = Logging
module ProcState = Llair2TextualProcState
module VarMap = Textual.VarName.Map
module IdentMap = Textual.Ident.Map
module RegMap = Llair.Exp.Reg.Map

let builtin_qual_proc_name name : Textual.QualifiedProcName.t =
  { enclosing_class= Enclosing (Textual.TypeName.of_string "$builtins")
  ; name= Textual.ProcName.of_string name }


let string_name_of_reg reg =
  let name = Reg.name reg in
  if Option.is_some (Int.of_string_opt name) then Format.sprintf "var%s" name else name


let reg_to_var_name reg = Textual.VarName.of_string (string_name_of_reg reg)

let reg_to_id ~proc_state reg =
  let id = ProcState.mk_fresh_id ~reg proc_state in
  let reg_typ =
    Type.to_textual_typ proc_state.ProcState.lang ~struct_map:proc_state.ProcState.struct_map
      (Reg.typ reg)
  in
  ProcState.update_ids ~proc_state id (Textual.Typ.mk_without_attributes reg_typ) ;
  (id, reg_typ)


let add_fresh_id ~proc_state ?typ () =
  let id = ProcState.mk_fresh_id proc_state in
  ( match typ with
  | None ->
      ()
  | Some typ ->
      ProcState.update_ids ~proc_state id (Textual.Typ.mk_without_attributes typ) ) ;
  id


let reg_to_textual_var ~(proc_state : ProcState.t) reg =
  let reg_var_name = reg_to_var_name reg in
  if VarMap.mem reg_var_name proc_state.formals || VarMap.mem reg_var_name proc_state.locals then
    Textual.Exp.Lvar reg_var_name
  else Textual.Exp.Var (reg_to_id ~proc_state reg |> fst)


let reg_to_annot_typ lang ~struct_map reg =
  Type.to_annotated_textual_typ lang ~struct_map (Reg.typ reg)


let to_textual_loc {Loc.line; col} = Textual.Location.Known {line; col}

let is_loc_default loc =
  match loc with
  | Textual.Location.Known {line; col} ->
      Int.equal line 0 && Int.equal col 0
  | _ ->
      false


let to_textual_loc_instr ~(proc_state : ProcState.t) loc =
  let loc = to_textual_loc loc in
  if is_loc_default loc then proc_state.loc else loc


let translate_llair_globals globals =
  let add_global map global =
    let name = global.GlobalDefn.name |> Global.name |> Textual.VarName.of_string in
    Textual.VarName.Map.add name global map
  in
  let globals = StdUtils.iarray_to_list globals in
  List.fold ~f:add_global globals ~init:Textual.VarName.Map.empty


let to_qualified_proc_name ?loc func_name =
  let func_name = FuncName.name func_name in
  Textual.QualifiedProcName.
    {enclosing_class= TopLevel; name= Textual.ProcName.of_string ?loc func_name}


let to_name_attr func_name =
  Option.map ~f:Textual.Attr.mk_plain_name (FuncName.unmangled_name func_name)


let to_formals lang ~struct_map func =
  let to_textual_formal formal = reg_to_var_name formal in
  let to_textual_formal_type formal = reg_to_annot_typ lang ~struct_map formal in
  let llair_formals = StdUtils.iarray_to_list func.Llair.formals in
  let formals = List.map ~f:to_textual_formal llair_formals in
  let formals_types = List.map ~f:to_textual_formal_type llair_formals in
  (formals, formals_types)


let block_to_node_name block =
  let name = block.Llair.lbl in
  Textual.NodeName.of_string name


let to_textual_arith_exp_builtin (op : Llair.Exp.op2) (typ : Llair.Typ.t) =
  let sil_binop : Binop.t =
    match (op, typ) with
    | Add, Integer _ ->
        PlusA (Some IInt)
    | Add, Pointer _ ->
        PlusPI
    | Sub, Integer _ ->
        MinusA (Some IInt)
    | Mul, Integer _ ->
        Mult (Some IInt)
    | Div, Integer _ ->
        DivI
    | Div, Float _ ->
        DivF
    | Rem, Integer _ ->
        Mod
    | _ ->
        L.die InternalError "unsupported llair binop %a:%a@\n" Sexp.pp (Llair.Exp.sexp_of_op2 op)
          Llair.Typ.pp typ
  in
  Textual.ProcDecl.of_binop sil_binop


let to_textual_bool_exp_builtin (op : Llair.Exp.op2) =
  let sil_bin_op =
    match op with
    | Eq ->
        Binop.Eq
    | Dq ->
        Binop.Ne
    | Gt ->
        Binop.Gt
    | Ge ->
        Binop.Ge
    | Le ->
        Binop.Le
    | Lt ->
        Binop.Lt
    | And ->
        Binop.LAnd
    | Or ->
        Binop.LOr
    | Xor ->
        Binop.BXor
    | Shl ->
        Binop.Shiftlt
    | Lshr ->
        Binop.Shiftrt
    | Ashr ->
        Binop.Shiftrt
    | _ ->
        assert false
  in
  Textual.ProcDecl.of_binop sil_bin_op


let undef_exp exp =
  L.internal_error "unsupported llair exp %a@\n" Llair.Exp.pp exp ;
  let proc = builtin_qual_proc_name "llvm_nondet" in
  (* TODO: should include the arguments here too *)
  (Textual.Exp.Call {proc; args= []; kind= NonVirtual}, None, [])


let add_deref ~proc_state exp loc =
  match exp with
  | Textual.Exp.Lvar _ | Textual.Exp.Field _ ->
      let id = add_fresh_id ~proc_state () in
      let instr = Textual.Instr.Load {id; exp; typ= None; loc} in
      ([instr], Textual.Exp.Var id)
  | _ ->
      ([], exp)


let rec to_textual_exp ~(proc_state : ProcState.t) loc ?generate_typ_exp (exp : Llair.Exp.t) :
    Textual.Exp.t * Textual.Typ.t option * Textual.Instr.t list =
  let struct_map = proc_state.ProcState.struct_map in
  match exp with
  | Integer {data; typ} ->
      let textual_typ = Type.to_textual_typ proc_state.lang ~struct_map typ in
      let textual_exp =
        if Option.is_some generate_typ_exp then Textual.Exp.Typ textual_typ
        else if NS.Z.is_false data && not (Llair.Typ.is_int typ) then Textual.Exp.Const Null
        else Textual.Exp.Const (Int data)
      in
      (textual_exp, Some textual_typ, [])
  | Float {data; typ} ->
      let textual_typ = Type.to_textual_typ proc_state.lang ~struct_map typ in
      let textual_exp =
        if Option.is_some generate_typ_exp then
          Textual.Exp.Typ (Type.to_textual_typ proc_state.lang ~struct_map typ)
        else Textual.Exp.Const (Float (Float.of_string data))
      in
      (textual_exp, Some textual_typ, [])
  | FuncName {name} ->
      (Textual.Exp.Const (Str name), None, [])
  | Reg {id; name; typ} ->
      let textual_typ = Type.to_textual_typ proc_state.lang ~struct_map typ in
      let textual_exp = reg_to_textual_var ~proc_state (Reg.mk typ id name) in
      (textual_exp, Some textual_typ, [])
  | Global {name; typ; is_constant} ->
      let textual_typ = Type.to_textual_typ proc_state.lang ~struct_map typ in
      let textual_exp =
        match textual_typ with
        | Textual.Typ.Ptr _ when is_constant -> (
            let string_opt =
              match
                VarMap.find_opt (Textual.VarName.of_string name) proc_state.ProcState.globals
              with
              | Some global ->
                  Option.bind ~f:Llair.Exp.string_of_exp global.Llair.GlobalDefn.init
              | None ->
                  None
            in
            match string_opt with
            | Some s ->
                Textual.Exp.Const (Str s)
            | None ->
                Textual.Exp.Lvar (Textual.VarName.of_string name) )
        | _ ->
            Textual.Exp.Lvar (Textual.VarName.of_string name)
      in
      (textual_exp, Some textual_typ, [])
  | Ap1 (Select n, typ, exp) -> (
      let typ_name =
        match typ with
        | Struct {name} ->
            Some (Textual.TypeName.of_string name)
        | Tuple _ -> (
          match Type.to_textual_typ proc_state.lang ~struct_map typ with
          | Textual.Typ.(Ptr (Struct name)) ->
              Some name
          | _ ->
              None )
        | _ ->
            None
      in
      match typ_name with
      | None ->
          undef_exp exp
      | Some typ_name ->
          let exp, _, exp_instrs = to_textual_exp loc ~proc_state exp in
          let field =
            if Llair.Typ.is_tuple typ then Type.tuple_field_of_pos typ_name n
            else Type.field_of_pos typ_name n
          in
          let field_instrs, exp = add_deref ~proc_state exp loc in
          let instrs = List.append field_instrs exp_instrs in
          let exp = Textual.Exp.Field {exp; field} in
          let typ = Type.lookup_field_type ~struct_map typ_name field in
          (exp, typ, instrs) )
  | Ap1 ((Convert _ | Signed _ | Unsigned _), dst_typ, exp) ->
      (* Signed is the translation of llvm's trunc and SExt and Unsigned is the translation of ZExt, all different types of cast,
         and convert translates other types of cast *)
      let exp = to_textual_exp loc ~proc_state exp |> fst3 in
      let textual_dst_typ = Type.to_textual_typ proc_state.lang ~struct_map dst_typ in
      let proc = Textual.ProcDecl.cast_name in
      ( Call {proc; args= [Textual.Exp.Typ textual_dst_typ; exp]; kind= Textual.Exp.NonVirtual}
      , None
      , [] )
  | Ap1 (Splat, _, _) ->
      (* [splat exp] initialises every element of an array with the element exp, so to be precise it
         needs to be translated as a loop. We translate here to a non-deterministic value for the array *)
      let proc = builtin_qual_proc_name "llvm_nondet" in
      (Call {proc; args= []; kind= Textual.Exp.NonVirtual}, None, [])
  | Ap2 (((Add | Sub | Mul | Div | Rem) as op), typ, e1, e2) ->
      let proc = to_textual_arith_exp_builtin op typ in
      let exp1, typ1, exp1_instrs = to_textual_exp loc ~proc_state e1 in
      let exp2, _, exp2_instrs = to_textual_exp loc ~proc_state e2 in
      ( Call {proc; args= [exp1; exp2]; kind= Textual.Exp.NonVirtual}
      , typ1
      , List.append exp1_instrs exp2_instrs )
  | Ap2 (((Eq | Dq | Gt | Ge | Le | Lt | And | Or | Xor | Shl | Lshr | Ashr) as op), _, e1, e2) ->
      let proc = to_textual_bool_exp_builtin op in
      let exp1, typ1, exp1_instrs = to_textual_exp loc ~proc_state e1 in
      let exp2, _, exp2_instrs = to_textual_exp loc ~proc_state e2 in
      ( Call {proc; args= [exp1; exp2]; kind= Textual.Exp.NonVirtual}
      , typ1
      , List.append exp1_instrs exp2_instrs )
  | Ap2 (Update idx, typ, rcd, elt) ->
      let rcd_exp, _, rcd_instrs = to_textual_exp loc ~proc_state rcd in
      let elt_exp, _, elt_instrs = to_textual_exp loc ~proc_state elt in
      let elt_deref_instrs, elt_exp_deref = add_deref ~proc_state elt_exp loc in
      let elt_instrs = List.append elt_instrs elt_deref_instrs in
      let textual_typ = Type.to_textual_typ proc_state.lang ~struct_map typ in
      let type_name =
        match textual_typ with Textual.Typ.(Ptr (Struct name)) -> name | _ -> assert false
      in
      let index_exp =
        Textual.Exp.Field {exp= rcd_exp; field= Type.tuple_field_of_pos type_name idx}
      in
      let store_instr =
        Textual.Instr.Store {exp1= index_exp; exp2= elt_exp_deref; typ= None; loc}
      in
      (rcd_exp, Some textual_typ, List.append (List.append rcd_instrs elt_instrs) [store_instr])
  | _ ->
      undef_exp exp


let to_textual_bool_exp ~proc_state loc exp =
  let textual_exp, textual_typ_opt, _ = to_textual_exp loc ~proc_state exp in
  (Textual.BoolExp.Exp textual_exp, textual_typ_opt)


let to_textual_call_aux ~proc_state ~kind ?exp_opt proc return ?generate_typ_exp args loc =
  let loc = to_textual_loc_instr ~proc_state loc in
  let args_instrs, args =
    List.fold_map
      ~f:(fun acc_instrs exp ->
        let exp, _, instrs = to_textual_exp loc ~proc_state ?generate_typ_exp exp in
        let deref_instrs, deref_exp =
          (* So far it looks like for C the load operations are already there when needed. *)
          if Textual.Lang.is_swift proc_state.ProcState.lang then add_deref ~proc_state exp loc
          else ([], exp)
        in
        (List.append deref_instrs (List.append acc_instrs instrs), deref_exp) )
      args ~init:[]
  in
  let args = List.append (Option.to_list exp_opt) args in
  let args, args_instrs =
    if Textual.QualifiedProcName.equal proc Textual.ProcDecl.assert_fail_name then
      ([Textual.Exp.Const Null], [])
    else (args, args_instrs)
  in
  let id =
    Option.map return ~f:(fun reg ->
        let id = reg_to_id ~proc_state reg |> fst in
        id )
  in
  let call_exp =
    if
      (* skip calls to swift_retain *)
      Textual.ProcName.equal proc.Textual.QualifiedProcName.name
        (Textual.ProcName.of_string "swift_retain")
    then List.hd_exn args
    else Textual.Exp.Call {proc; args; kind}
  in
  let let_instr = Textual.Instr.Let {id; exp= call_exp; loc} in
  List.append [let_instr] args_instrs


let to_textual_call ~proc_state (call : 'a Llair.call) =
  let loc = to_textual_loc call.loc in
  let proc, kind, exp_opt =
    match call.callee with
    | Direct {func} ->
        let proc =
          if
            String.equal (FuncName.name func.Llair.name)
              (Procname.get_method BuiltinDecl.__assert_fail)
            || String.is_substring ~substring:"assertionFailure" (FuncName.name func.Llair.name)
          then Textual.ProcDecl.assert_fail_name
          else to_qualified_proc_name func.Llair.name
        in
        (proc, Textual.Exp.NonVirtual, None)
    | Indirect {ptr} ->
        let proc = builtin_qual_proc_name "llvm_dynamic_call" in
        (proc, Textual.Exp.NonVirtual, Some (to_textual_exp loc ~proc_state ptr |> fst3))
    | Intrinsic intrinsic ->
        let proc = builtin_qual_proc_name (Llair.Intrinsic.to_name intrinsic) in
        (proc, Textual.Exp.NonVirtual, None)
  in
  let args = StdUtils.iarray_to_list call.actuals in
  to_textual_call_aux ~proc_state ~kind ?exp_opt proc call.areturn args call.loc


let to_textual_builtin ~proc_state return name args loc =
  let proc = builtin_qual_proc_name name in
  to_textual_call_aux ~proc_state ~kind:Textual.Exp.NonVirtual proc return args loc


let remove_store_zero_in_class exp1 typ_exp1 exp2 loc =
  let any_type_llvm_name =
    match Textual.Typ.any_type_llvm with Textual.Typ.Struct name -> name | _ -> assert false
  in
  match typ_exp1 with
  | Some (Textual.Typ.Ptr (Struct type_name))
    when Textual.Exp.is_zero_exp exp2 && not (Textual.TypeName.equal type_name any_type_llvm_name)
    ->
      None
  | _ ->
      Some (Textual.Instr.Store {exp1; typ= None; exp2; loc})


let cmnd_to_instrs ~(proc_state : ProcState.t) block =
  let struct_map = proc_state.ProcState.struct_map in
  let to_instr textual_instrs inst =
    match inst with
    | Load {reg; ptr; loc} ->
        let loc = to_textual_loc_instr ~proc_state loc in
        let id, _ = reg_to_id ~proc_state reg in
        let exp, _, ptr_instrs = to_textual_exp loc ~proc_state ptr in
        let textual_instr = Textual.Instr.Load {id; exp; typ= None; loc} in
        List.append ptr_instrs (textual_instr :: textual_instrs)
    | Store {ptr; exp; loc} ->
        let loc = to_textual_loc_instr ~proc_state loc in
        let exp2, _, exp2_instrs_ = to_textual_exp loc ~proc_state exp in
        let exp2, exp2_instrs =
          match (exp, exp2) with
          | Llair.Exp.Reg {name; id; typ}, Textual.Exp.Lvar _ ->
              let reg = Reg.mk typ id name in
              let id, reg_typ = reg_to_id ~proc_state reg in
              let new_exp2 = Textual.Exp.Var id in
              ProcState.update_ids ~proc_state id (Textual.Typ.mk_without_attributes reg_typ) ;
              let exp2_instr = Textual.Instr.Load {id; exp= exp2; typ= None; loc} in
              (new_exp2, exp2_instrs_ @ [exp2_instr])
          | _ ->
              (exp2, exp2_instrs_)
        in
        let exp1, typ_exp1, exp1_instrs = to_textual_exp loc ~proc_state ptr in
        let textual_instr_opt = remove_store_zero_in_class exp1 typ_exp1 exp2 loc in
        (Option.to_list textual_instr_opt @ exp2_instrs) @ exp1_instrs @ textual_instrs
    | Alloc {reg} ->
        let reg_var_name = reg_to_var_name reg in
        let ptr_typ = Type.to_annotated_textual_typ proc_state.lang ~struct_map (Reg.typ reg) in
        ProcState.update_locals ~proc_state reg_var_name ptr_typ ;
        textual_instrs
    | Free {ptr; loc} ->
        let proc = Textual.ProcDecl.free_name in
        let call_textual_instrs =
          to_textual_call_aux ~proc_state ~kind:Textual.Exp.NonVirtual proc None [ptr] loc
        in
        List.append call_textual_instrs textual_instrs
    | Nondet {reg; loc} ->
        (* llvm_init_tuple is also a nondet builtin but we return the type for tuples in the Textual to Sil translation *)
        let builtin_name_opt =
          match reg with
          | Some reg when Llair.Typ.is_tuple (Reg.typ reg) ->
              Some "llvm_init_tuple"
          | Some _ ->
              Some "llvm_nondet"
          | None ->
              None
        in
        let call_textual_instrs =
          match builtin_name_opt with
          | Some builtin_name ->
              to_textual_builtin ~proc_state reg builtin_name [] loc
          | None ->
              []
        in
        List.append call_textual_instrs textual_instrs
    | Builtin {reg; name; args; loc} when Llair.Builtin.equal name `malloc -> (
        let proc = Textual.ProcDecl.malloc_name in
        match StdUtils.iarray_to_list args with
        | [((Llair.Exp.Integer _ | Llair.Exp.Float _) as exp)] ->
            let call_textual_instrs =
              to_textual_call_aux ~proc_state ~generate_typ_exp:(Some true)
                ~kind:Textual.Exp.NonVirtual proc reg [exp] loc
            in
            List.append call_textual_instrs textual_instrs
        | _ ->
            assert false )
    | Builtin {reg; name; args; loc= loc_}
      when Textual.Lang.is_swift proc_state.ProcState.lang && Llair.Builtin.equal name `memset -> (
        let args = StdUtils.iarray_to_list args in
        let loc = to_textual_loc loc_ in
        match args with
        | [_; arg2; _; _] when Textual.Exp.is_zero_exp (fst3 (to_textual_exp loc ~proc_state arg2))
          ->
            textual_instrs
        | _ ->
            let name = Llair.Builtin.to_name name in
            let call_textual_instrs = to_textual_builtin ~proc_state reg name args loc_ in
            List.append call_textual_instrs textual_instrs )
    | Builtin {reg; name; args; loc= loc_} when Llair.Builtin.equal name `expect -> (
        let args = StdUtils.iarray_to_list args in
        let loc = to_textual_loc loc_ in
        match args with
        | [arg1; _] ->
            let exp, _, exp_instrs = to_textual_exp loc ~proc_state arg1 in
            let id =
              match reg with
              | Some reg ->
                  Some (reg_to_id ~proc_state reg |> fst)
              | None ->
                  Some (add_fresh_id ~proc_state ())
            in
            Textual.Instr.Let {id; exp; loc} :: exp_instrs
        | _ ->
            assert false )
    | Builtin {reg; name; args; loc} ->
        let name = Llair.Builtin.to_name name in
        let args = StdUtils.iarray_to_list args in
        let call_textual_instrs = to_textual_builtin ~proc_state reg name args loc in
        List.append call_textual_instrs textual_instrs
    | Move {reg_exps: (Reg.t * Exp.t) NS.iarray; loc} ->
        let reg_exps = StdUtils.iarray_to_list reg_exps in
        let loc = to_textual_loc_instr ~proc_state loc in
        let instrs =
          List.fold
            ~f:(fun instrs (reg, exp) ->
              let id = Some (reg_to_id ~proc_state reg |> fst) in
              let exp, _, exp_instrs = to_textual_exp loc ~proc_state exp in
              let instrs = List.append instrs exp_instrs in
              match exp with
              | Textual.Exp.Field _ ->
                  let id = Option.value id ~default:(add_fresh_id ~proc_state ()) in
                  Textual.Instr.Load {id; exp; typ= None; loc} :: instrs
              | _ ->
                  Textual.Instr.Let {id; exp; loc} :: instrs )
            ~init:[] reg_exps
        in
        List.append instrs textual_instrs
    | AtomicRMW {reg; ptr; exp; loc} ->
        let call_textual_instrs =
          to_textual_builtin ~proc_state (Some reg) "llvm_atomicRMW" [ptr; exp] loc
        in
        List.append call_textual_instrs textual_instrs
    | AtomicCmpXchg {reg; ptr; cmp; exp; loc} ->
        let call_textual_instrs =
          to_textual_builtin ~proc_state (Some reg) "llvm_atomicCmpXchg" [ptr; cmp; exp] loc
        in
        List.append call_textual_instrs textual_instrs
  in
  let rev_instrs = List.fold ~init:[] ~f:to_instr (StdUtils.iarray_to_list block.cmnd) in
  let call_instrs =
    match block.term with Call call -> to_textual_call ~proc_state call | _ -> []
  in
  let rev_instrs = List.append call_instrs rev_instrs in
  let instrs = List.rev rev_instrs in
  let first_loc, last_loc =
    match (instrs, rev_instrs) with
    | first :: _, last :: _ ->
        (Some (Textual.Instr.loc first), Some (Textual.Instr.loc last))
    | _ ->
        (None, None)
  in
  (instrs, first_loc, last_loc)


let rec to_textual_jump_and_succs ~proc_state ~seen_nodes jump =
  let block = jump.dst in
  let node_label = block_to_node_name block in
  let node_label, typ_opt, succs =
    (* If we've seen this node, stop the recursion *)
    if Textual.NodeName.Set.mem node_label seen_nodes then (node_label, None, Textual.Node.Set.empty)
    else
      let node, typ_opt, nodes = block_to_node_and_succs ~proc_state ~seen_nodes jump.dst in
      (node.label, typ_opt, nodes)
  in
  let node_call = Textual.Terminator.{label= node_label; ssa_args= []} in
  (Textual.Terminator.Jump [node_call], typ_opt, succs)


and to_terminator_and_succs ~proc_state ~seen_nodes term :
    (Textual.Terminator.t * Textual.Typ.t option * Textual.Node.Set.t)
    * Textual.Location.t option
    * Textual.Instr.t list =
  let no_succs = Textual.Node.Set.empty in
  match term with
  | Call {return; loc} ->
      let loc = to_textual_loc_instr ~proc_state loc in
      (to_textual_jump_and_succs ~proc_state ~seen_nodes return, Some loc, [])
  | Return {exp= Some exp; loc= loc_} ->
      let loc = to_textual_loc_instr ~proc_state loc_ in
      let textual_exp, textual_typ_opt, instrs = to_textual_exp loc ~proc_state exp in
      let exp_deref_instrs, textual_exp = add_deref ~proc_state textual_exp loc in
      let instrs = List.append instrs exp_deref_instrs in
      ((Textual.Terminator.Ret textual_exp, textual_typ_opt, no_succs), Some loc, instrs)
  | Return {exp= None; loc} ->
      let loc = to_textual_loc_instr ~proc_state loc in
      ((Textual.Terminator.Ret (Textual.Exp.Const Null), None, no_succs), Some loc, [])
  | Throw {exc; loc} ->
      let loc = to_textual_loc_instr ~proc_state loc in
      ( (Textual.Terminator.Throw (to_textual_exp loc ~proc_state exc |> fst3), None, no_succs)
      , Some loc
      , [] )
  | Switch {key; tbl; els; loc} -> (
      let loc = to_textual_loc_instr ~proc_state loc in
      match StdUtils.iarray_to_list tbl with
      | [(exp, zero_jump)] when Exp.equal exp Exp.false_ ->
          (* if then else *)
          let bexp = to_textual_bool_exp loc ~proc_state key |> fst in
          let else_, else_typ, zero_nodes =
            to_textual_jump_and_succs ~proc_state ~seen_nodes zero_jump
          in
          let then_, if_typ, els_nodes = to_textual_jump_and_succs ~proc_state ~seen_nodes els in
          let term = Textual.Terminator.If {bexp; then_; else_} in
          let nodes = Textual.Node.Set.union zero_nodes els_nodes in
          let typ_opt = Type.join_typ if_typ else_typ in
          ((term, typ_opt, nodes), Some loc, [])
      | [] when Exp.equal key Exp.false_ ->
          (* goto *)
          (to_textual_jump_and_succs ~proc_state ~seen_nodes els, Some loc, [])
      | _ ->
          ((Textual.Terminator.Unreachable, None, no_succs), None, [] (* TODO translate Switch *)) )
  | Iswitch {loc} | Abort {loc} | Unreachable {loc} ->
      let loc = to_textual_loc_instr ~proc_state loc in
      ((Textual.Terminator.Unreachable, None, no_succs), Some loc, [])


(* TODO still various parts of the node left to be translated *)
and block_to_node_and_succs ~proc_state ~seen_nodes (block : Llair.block) :
    Textual.Node.t * Textual.Typ.t option * Textual.Node.Set.t =
  let node_name = block_to_node_name block in
  let instrs, first_loc, last_loc = cmnd_to_instrs ~proc_state block in
  let (terminator, typ_opt, succs), term_loc_opt, term_instrs =
    to_terminator_and_succs ~proc_state
      ~seen_nodes:(Textual.NodeName.Set.add node_name seen_nodes)
      block.term
  in
  let instrs = List.append instrs term_instrs in
  let last_loc =
    match term_loc_opt with
    | Some loc ->
        loc
    | None ->
        Option.value last_loc ~default:Textual.Location.Unknown
  in
  let first_loc =
    match first_loc with
    | Some loc ->
        loc
    | None when List.is_empty instrs ->
        last_loc
    | _ ->
        Textual.Location.Unknown
  in
  let node =
    Textual.Node.
      { label= node_name
      ; ssa_parameters= []
      ; exn_succs= []
      ; last= terminator
      ; instrs
      ; last_loc
      ; label_loc= first_loc }
  in
  (* We add the nodes here to make sure they always get added even in the case of recursive jumps *)
  (node, typ_opt, Textual.Node.Set.add node succs)


let func_to_nodes ~proc_state func =
  let _, typ_opt, nodes =
    block_to_node_and_succs ~proc_state ~seen_nodes:Textual.NodeName.Set.empty func.Llair.entry
  in
  (typ_opt, Textual.Node.Set.to_list nodes)


let is_undefined func =
  let entry = func.Llair.entry in
  match entry.term with
  | Unreachable _ ->
      String.equal entry.lbl "undefined" && List.is_empty (StdUtils.iarray_to_list entry.cmnd)
  | _ ->
      false


type textual_proc = ProcDecl of Textual.ProcDecl.t | ProcDesc of Textual.ProcDesc.t

let should_translate plain_name lang source_file (loc : Llair.Loc.t) =
  let file =
    if Textual.Lang.is_c lang then loc.Llair.Loc.dir ^ "/" ^ loc.Llair.Loc.file
    else loc.Llair.Loc.file
  in
  let source_file_loc = SourceFile.create file in
  SourceFile.equal source_file source_file_loc
  || Option.exists plain_name ~f:(fun plain_name ->
         (* the loc in these methods is empty but these are getters and setters and we need to translate them *)
         String.is_substring ~substring:".get" plain_name
         || String.is_substring ~substring:".set" plain_name )


let translate_llair_functions source_file lang struct_map globals functions =
  let function_to_formal proc_descs (func_name, func) =
    let formals_list, formals_types = to_formals lang ~struct_map func in
    let loc = to_textual_loc func.Llair.loc in
    let should_translate =
      should_translate (FuncName.unmangled_name func_name) lang source_file func.Llair.loc
    in
    let qualified_name = to_qualified_proc_name ~loc func_name in
    let plain_name = match lang with Textual.Lang.Swift -> to_name_attr func_name | _ -> None in
    let proc_loc = to_textual_loc func.Llair.loc in
    let formals_ =
      List.fold2_exn
        ~f:(fun formals varname typ -> Textual.VarName.Map.add varname typ formals)
        formals_list formals_types ~init:Textual.VarName.Map.empty
    in
    let proc_state : ProcState.t =
      { qualified_name
      ; loc= proc_loc
      ; formals= formals_
      ; locals= VarMap.empty
      ; ids= IdentMap.empty
      ; reg_map= RegMap.empty
      ; last_id= Textual.Ident.of_int 0
      ; struct_map
      ; globals
      ; lang }
    in
    let ret_typ, nodes = if should_translate then func_to_nodes ~proc_state func else (None, []) in
    let result_type =
      Textual.Typ.mk_without_attributes
        (Type.to_textual_typ lang ~struct_map (FuncName.typ func_name))
    in
    let result_type =
      match ret_typ with Some typ -> Textual.Typ.mk_without_attributes typ | None -> result_type
    in
    let procdecl =
      Textual.ProcDecl.
        { qualified_name
        ; result_type
        ; attributes= Option.to_list plain_name
        ; formals_types= Some formals_types }
    in
    let is_deinit () =
      Option.exists (FuncName.unmangled_name func_name) ~f:(String.equal "deinit")
    in
    if is_undefined func || (not should_translate) || (Textual.Lang.is_swift lang && is_deinit ())
    then ProcDecl procdecl :: proc_descs
    else
      let locals =
        VarMap.fold (fun varname typ locals -> (varname, typ) :: locals) proc_state.locals []
      in
      ProcDesc
        Textual.ProcDesc.
          { params= formals_list
          ; locals
          ; procdecl
          ; start= block_to_node_name func.Llair.entry
          ; nodes
          ; exit_loc= Unknown (* TODO: get this location *) }
      :: proc_descs
  in
  let values = FuncName.Map.to_list functions in
  List.fold values ~f:function_to_formal ~init:[]


let to_textual_global lang ~struct_map global =
  let global_ = global.GlobalDefn.name in
  let global_name = Global.name global_ in
  let name = Textual.VarName.of_string global_name in
  let typ = Type.to_textual_typ lang ~struct_map (Global.typ global_) in
  let loc = to_textual_loc global.GlobalDefn.loc in
  let global_proc_state = ProcState.global_proc_state lang loc in
  let init_exp =
    Option.map
      ~f:(fun exp -> to_textual_exp ~proc_state:global_proc_state loc exp |> fst3)
      global.GlobalDefn.init
  in
  Textual.Global.{name; typ; attributes= []; init_exp}


let translate ~source_file (llair_program : Llair.Program.t) lang : Textual.Module.t =
  let struct_map = Llair2TextualType.translate_types_env lang llair_program.typ_defns in
  let globals_map = translate_llair_globals llair_program.Llair.globals in
  let source_file_ = SourceFile.create source_file in
  let procs =
    translate_llair_functions source_file_ lang struct_map globals_map llair_program.Llair.functions
  in
  let procs =
    List.fold
      ~f:(fun procs proc ->
        match proc with
        | ProcDecl proc_decl ->
            Textual.Module.Procdecl proc_decl :: procs
        | ProcDesc proc_desc ->
            Textual.Module.Proc proc_desc :: procs )
      procs ~init:[]
  in
  let globals =
    Textual.VarName.Map.fold
      (fun _ global globals ->
        Textual.Module.Global (to_textual_global lang ~struct_map global) :: globals )
      globals_map []
  in
  let structs =
    List.map
      ~f:(fun (_, struct_) -> Textual.Module.Struct struct_)
      (Textual.TypeName.Map.bindings struct_map)
  in
  let decls = List.append procs globals in
  let decls = List.append decls structs in
  let attrs = [Textual.Attr.mk_source_language lang] in
  let textual_source_file = Textual.SourceFile.create source_file in
  Textual.Module.{attrs; decls; sourcefile= textual_source_file}
