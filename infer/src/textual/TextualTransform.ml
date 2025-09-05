(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
open Textual

let get_fresh_ident ({nodes; fresh_ident} : ProcDesc.t) =
  match fresh_ident with
  | None ->
      let idents =
        (* all the idents that are defined in the procdesc *)
        List.fold nodes ~init:Ident.Set.empty ~f:(fun set (node : Node.t) ->
            let set =
              List.fold node.ssa_parameters ~init:set ~f:(fun set (id, _) -> Ident.Set.add id set)
            in
            List.fold node.instrs ~init:set ~f:(fun set (instr : Instr.t) ->
                match instr with
                | Load {id} | Let {id= Some id} ->
                    Ident.Set.add id set
                | Store _ | Prune _ | Let _ ->
                    set ) )
      in
      Ident.fresh idents
  | Some fresh_ident ->
      fresh_ident


let create_fresh_label_generator ~prefix ({nodes} : ProcDesc.t) =
  let labels : NodeName.Set.t =
    List.fold nodes ~init:NodeName.Set.empty ~f:(fun set (node : Node.t) ->
        NodeName.Set.add node.label set )
  in
  let counter = ref 0 in
  let rec fresh_label () =
    let name : NodeName.t = {value= Printf.sprintf "%s%d" prefix !counter; loc= Location.Unknown} in
    incr counter ;
    if NodeName.Set.mem name labels then fresh_label () else name
  in
  fresh_label


let module_map_procs ~f (module_ : Module.t) =
  let open Module in
  let decls =
    List.map module_.decls ~f:(fun decl ->
        match decl with Proc pdesc -> Proc (f pdesc) | Global _ | Struct _ | Procdecl _ -> decl )
  in
  {module_ with decls}


let module_map_procdecl ~f (module_ : Module.t) =
  let open Module in
  let decls =
    List.map module_.decls ~f:(fun decl ->
        match decl with
        | Procdecl pdecl ->
            Procdecl (f pdecl)
        | Global _ | Struct _ | Proc _ ->
            decl )
  in
  {module_ with decls}


let module_fold_procs ~init ~f (module_ : Module.t) =
  let open Module in
  let decls, acc =
    List.fold module_.decls ~init:([], init) ~f:(fun (decls, acc) decl ->
        match decl with
        | Proc pdesc ->
            let pdesc, acc = f pdesc acc in
            (Proc pdesc :: decls, acc)
        | Global _ | Struct _ | Procdecl _ ->
            (decl :: decls, acc) )
  in
  ({module_ with decls= List.rev decls}, acc)


module FixClosureAppExpr = struct
  let is_ident =
    let regexp = Str.regexp "n[0-9]+" in
    fun string -> Str.string_match regexp string 0


  let of_procdesc globals pdesc =
    let open ProcDesc in
    if Option.is_some pdesc.fresh_ident then
      L.die InternalError "we assume fresh ident has not been computed yet" ;
    let globals_and_locals =
      List.fold pdesc.locals ~init:globals ~f:(fun set (varname, _) ->
          IString.Set.add varname.VarName.value set )
    in
    let is_varname ({enclosing_class; name} : QualifiedProcName.t) =
      match enclosing_class with
      | TopLevel when IString.Set.mem name.value globals_and_locals ->
          let varname : VarName.t = {value= name.value; loc= name.loc} in
          Some (Exp.Load {exp= Lvar varname; typ= None})
      | TopLevel when is_ident name.value ->
          let ident =
            Ident.of_int
              (int_of_string (String.sub name.value ~pos:1 ~len:(String.length name.value - 1)))
          in
          Some (Exp.Var ident)
      | _ ->
          None
    in
    let rec of_exp exp =
      let open Exp in
      match exp with
      | Var _ | Lvar _ | Const _ | Typ _ ->
          exp
      | If {cond; then_; else_} ->
          If {cond= of_bexp cond; then_= of_exp then_; else_= of_exp else_}
      | Load l ->
          Load {l with exp= of_exp l.exp}
      | Field f ->
          Field {f with exp= of_exp f.exp}
      | Index (exp1, exp2) ->
          Index (of_exp exp1, of_exp exp2)
      | Call {proc; args; kind} ->
          let args = List.map args ~f:of_exp in
          Option.value_map (is_varname proc)
            ~f:(fun closure -> Apply {closure; args})
            ~default:(Call {proc; args; kind})
      | Closure {proc; captured; params; attributes} ->
          Closure {proc; captured= List.map captured ~f:of_exp; params; attributes}
      | Apply {closure; args} ->
          Apply {closure= of_exp closure; args= List.map args ~f:of_exp}
    and of_bexp bexp =
      let open BoolExp in
      match bexp with
      | Exp exp ->
          Exp (of_exp exp)
      | Not bexp ->
          Not (of_bexp bexp)
      | And (bexp1, bexp2) ->
          And (of_bexp bexp1, of_bexp bexp2)
      | Or (bexp1, bexp2) ->
          Or (of_bexp bexp1, of_bexp bexp2)
    in
    let of_instr instr =
      let open Instr in
      match instr with
      | Load args ->
          Load {args with exp= of_exp args.exp}
      | Store args ->
          Store {args with exp1= of_exp args.exp1; exp2= of_exp args.exp2}
      | Prune args ->
          Prune {args with exp= of_exp args.exp}
      | Let args ->
          Let {args with exp= of_exp args.exp}
    in
    let rec of_terminator t =
      let open Terminator in
      match t with
      | If {bexp; then_; else_} ->
          If {bexp= of_bexp bexp; then_= of_terminator then_; else_= of_terminator else_}
      | Ret exp ->
          Ret (of_exp exp)
      | Jump node_call_list ->
          let f {label; ssa_args} = {label; ssa_args= List.map ssa_args ~f:of_exp} in
          Jump (List.map node_call_list ~f)
      | Throw exp ->
          Throw (of_exp exp)
      | Unreachable ->
          t
    in
    let of_node node =
      let open Node in
      {node with last= of_terminator node.last; instrs= List.map ~f:of_instr node.instrs}
    in
    {pdesc with nodes= List.map pdesc.nodes ~f:of_node}


  let transform (module_ : Module.t) =
    let open Module in
    let globals =
      List.fold module_.decls ~init:IString.Set.empty ~f:(fun set decl ->
          match decl with
          | Global {name} ->
              IString.Set.add name.VarName.value set
          | Proc _ | Struct _ | Procdecl _ ->
              set )
    in
    let decls =
      List.map module_.decls ~f:(fun decl ->
          match decl with
          | Proc pdesc ->
              Proc (of_procdesc globals pdesc)
          | Global _ | Struct _ | Procdecl _ ->
              decl )
    in
    {module_ with decls}
end

let fix_closure_app = FixClosureAppExpr.transform

module FixHackWrapper = struct
  let is_forward_to_static_method_wrapper (pdesc : ProcDesc.t) =
    (* hackc generates two kinds of wrapper but we are not interested in the
       forward-to-static-method case *)
    Int.equal (List.length pdesc.nodes) 1


  let replace_forward_call ({procdecl; nodes} as pdesc : ProcDesc.t) =
    (* we expect the last node to be of the form:
       n0: *HackMixed = load &$param1
       n1: *HackMixed = load &$param2
       ...
       n_{k+1} = n_k.?.name(n0, n1, ...)
       ret n_{k+1}
    *)
    let nodes =
      match List.rev nodes with
      | [] ->
          []
      | last_node :: others ->
          let instrs =
            List.map last_node.Node.instrs ~f:(function
              | Instr.Let {id; exp= Exp.Call {proc; args; kind= Virtual}; loc} ->
                  let enclosing_class = procdecl.qualified_name.enclosing_class in
                  let proc : QualifiedProcName.t = {proc with enclosing_class} in
                  let instr = Instr.Let {id; exp= Exp.Call {proc; args; kind= NonVirtual}; loc} in
                  instr
              | instr ->
                  instr )
          in
          {last_node with Node.instrs} :: others |> List.rev
    in
    (* no need to update fresh_ident *)
    {pdesc with nodes}


  let transform module_ =
    module_map_procs module_ ~f:(fun pdesc : ProcDesc.t ->
        let has_wrapper_attr =
          List.exists pdesc.procdecl.attributes ~f:Textual.Attr.is_hack_wrapper
        in
        if has_wrapper_attr && not (is_forward_to_static_method_wrapper pdesc) then
          replace_forward_call pdesc
        else pdesc )
end

module FixHackInvokeClosure = struct
  let fix_proc_name (type_name : TypeName.t) (name : ProcName.t) =
    let str_name = type_name.name.value in
    match String.substr_index_all str_name ~may_overlap:false ~pattern:"::" |> List.last with
    | Some last_pos ->
        let chopped_str_name =
          String.sub str_name ~pos:0 ~len:last_pos |> String.chop_prefix_exn ~prefix:"Closure$"
        in
        let chopped_type_name =
          {type_name with TypeName.name= {type_name.name with BaseTypeName.value= chopped_str_name}}
        in
        let chopped_static_type_name =
          { type_name with
            TypeName.name= {type_name.name with BaseTypeName.value= chopped_str_name ^ "$static"} }
        in
        ( chopped_type_name
        , {QualifiedProcName.enclosing_class= Enclosing chopped_static_type_name; name} )
    | _ ->
        (* should not happen *)
        (type_name, {QualifiedProcName.enclosing_class= Enclosing type_name; name})


  let transform_invoke hashtbl (pdesc : ProcDesc.t) =
    let qualified_procname = pdesc.procdecl.qualified_name in
    let is_async = List.exists ~f:Attr.is_async pdesc.procdecl.attributes in
    if QualifiedProcName.is_hack_closure_generated_invoke qualified_procname then
      match (pdesc.nodes, qualified_procname.enclosing_class) with
      | [node], Enclosing class_name -> (
          let rev_instrs = List.rev node.instrs in
          let rev_instrs, last_await_call_option =
            if is_async && not (List.is_empty rev_instrs) then
              (List.tl_exn rev_instrs, List.hd rev_instrs)
            else (rev_instrs, None)
          in
          match rev_instrs with
          | Instr.Let
              { id= Some ret_id
              ; exp=
                  Call
                    { proc= {enclosing_class= Enclosing class_name_call; name} as to_be_fixed_proc
                    ; args
                    ; kind= NonVirtual }
              ; loc= loc1 }
            :: Instr.Load {id= cls_id; exp= Lvar _ (* &$this *); loc= loc2}
            :: rev_instrs
            when TypeName.equal class_name_call class_name ->
              let fixed_typed_name, fixed_proc = fix_proc_name class_name_call name in
              let fixed_call_exp = Exp.Call {proc= fixed_proc; args; kind= NonVirtual} in
              let fixed_call_instr = Instr.Let {id= Some ret_id; exp= fixed_call_exp; loc= loc1} in
              let fixed_lazy_init_instr =
                Instr.Let
                  { id= Some cls_id
                  ; exp=
                      Call
                        { proc= ProcDecl.lazy_class_initialize_builtin
                        ; args= [Exp.Typ (Typ.Struct fixed_typed_name)]
                        ; kind= NonVirtual }
                  ; loc= loc2 }
              in
              let rev_instrs = fixed_call_instr :: fixed_lazy_init_instr :: rev_instrs in
              let rev_instrs = IList.opt_cons last_await_call_option rev_instrs in
              let instrs = List.rev rev_instrs in
              QualifiedProcName.Hashtbl.replace hashtbl to_be_fixed_proc fixed_proc ;
              {pdesc with nodes= [{node with instrs}]}
          | _ ->
              pdesc )
      | _, _ ->
          pdesc
    else pdesc


  let fix_decls hashtbl module_ =
    module_map_procdecl module_ ~f:(fun ({qualified_name} as pdecl) ->
        QualifiedProcName.Hashtbl.find_opt hashtbl qualified_name
        |> Option.value_map ~default:pdecl ~f:(fun qualified_name -> {pdecl with qualified_name}) )


  let transform module_ =
    let hashtbl = QualifiedProcName.Hashtbl.create 16 in
    let module_ = module_map_procs module_ ~f:(transform_invoke hashtbl) in
    if QualifiedProcName.Hashtbl.length hashtbl > 0 then fix_decls hashtbl module_ else module_
end

module Subst = struct
  let rec of_exp_one exp ~id ~by =
    let open Exp in
    match exp with
    | Var id' when Ident.equal id id' ->
        by
    | Var _ | Lvar _ | Const _ | Typ _ ->
        exp
    | Load l ->
        Load {l with exp= of_exp_one l.exp ~id ~by}
    | Field f ->
        Field {f with exp= of_exp_one f.exp ~id ~by}
    | Index (exp1, exp2) ->
        Index (of_exp_one exp1 ~id ~by, of_exp_one exp2 ~id ~by)
    | If _ ->
        L.die InternalError "TODO: Textual If statement"
    | Call f ->
        Call {f with args= List.map f.args ~f:(fun exp -> of_exp_one exp ~id ~by)}
    | Closure {proc; captured; params; attributes} ->
        Closure
          { proc
          ; captured= List.map captured ~f:(fun exp -> of_exp_one exp ~id ~by)
          ; params
          ; attributes }
    | Apply {closure; args} ->
        Apply
          { closure= of_exp_one closure ~id ~by
          ; args= List.map args ~f:(fun exp -> of_exp_one exp ~id ~by) }


  let rec of_exp exp eqs =
    let open Exp in
    match exp with
    | Var id ->
        Ident.Map.find_opt id eqs |> Option.value ~default:exp
    | Lvar _ | Const _ | Typ _ ->
        exp
    | Load l ->
        Load {l with exp= of_exp l.exp eqs}
    | Field f ->
        Field {f with exp= of_exp f.exp eqs}
    | Index (exp1, exp2) ->
        Index (of_exp exp1 eqs, of_exp exp2 eqs)
    | If _ ->
        L.die InternalError "TODO: Textual If statement"
    | Call f ->
        Call {f with args= List.map f.args ~f:(fun exp -> of_exp exp eqs)}
    | Closure {proc; captured; params; attributes} ->
        Closure
          {proc; captured= List.map captured ~f:(fun exp -> of_exp exp eqs); params; attributes}
    | Apply {closure; args} ->
        Apply {closure= of_exp closure eqs; args= List.map args ~f:(fun exp -> of_exp exp eqs)}


  let of_instr instr eqs =
    let open Instr in
    match instr with
    | Load args ->
        Load {args with exp= of_exp args.exp eqs}
    | Store args ->
        Store {args with exp1= of_exp args.exp1 eqs; exp2= of_exp args.exp2 eqs}
    | Prune args ->
        Prune {args with exp= of_exp args.exp eqs}
    | Let args ->
        Let {args with exp= of_exp args.exp eqs}


  let of_terminator t eqs =
    let open Terminator in
    match t with
    | If _ ->
        L.die InternalError "subst should not be called on If terminator"
    | Ret exp ->
        Ret (of_exp exp eqs)
    | Jump node_call_list ->
        let f {label; ssa_args} =
          {label; ssa_args= List.map ssa_args ~f:(fun exp -> of_exp exp eqs)}
        in
        Jump (List.map node_call_list ~f)
    | Throw exp ->
        Throw (of_exp exp eqs)
    | Unreachable ->
        t


  let of_node node eqs =
    let open Node in
    let rev_instrs =
      List.fold node.instrs ~init:[] ~f:(fun rev_instrs (instr : Instr.t) ->
          match instr with
          | Let {id= Some id} when Ident.Map.mem id eqs ->
              rev_instrs
          | _ ->
              of_instr instr eqs :: rev_instrs )
    in
    let instrs = List.rev rev_instrs in
    {node with last= of_terminator node.last eqs; instrs}


  let of_procdesc pdesc eqs =
    let open ProcDesc in
    (* no need to update fresh_ident *)
    {pdesc with nodes= List.map pdesc.nodes ~f:(fun node -> of_node node eqs)}
end

module State = struct
  module ClosureDeclarations = struct
    type t = {typenames: TypeName.Set.t; decls: Module.decl list}

    let empty = {typenames= TypeName.Set.empty; decls= []}

    let is_empty {typenames} = TypeName.Set.is_empty typenames

    let add struct_ procdecl {typenames; decls} =
      let name = struct_.Struct.name in
      (* we assume [name] is not already in set [typnames] *)
      {typenames= TypeName.Set.add name typenames; decls= Struct struct_ :: Proc procdecl :: decls}


    let mem name {typenames} = TypeName.Set.mem name typenames
  end

  type t =
    { instrs_rev: Instr.t list
    ; fresh_ident: Ident.t
    ; may_need_iteration: bool
    ; pdesc: ProcDesc.t
    ; closure_declarations: ClosureDeclarations.t }

  let add_closure_declaration struct_ procdecl state =
    { state with
      closure_declarations= ClosureDeclarations.add struct_ procdecl state.closure_declarations }


  let incr_fresh state = {state with fresh_ident= Ident.next state.fresh_ident}

  let insert_load state loc exp typ =
    let fresh = state.fresh_ident in
    let state = incr_fresh state in
    let load = Instr.Load {id= fresh; exp; typ= Some typ; loc} in
    (state, Exp.Var fresh, load)


  let push_instr instr state =
    match instr with
    | Instr.Let {id= None; exp; loc} ->
        let fresh = state.fresh_ident in
        let state = incr_fresh state in
        let instr = Instr.Let {id= Some fresh; exp; loc} in
        {state with instrs_rev= instr :: state.instrs_rev}
    | _ ->
        {state with instrs_rev= instr :: state.instrs_rev}
end

module TransformClosures = struct
  let rec name_of_attribute = function
    | [] ->
        None
    | {Textual.Attr.name= "name"; values= [name]} :: _ ->
        Some name
    | _ :: l ->
        name_of_attribute l


  let typename ~fresh_closure_counter sourcefile attributes loc : TypeName.t =
    (* we create a new type for this closure *)
    let value =
      match name_of_attribute attributes with
      | None ->
          let filename = F.asprintf "%a" SourceFile.pp sourcefile in
          let name, _ = Filename.split_extension filename in
          let id = fresh_closure_counter () in
          Printf.sprintf "%s:%d" name id
      | Some name ->
          name
    in
    {name= {value= "PyClosure"; loc}; args= [{name= {value; loc}; args= []}]}


  let mk_fielddecl (varname : VarName.t) (typ : Typ.t) : FieldDecl.t =
    (* each captured variable will be associated to an instance field *)
    let qualified_name : qualified_fieldname =
      {enclosing_class= TypeName.wildcard; name= {value= varname.value; loc= varname.loc}}
    in
    {qualified_name; typ; attributes= []}


  let signature_body lang proc captured params =
    Exp.call_sig proc (List.length captured + List.length params) lang


  let closure_building_instrs id loc typename (closure : ProcDesc.t) captured =
    let alloc : Instr.t = Let {id= Some id; loc; exp= Exp.allocate_object typename} in
    let formals =
      Option.value_exn ~message:"implementation always have formals" closure.procdecl.formals_types
    in
    let typed_params = List.zip_exn closure.params formals in
    let instrs, fields, _ =
      List.fold typed_params ~init:([], [], captured)
        ~f:(fun (instrs, fields, captured) (varname, ({typ} : Typ.annotated)) ->
          match captured with
          | [] ->
              (instrs, fields, [])
          | arg :: captured ->
              let fielddecl = mk_fielddecl varname typ in
              let field = fielddecl.qualified_name in
              let instr : Instr.t =
                Store {exp1= Field {exp= Var id; field}; typ= Some typ; exp2= arg; loc}
              in
              (instr :: instrs, fielddecl :: fields, captured) )
    in
    (alloc :: List.rev instrs, List.rev fields)


  let type_declaration name fields : Struct.t =
    {name; supers= []; fields; attributes= [Attr.mk_final]}


  let closure_call_qualified_procname loc : QualifiedProcName.t =
    {enclosing_class= Enclosing TypeName.wildcard; name= {value= "call"; loc}}


  let closure_call_exp loc closure args : Exp.t =
    Call {proc= closure_call_qualified_procname loc; args= closure :: args; kind= Virtual}


  let closure_call_procdecl loc typename (closure : ProcDesc.t) nb_captured : ProcDecl.t =
    let procdecl = closure.procdecl in
    let attributes =
      Textual.Attr.mk_closure_wrapper
      :: (* in Python, we transfert the 'args' and 'async' attributes from the 'closure' proc to the generated 'call' proc *)
         List.filter procdecl.attributes ~f:(fun attr ->
             Option.is_some (Textual.Attr.find_python_args attr) || Textual.Attr.is_async attr )
    in
    let unresolved_qualified_name = closure_call_qualified_procname loc in
    let qualified_name = {unresolved_qualified_name with enclosing_class= Enclosing typename} in
    let this_typ = Typ.mk_without_attributes (Ptr (Struct typename)) in
    let formals_types =
      Option.map procdecl.formals_types ~f:(fun formals ->
          this_typ :: List.drop formals nb_captured )
    in
    let result_type = procdecl.result_type in
    {qualified_name; formals_types; result_type; attributes}


  let closure_call_procdesc loc typename state (closure : ProcDesc.t) fields params :
      State.t * ProcDesc.t =
    let nb_captured = List.length fields in
    let save_fresh_ident = state.State.fresh_ident in
    let state = {state with State.fresh_ident= Ident.of_int 0} in
    let procdecl = closure_call_procdecl loc typename closure nb_captured in
    let start : NodeName.t = {value= "entry"; loc} in
    let this_var : VarName.t = {value= "__this"; loc} in
    let state, args, instrs =
      List.fold fields ~init:(state, [], [])
        ~f:(fun (state, args, instrs) ({qualified_name= field; typ} : FieldDecl.t) ->
          let state, exp, load1 =
            State.insert_load state loc (Exp.Lvar this_var) Typ.(Ptr (Struct typename))
          in
          let state, exp, load2 = State.insert_load state loc (Exp.Field {exp; field}) typ in
          (state, exp :: args, load2 :: load1 :: instrs) )
    in
    let formals = ProcDesc.formals closure in
    let params_types = List.drop formals nb_captured in
    if not (Int.equal (List.length params_types) (List.length params)) then
      L.die InternalError "ill-formed closure at %a@\n" Location.pp_line loc ;
    let state, args, instrs =
      List.fold2_exn params params_types ~init:(state, args, instrs)
        ~f:(fun (state, args, instrs) vname {Typ.typ} ->
          let state, exp, load = State.insert_load state loc (Exp.Lvar vname) typ in
          (state, exp :: args, load :: instrs) )
    in
    let node : Node.t =
      let exp =
        Exp.Call {proc= closure.procdecl.qualified_name; args= List.rev args; kind= NonVirtual}
      in
      let fresh_ident = state.State.fresh_ident in
      let instr = Instr.Let {id= Some fresh_ident; exp; loc} in
      let last : Terminator.t = Ret (Var fresh_ident) in
      { label= start
      ; ssa_parameters= []
      ; exn_succs= []
      ; last
      ; instrs= List.rev (instr :: instrs)
      ; last_loc= loc
      ; label_loc= loc }
    in
    let params = this_var :: params in
    let fresh_ident = Some (Ident.next state.fresh_ident) in
    let state = {state with fresh_ident= save_fresh_ident} in
    (state, {procdecl; nodes= [node]; fresh_ident; start; params; locals= []; exit_loc= loc})
end

module RemoveIf = struct
  (* first transform if conditions into disjunctions of conjunctions of possibly-negated
     atoms (expressions) then use this form to build the CFG by jumping to each disjunct
     non-deterministically and in each of them asserting each conjunct in sequence *)
  let negative_normal_form bexp =
    let rec pos (bexp : BoolExp.t) : BoolExp.t =
      match bexp with
      | Exp _ ->
          bexp
      | Not bexp ->
          neg bexp
      | And (bexp1, bexp2) ->
          And (pos bexp1, pos bexp2)
      | Or (bexp1, bexp2) ->
          Or (pos bexp1, pos bexp2)
    and neg (bexp : BoolExp.t) : BoolExp.t =
      match bexp with
      | Exp exp ->
          Exp (Exp.not exp)
      | Not bexp ->
          pos bexp
      | And (bexp1, bexp2) ->
          Or (neg bexp1, neg bexp2)
      | Or (bexp1, bexp2) ->
          And (neg bexp1, neg bexp2)
    in
    pos bexp


  let disjunctive_normal_form bexp =
    let open BoolExp in
    let rec dnf (bexp : BoolExp.t) : BoolExp.t =
      match bexp with
      | Exp _ ->
          bexp
      | Not _ ->
          L.die InternalError "disjunctive_normal_form does not expect Not case"
      | Or (bexp1, bexp2) ->
          Or (dnf bexp1, dnf bexp2)
      | And (bexp1, bexp2) -> (
          let bexp1 = dnf bexp1 in
          let bexp2 = dnf bexp2 in
          match (bexp1, bexp2) with
          | Or (case1, case2), _ ->
              Or (dnf (And (case1, bexp2)), dnf (And (case2, bexp2)))
          | _, Or (case1, case2) ->
              Or (dnf (And (bexp1, case1)), dnf (And (bexp1, case2)))
          | _, _ ->
              And (bexp1, bexp2) )
    in
    dnf bexp


  let transform_exp (pdesc : ProcDesc.t) =
    let fresh_label = create_fresh_label_generator ~prefix:"if_exp" pdesc in
    let rec transform_instrs rev_instrs rev_nodes node instrs =
      match instrs with
      | Instr.Let {id= Some id; exp= If {cond; then_; else_}; loc} :: instrs ->
          let next_label = fresh_label () in
          let mk_branch_node exp : Node.t =
            { label= fresh_label ()
            ; ssa_parameters= []
            ; exn_succs= []
            ; last= Jump [{label= next_label; ssa_args= [exp]}]
            ; instrs= []
            ; last_loc= loc
            ; label_loc= loc }
          in
          let typ =
            Typ.(Ptr Void)
            (* TODO find a better type *)
          in
          let next_node : Node.t =
            {node with label= next_label; ssa_parameters= [(id, typ)]; instrs; label_loc= loc}
          in
          let then_node = mk_branch_node then_ in
          let else_node = mk_branch_node else_ in
          let interrupted_node : Node.t =
            { node with
              last=
                If
                  { bexp= cond
                  ; then_= Jump [{label= then_node.label; ssa_args= []}]
                  ; else_= Jump [{label= else_node.label; ssa_args= []}] }
            ; instrs= List.rev rev_instrs
            ; last_loc= loc }
          in
          let rev_nodes = then_node :: else_node :: interrupted_node :: rev_nodes in
          transform_instrs [] rev_nodes next_node instrs
      | instr :: instrs ->
          transform_instrs (instr :: rev_instrs) rev_nodes node instrs
      | [] ->
          {node with Node.instrs= List.rev rev_instrs} :: rev_nodes
    in
    let rev_nodes =
      List.fold pdesc.nodes ~init:[] ~f:(fun rev_nodes node ->
          transform_instrs [] rev_nodes node node.Node.instrs )
    in
    {pdesc with nodes= List.rev rev_nodes}


  let transform_terminator pdesc =
    let fresh_label = create_fresh_label_generator ~prefix:"if" pdesc in
    let predecessors_count : NodeName.t -> int =
      (* we count how many predecessors a node has *)
      let get label map = NodeName.Map.find_opt label map |> Option.value ~default:0 in
      let map =
        List.fold pdesc.ProcDesc.nodes ~init:NodeName.Map.empty ~f:(fun map (node : Node.t) ->
            let rec succs (terminator : Terminator.t) =
              match terminator with
              | If {then_; else_} ->
                  succs then_ @ succs else_
              | Ret _ | Throw _ | Unreachable ->
                  []
              | Jump l ->
                  l
            in
            succs node.Node.last
            |> List.fold ~init:map ~f:(fun map ({label} : Terminator.node_call) ->
                   let count = get label map in
                   NodeName.Map.add label (count + 1) map ) )
      in
      fun label -> get label map
    in
    let rec collect_conjuncts bexp conjuncts =
      (* should be called after disjunctive_normal_form and negative_normal_form *)
      match (bexp : BoolExp.t) with
      | Exp exp ->
          exp :: conjuncts
      | Not _ ->
          L.die InternalError "not expected"
      | And (bexp1, bexp2) ->
          collect_conjuncts bexp2 conjuncts |> collect_conjuncts bexp1
      | Or _ ->
          L.die InternalError "not expected"
    in
    let rec collect_disjuncts bexp disjuncts =
      (* should be called after disjunctive_normal_form and negative_normal_form *)
      match (bexp : BoolExp.t) with
      | Or (bexp1, bexp2) ->
          collect_disjuncts bexp2 disjuncts |> collect_disjuncts bexp1
      | _ ->
          collect_conjuncts bexp [] :: disjuncts
    in
    let mk_extra_nodes loc bexp terminator patch =
      (* the patch map records if a node (with a single predecessor) needs to
         receive a preambule of prune instructions *)
      let bexp = bexp |> negative_normal_form |> disjunctive_normal_form in
      let disjuncts = collect_disjuncts bexp [] in
      match (disjuncts, (terminator : Terminator.t)) with
      | [], _ ->
          L.die InternalError "0 disjuncts is not possible"
      | [conjuncts], Jump [target] when Int.equal (predecessors_count target.label) 1 ->
          (* we can directly add the prune instructions in the target *)
          let instrs = List.map conjuncts ~f:(fun exp -> Instr.Prune {exp; loc}) in
          ([], [target.label], NodeName.Map.add target.label instrs patch)
      | _ ->
          let extra_nodes =
            List.rev_map disjuncts ~f:(fun conjuncts : Node.t ->
                let label = fresh_label () in
                let instrs = List.map conjuncts ~f:(fun exp -> Instr.Prune {exp; loc}) in
                { label
                ; ssa_parameters= []
                ; exn_succs= []
                ; last= terminator
                ; instrs
                ; last_loc= loc
                ; label_loc= loc } )
          in
          let targets = List.map extra_nodes ~f:(fun (node : Node.t) -> node.label) in
          (extra_nodes, targets, patch)
    in
    let rec remove_if loc terminator extra_nodes patch =
      match (terminator : Terminator.t) with
      | If {bexp; then_; else_} ->
          let then_, extra_nodes, patch = remove_if loc then_ extra_nodes patch in
          let else_, extra_nodes, patch = remove_if loc else_ extra_nodes patch in
          let extra_nodes_false, targets_false, patch =
            mk_extra_nodes loc (BoolExp.Not bexp) else_ patch
          in
          let extra_nodes_true, targets_true, patch = mk_extra_nodes loc bexp then_ patch in
          let new_extra_nodes = extra_nodes_false @ extra_nodes_true in
          let targets : Terminator.node_call list =
            List.rev_map (targets_false @ targets_true) ~f:(fun label ->
                {Terminator.label; ssa_args= []} )
          in
          (Terminator.Jump targets, extra_nodes @ new_extra_nodes, patch)
      | _ ->
          (terminator, extra_nodes, patch)
    in
    let nodes = pdesc.ProcDesc.nodes in
    let rev_nodes, patch =
      List.fold nodes ~init:([], NodeName.Map.empty) ~f:(fun (nodes, patch) (node : Node.t) ->
          let last, extra_nodes, patch = remove_if node.last_loc node.last [] patch in
          (extra_nodes @ ({node with last} :: nodes), patch) )
    in
    let nodes =
      List.fold rev_nodes ~init:[] ~f:(fun nodes (node : Node.t) ->
          let label = node.label in
          if NodeName.Map.mem label patch then
            let prelude = NodeName.Map.find label patch in
            {node with instrs= prelude @ node.instrs} :: nodes
          else node :: nodes )
    in
    (* no need to update fresh_ident *)
    {pdesc with ProcDesc.nodes}


  let transform_pdesc pdesc = transform_exp pdesc |> transform_terminator

  let run module_ = module_map_procs ~f:transform_pdesc module_
end

let remove_if_exp_and_terminator = RemoveIf.run

let rec exp_needs_flattening (exp : Exp.t) =
  match exp with
  | Var _ | Lvar _ | Const _ | Typ _ ->
      false
  | Load _ | Closure _ | Apply _ ->
      true
  | Field f ->
      exp_needs_flattening f.exp
  | Index (exp1, exp2) ->
      exp_needs_flattening exp1 || exp_needs_flattening exp2
  | If {cond; then_; else_} ->
      bexp_needs_flattening cond || exp_needs_flattening then_ || exp_needs_flattening else_
  | Call {args} ->
      List.exists ~f:exp_needs_flattening args


and bexp_needs_flattening (bexp : BoolExp.t) =
  match bexp with
  | Exp exp ->
      exp_needs_flattening exp
  | Not bexp ->
      bexp_needs_flattening bexp
  | And (bexp1, bexp2) ->
      bexp_needs_flattening bexp1 || bexp_needs_flattening bexp2
  | Or (bexp1, bexp2) ->
      bexp_needs_flattening bexp1 || bexp_needs_flattening bexp2


let rec terminator_needs_flattening (last : Terminator.t) =
  match last with
  | If {bexp; then_; else_} ->
      bexp_needs_flattening bexp || terminator_needs_flattening then_
      || terminator_needs_flattening else_
  | Ret exp ->
      exp_needs_flattening exp
  | Jump node_calls ->
      List.exists node_calls ~f:(fun {Terminator.ssa_args} ->
          List.exists ~f:exp_needs_flattening ssa_args )
  | Throw exp ->
      exp_needs_flattening exp
  | Unreachable ->
      false


let remove_effects_in_subexprs lang decls_env module_ =
  let fresh_closure_counter =
    let counter = ref (-1) in
    fun () ->
      incr counter ;
      !counter
  in
  let rec flatten_exp loc ?(toplevel = false) (exp : Exp.t) state : Exp.t * State.t =
    match exp with
    | Var _ | Lvar _ | Const _ | Typ _ ->
        (exp, state)
    | Load {exp; typ} ->
        let exp, state = flatten_exp loc exp state in
        let fresh = state.State.fresh_ident in
        let new_instr : Instr.t = Load {id= fresh; exp; typ; loc} in
        (Var fresh, State.push_instr new_instr state |> State.incr_fresh)
    | Field f ->
        let exp, state = flatten_exp loc f.exp state in
        (Field {f with exp}, state)
    | Index (exp1, exp2) ->
        let exp1, state = flatten_exp loc exp1 state in
        let exp2, state = flatten_exp loc exp2 state in
        (Index (exp1, exp2), state)
    | If {cond; then_; else_} when toplevel ->
        let cond, state = flatten_bexp loc cond state in
        let exp = Exp.If {cond; then_; else_} in
        let state = {state with State.may_need_iteration= exp_needs_flattening exp} in
        (exp, state)
    | If {cond; then_; else_} ->
        let cond, state = flatten_bexp loc cond state in
        let fresh = state.State.fresh_ident in
        let exp = Exp.If {cond; then_; else_} in
        let new_instr : Instr.t = Let {id= Some fresh; exp; loc} in
        let state = {state with State.may_need_iteration= exp_needs_flattening exp} in
        (Var fresh, State.push_instr new_instr state |> State.incr_fresh)
    | Call {proc; args; kind} ->
        let args, state = flatten_exp_list loc args state in
        if ProcDecl.is_side_effect_free_sil_expr proc then (Call {proc; args; kind}, state)
        else
          let fresh = state.State.fresh_ident in
          let new_instr : Instr.t = Let {id= Some fresh; exp= Call {proc; args; kind}; loc} in
          (Var fresh, State.push_instr new_instr state |> State.incr_fresh)
    | Closure {proc; captured; params; attributes} ->
        let captured, state = flatten_exp_list loc captured state in
        let signature = TransformClosures.signature_body lang proc captured params in
        let closure =
          match TextualDecls.get_procdesc decls_env signature with
          | Some procdecl ->
              procdecl
          | None ->
              L.die InternalError "TextualBasicVerication should make this situation impossible"
        in
        let typename =
          TransformClosures.typename ~fresh_closure_counter module_.Module.sourcefile attributes loc
        in
        let id_object = state.State.fresh_ident in
        let state = State.incr_fresh state in
        let instrs, fields =
          TransformClosures.closure_building_instrs id_object loc typename closure captured
        in
        let state =
          List.fold instrs ~init:state ~f:(fun state instr -> State.push_instr instr state)
        in
        let struct_ = TransformClosures.type_declaration typename fields in
        let state = State.incr_fresh state in
        let loc = if Lang.equal lang Python then Location.decr_line loc else loc in
        let state =
          if not (State.ClosureDeclarations.mem typename state.closure_declarations) then
            let state, call_procdecl =
              TransformClosures.closure_call_procdesc loc typename state closure fields params
            in
            State.add_closure_declaration struct_ call_procdecl state
          else state
        in
        (Var id_object, state)
    | Apply {closure; args} ->
        let closure, state = flatten_exp loc closure state in
        let args, state = flatten_exp_list loc args state in
        let fresh = state.State.fresh_ident in
        let new_instr : Instr.t =
          Let {id= Some fresh; exp= TransformClosures.closure_call_exp loc closure args; loc}
        in
        (Var fresh, State.push_instr new_instr state |> State.incr_fresh)
  and flatten_exp_list loc exp_list state =
    let exp_list, state =
      List.fold exp_list ~init:([], state) ~f:(fun (args, state) exp ->
          let exp, state = flatten_exp loc exp state in
          (exp :: args, state) )
    in
    (List.rev exp_list, state)
  and flatten_bexp loc bexp state =
    match (bexp : BoolExp.t) with
    | Exp exp ->
        let exp, state = flatten_exp loc exp state in
        (BoolExp.Exp exp, state)
    | Not bexp ->
        let bexp, state = flatten_bexp loc bexp state in
        (BoolExp.Not bexp, state)
    | And (bexp1, bexp2) ->
        let bexp1, state = flatten_bexp loc bexp1 state in
        (* because of lazyness we only transform fst arg *)
        (BoolExp.And (bexp1, bexp2), state)
    | Or (bexp1, bexp2) ->
        let bexp1, state = flatten_bexp loc bexp1 state in
        (* because of lazyness we only transform fst arg *)
        (BoolExp.Or (bexp1, bexp2), state)
  in
  let flatten_in_instr (instr : Instr.t) state : State.t =
    match instr with
    | Load ({exp; loc} as args) ->
        let exp, state = flatten_exp loc exp state in
        State.push_instr (Load {args with exp}) state
    | Store ({exp1; exp2; loc} as args) ->
        let exp1, state = flatten_exp loc exp1 state in
        let exp2, state = flatten_exp loc exp2 state in
        State.push_instr (Store {args with exp1; exp2}) state
    | Prune {exp; loc} ->
        let exp, state = flatten_exp loc exp state in
        State.push_instr (Prune {exp; loc}) state
    | Let {id; exp= Call {proc; args; kind}; loc}
      when not (ProcDecl.is_side_effect_free_sil_expr proc) ->
        let args, state = flatten_exp_list loc args state in
        State.push_instr (Let {id; exp= Call {proc; args; kind}; loc}) state
    | Let {id; exp; loc} ->
        let exp, state = flatten_exp loc ~toplevel:true exp state in
        State.push_instr (Let {id; exp; loc}) state
  in
  let flatten_in_terminator loc (last : Terminator.t) state : Terminator.t * State.t =
    match last with
    | If {bexp; then_; else_} ->
        let bexp, state = flatten_bexp loc bexp state in
        let last = Terminator.If {bexp; then_; else_} in
        let may_need_iteration = terminator_needs_flattening last in
        let state = {state with State.may_need_iteration} in
        (last, state)
    | Ret exp ->
        let exp, state = flatten_exp loc exp state in
        (Ret exp, state)
    | Jump node_calls ->
        let node_calls_rev, state =
          List.fold node_calls ~init:([], state)
            ~f:(fun (node_calls, state) {Terminator.label; ssa_args} ->
              let ssa_args, state = flatten_exp_list loc ssa_args state in
              ({Terminator.label; ssa_args} :: node_calls, state) )
        in
        (Jump (List.rev node_calls_rev), state)
    | Throw exp ->
        let exp, state = flatten_exp loc exp state in
        (Throw exp, state)
    | Unreachable ->
        (last, state)
  in
  let flatten_node (node : Node.t) state =
    let state =
      let init : State.t = {state with instrs_rev= []} in
      List.fold node.instrs ~init ~f:(fun state instr -> flatten_in_instr instr state)
    in
    let last, ({instrs_rev} as state : State.t) =
      flatten_in_terminator node.last_loc node.last state
    in
    ({node with last; instrs= List.rev instrs_rev}, state)
  in
  let rec flatten_pdesc (pdesc : ProcDesc.t) closure_declarations =
    let fresh_ident = get_fresh_ident pdesc in
    let state : State.t =
      {pdesc; may_need_iteration= false; instrs_rev= []; fresh_ident; closure_declarations}
    in
    let rev_nodes, {State.fresh_ident; may_need_iteration; closure_declarations} =
      List.fold pdesc.nodes ~init:([], state) ~f:(fun (rev_nodes, state) node ->
          let node, state = flatten_node node state in
          (node :: rev_nodes, state) )
    in
    let pdesc = {pdesc with nodes= List.rev rev_nodes; fresh_ident= Some fresh_ident} in
    let pdesc = RemoveIf.transform_pdesc pdesc in
    if may_need_iteration then flatten_pdesc pdesc closure_declarations
    else (pdesc, closure_declarations)
  in
  let module_, closure_declarations =
    module_fold_procs ~init:State.ClosureDeclarations.empty ~f:flatten_pdesc module_
  in
  ( {module_ with decls= closure_declarations.decls @ module_.decls}
  , not (State.ClosureDeclarations.is_empty closure_declarations) )


(* TODO (T131910123): replace with STORE+LOAD transform *)
let let_propagation module_ =
  let get id ident_map =
    try Ident.Map.find id ident_map
    with Stdlib.Not_found ->
      L.die InternalError "Textual.let_propagation.get failed: unknown identifier %a" Ident.pp id
  in
  let build_equations pdesc : Exp.t Ident.Map.t =
    (* we collect all rule of the form [id = exp] where [exp] is not a regular call nor an
       allocation *)
    List.fold pdesc.ProcDesc.nodes ~init:Ident.Map.empty ~f:(fun eqs (node : Node.t) ->
        List.fold node.instrs ~init:eqs ~f:(fun eqs (instr : Instr.t) ->
            match instr with
            | Load _ | Store _ | Prune _ ->
                eqs
            | Let {id= None} ->
                L.die InternalError "let_propagation should come after remove_effects_in_subexprs"
            | Let {exp= Call {proc}} when not (ProcDecl.is_side_effect_free_sil_expr proc) ->
                eqs
            | Let {id= Some id; exp} ->
                Ident.Map.add id exp eqs ) )
  in
  let compute_dependencies equations : Ident.Set.t Ident.Map.t =
    (* for each equation we record which equation it depends on for its evaluation *)
    let domain = Ident.Map.fold (fun id _ set -> Ident.Set.add id set) equations Ident.Set.empty in
    let vars exp = Ident.Set.inter (Exp.vars exp) domain in
    Ident.Map.map vars equations
  in
  let sort_equations equations dependencies : Ident.t list =
    (* returns a topological sorted list of identifiers such that if the equation of [id1] depends
       on [id2], then [id1] is after [id2] in the list.
       [dependencies] must be equal to [compute_dependencies equations] *)
    let init = (Ident.Map.empty, []) in
    let rec visit id ((status, sorted_idents) as state) =
      match Ident.Map.find_opt id status with
      | Some `VisitInProgress ->
          L.die InternalError
            "Textual transformation error: sort_equation was given a set of equations with cyclic \
             dependencies"
      | Some `VisitCompleted ->
          state
      | None ->
          let status = Ident.Map.add id `VisitInProgress status in
          let vars = get id dependencies in
          let status, sorted_idents = Ident.Set.fold visit vars (status, sorted_idents) in
          (Ident.Map.add id `VisitCompleted status, id :: sorted_idents)
    in
    let _, sorted_idents =
      Ident.Map.fold
        (fun id _ ((status, _) as state) ->
          if Ident.Map.mem id status then state else visit id state )
        equations init
    in
    List.rev sorted_idents
  in
  let transform pdesc =
    let equations = build_equations pdesc in
    let dependencies = compute_dependencies equations in
    let sorted = sort_equations equations dependencies in
    (* we saturate the equation set (id1, exp1), .. (idn, expn) by rewriting
       enough in each expi such that none depends on id1, .., idn at the end *)
    let saturated_equations =
      List.fold sorted ~init:Ident.Map.empty ~f:(fun saturated_equations id ->
          let eq = get id equations in
          let vars = get id dependencies in
          let saturated_eq =
            Ident.Set.fold
              (fun id' exp ->
                (* thanks to the topological sort, id' has already been processed *)
                let saturated_eq' = get id' saturated_equations in
                Subst.of_exp_one exp ~id:id' ~by:saturated_eq' )
              vars eq
          in
          Ident.Map.add id saturated_eq saturated_equations )
    in
    Subst.of_procdesc pdesc saturated_equations
  in
  module_map_procs ~f:transform module_


let out_of_ssa module_ =
  let transform (pdesc : ProcDesc.t) : ProcDesc.t =
    let get_node : NodeName.t -> Node.t =
      let map =
        List.fold pdesc.nodes ~init:NodeName.Map.empty ~f:(fun map (node : Node.t) ->
            NodeName.Map.add node.label node map )
      in
      fun node ->
        try NodeName.Map.find node map
        with Stdlib.Not_found -> L.die InternalError "Textual.remove_ssa_params internal error"
    in
    (* Compute which nodes are handlers. This should *really* be done upfront in hackc but for now just see
       which ones are listed as handlers for some other other node. We'll assume that handlers are disjoint from
       non-handlers and that handlers all have exactly one parameter *)
    let handler_nodes =
      List.fold pdesc.nodes ~init:NodeName.Set.empty ~f:(fun accum (node : Node.t) ->
          List.fold node.exn_succs ~init:accum ~f:(fun accum succ -> NodeName.Set.add succ accum) )
    in
    let zip_ssa_args call_location (node_call : Terminator.node_call) (end_node : Node.t) :
        Instr.t list =
      match
        List.map2 end_node.ssa_parameters node_call.ssa_args ~f:(fun (id, typ) exp2 ->
            let var_name = Ident.to_ssa_var id in
            Instr.Store {exp1= Lvar var_name; typ= Some typ; exp2; loc= Location.Unknown} )
      with
      | Ok equations ->
          equations
      | Unequal_lengths ->
          L.die InternalError
            "Jmp arguments at %a and block parameters at %a should have the same size" Location.pp
            call_location Location.pp end_node.label_loc
    in
    let build_assignements (start_node : Node.t) : Instr.t list =
      match (start_node.last : Terminator.t) with
      | If _ ->
          L.die InternalError "out_of_ssa should not be called on If terminator"
      | Ret _ | Throw _ | Unreachable ->
          []
      | Jump node_calls ->
          List.fold node_calls ~init:[] ~f:(fun instrs (node_call : Terminator.node_call) ->
              let end_node : Node.t = get_node node_call.label in
              if List.is_empty end_node.ssa_parameters then instrs
              else
                let let_instrs = zip_ssa_args start_node.last_loc node_call end_node in
                List.rev_append let_instrs instrs )
    in
    let terminator_remove_args (terminator : Terminator.t) : Terminator.t =
      let node_call_remove_args (node_call : Terminator.node_call) : Terminator.node_call =
        {node_call with ssa_args= []}
      in
      match terminator with
      | If _ ->
          L.die InternalError "out_of_ssa should not be called on If terminator"
      | Throw _ | Ret _ | Unreachable ->
          terminator
      | Jump node_calls ->
          Jump (List.map node_calls ~f:node_call_remove_args)
    in
    (* We leave the ssa parameters (there should be only one) in for handlers
       and do the parameter loading at the top during the conversion to SIL, as only there
       do we have a return variable available to read from *)
    let nodes =
      List.map pdesc.nodes ~f:(fun (node : Node.t) ->
          let rev_instrs = build_assignements node in
          let prefix, ssa_parameters =
            if NodeName.Set.mem node.label handler_nodes then ([], node.ssa_parameters)
            else
              let load_param (id, typ) : Instr.t =
                Load {id; exp= Lvar (Ident.to_ssa_var id); typ= Some typ; loc= Location.Unknown}
              in
              let prefix = List.map node.Node.ssa_parameters ~f:load_param in
              (prefix, [])
          in
          let last = terminator_remove_args node.Node.last in
          let instrs =
            if List.is_empty rev_instrs then prefix @ node.Node.instrs
            else prefix @ node.Node.instrs @ List.rev rev_instrs
          in
          ({node with instrs; ssa_parameters; last} : Node.t) )
    in
    (* no need to update fresh_ident *)
    {pdesc with nodes}
  in
  module_map_procs ~f:transform module_


let run lang module_ =
  let errors, decls_env = TextualDecls.make_decls module_ in
  if not (List.is_empty errors) then
    L.die InternalError
      "to_sil conversion should not be performed if TextualDecls verification has raised any \
       errors before." ;
  let module_, new_decls_were_added = module_ |> remove_effects_in_subexprs lang decls_env in
  let decls_env =
    if new_decls_were_added then TextualDecls.make_decls module_ |> snd else decls_env
  in
  let module_ = module_ |> let_propagation |> out_of_ssa in
  (module_, decls_env)


module ClassGetTS = struct
  let get_class_ts_old_procname = ProcName.of_string "TODO_hhbc_ClassGetTS"

  let get_class_ts_new_procname_str = "hack_get_class_from_type"

  let combine_and_resolve_type_struct_procname =
    ProcName.of_string "hhbc_combine_and_resolve_type_struct"


  let hack_new_dict_procname = ProcName.of_string "hack_new_dict"

  let hhbc_new_vec_procname = ProcName.of_string "hhbc_new_vec"

  let hack_string_procname = ProcName.of_string "hack_string"

  let build_ident_instr_map (instructions : Instr.t list) =
    List.fold_left ~init:Ident.Map.empty
      ~f:(fun acc (instruction : Instr.t) ->
        match instruction with Let {id= Some id; exp} -> Ident.Map.add id exp acc | _ -> acc )
      instructions


  (*
    This function uses the info from ident_instr_map to inline call exprs,
    replacing identifiers found in arguments by their corresponding expressions
    E.g.
       foo(n$0) becomes
       foo(bar(...)),
       assuming that n$0 is previously assigned to bar(...)
  *)
  let rec inline_call_expr ident_instr_map (exp : Textual.Exp.t) =
    match exp with
    | Apply {closure; args} ->
        let new_args = List.map ~f:(inline_call_expr ident_instr_map) args in
        Textual.Exp.Apply {args= new_args; closure}
    | Var id -> (
      match Ident.Map.find_opt id ident_instr_map with
      | Some new_exp ->
          inline_call_expr ident_instr_map new_exp
      | None ->
          exp )
    | Call {proc; args; kind} ->
        let new_args = List.map ~f:(inline_call_expr ident_instr_map) args in
        Textual.Exp.Call {proc; args= new_args; kind}
    | _ ->
        exp


  (*
    This function performs the final transformation needed for the TODO_ClassGetTS usecase.
    For that, it searches for the following pattern:
    TODO_hhbc_ClassGetTS(
      $builtins.hhbc_combine_and_resolve_type_struct(
        $builtins.hack_new_dict(
          _,
          _,
          _,
          $builtins.hack_string(rootname: str),
          _,
          $builtins.hhbc_new_vec($builtins.hack_string(typname: str))
        )
      )
    )
    in order to replace it by the following: hack_get_class_from_type(rootname, typname)
  *)
  let transform_hhbc_class_get_ts_exp (exp : Textual.Exp.t) =
    match exp with
    | Call
        { proc= {name= {loc} as proc_get_class_ts}
        ; args=
            [ Textual.Exp.Call
                { proc= {name= proc_combine_and_resolve_ts}
                ; args= [Textual.Exp.Call {proc= {name= proc_new_dict}; args}] } ]
        ; kind }
      when ProcName.equal proc_get_class_ts get_class_ts_old_procname
           && ProcName.equal proc_combine_and_resolve_ts combine_and_resolve_type_struct_procname
           && ProcName.equal proc_new_dict hack_new_dict_procname -> (
      match List.rev args with
      | Call
          { proc= {name= proc_new_vec}
          ; args= [Textual.Exp.Call {proc= {name= proc_hack_str1}; args= args_type_name}] }
        :: _
        :: Call {proc= {name= proc_hack_str2}; args= args_obj}
        :: _
        when ProcName.equal proc_new_vec hhbc_new_vec_procname
             && ProcName.equal proc_hack_str1 hack_string_procname
             && ProcName.equal proc_hack_str2 hack_string_procname ->
          Textual.Exp.Call
            { proc=
                { name= {value= get_class_ts_new_procname_str; loc}
                ; enclosing_class= Enclosing (Textual.TypeName.of_string "$builtins") }
            ; args= args_obj @ args_type_name
            ; kind }
      | _ ->
          exp )
    | _ ->
        exp


  let transform_hhbc_class_get_ts_instruction instructions =
    let ident_instr_map = build_ident_instr_map instructions in
    match instructions with
    | [] ->
        instructions
    | (Let {id; exp; loc} : Instr.t) :: instrs ->
        Textual.Instr.Let
          {id; exp= transform_hhbc_class_get_ts_exp (inline_call_expr ident_instr_map exp); loc}
        :: instrs
    | _ ->
        instructions


  let rec transform_instructions instructions =
    match instructions with
    | [] ->
        []
    | (Let {exp= Call {proc= {name}}} as instruction : Instr.t) :: instructions
      when ProcName.equal name get_class_ts_old_procname ->
        transform_hhbc_class_get_ts_instruction (instruction :: instructions)
    | instruction :: instructions ->
        instruction :: transform_instructions instructions


  let transform_nodes ({nodes} : ProcDesc.t) =
    let new_nodes =
      List.map
        ~f:(fun ({instrs} as node : Node.t) ->
          let instructions = transform_instructions (List.rev instrs) in
          {node with instrs= List.rev instructions} )
        nodes
    in
    new_nodes


  let transform ({decls} as module_ : Module.t) =
    let new_decls =
      List.map
        ~f:(fun (decl : Module.decl) ->
          match decl with
          | Proc procdesc ->
              (* no need to update fresh_ident *)
              Module.Proc {procdesc with nodes= transform_nodes procdesc}
          | _ ->
              decl )
        decls
    in
    {module_ with decls= new_decls}
end

let fix_hackc_mistranslations module_ =
  module_ |> ClassGetTS.transform |> FixHackInvokeClosure.transform |> FixHackWrapper.transform
