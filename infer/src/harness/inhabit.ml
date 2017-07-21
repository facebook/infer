(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Generate a procedure that calls a given sequence of methods. Useful for harness/test
 * generation. *)

module L = Logging
module F = Format
module P = Printf
module IdSet = Ident.IdentSet
module TypSet = Typ.Set
module TypMap = Typ.Map

type lifecycle_trace = (Typ.Procname.t * Typ.t option) list

(** list of instrs and temporary variables created during inhabitation and a cache of types that
 * have already been inhabited *)
type env =
  { instrs: Sil.instr list
  ; cache: Exp.t TypMap.t
  ; (* set of types currently being inhabited. consult to prevent infinite recursion *)
  cur_inhabiting: TypSet.t
  ; pc: Location.t
  ; harness_name: Typ.Procname.java }

let procdesc_from_name cfg pname =
  let pdesc_ref = ref None in
  Cfg.iter_proc_desc cfg (fun cfg_pname pdesc ->
      if Typ.Procname.equal cfg_pname pname then pdesc_ref := Some pdesc ) ;
  !pdesc_ref

let formals_from_name cfg pname =
  match procdesc_from_name cfg pname with Some pdesc -> Procdesc.get_formals pdesc | None -> []

(** add an instruction to the env, update tmp_vars, and bump the pc *)
let env_add_instr instr env =
  let incr_pc pc = {pc with Location.line= pc.Location.line + 1} in
  {env with instrs= instr :: env.instrs; pc= incr_pc env.pc}

(** call flags for an allocation or call to a constructor *)
let cf_alloc = CallFlags.default

let fun_exp_from_name proc_name = Exp.Const (Const.Cfun proc_name)

let local_name_cntr = ref 0

let create_fresh_local_name () =
  incr local_name_cntr ;
  "dummy_local" ^ string_of_int !local_name_cntr

(** more forgiving variation of List.tl that won't raise an exception on the empty list *)
let tl_or_empty l = if List.is_empty l then l else List.tl_exn l

let get_non_receiver_formals formals = tl_or_empty formals

(** create Sil corresponding to x = new typ() or x = new typ[]. For ordinary allocation, sizeof_typ
 * and ret_typ should be the same, but arrays are slightly odd in that sizeof_typ will have a size
 * component but the size component of ret_typ is always -1. *)
let inhabit_alloc sizeof_typ sizeof_len ret_typ alloc_kind env =
  let retval = Ident.create_fresh Ident.knormal in
  let inhabited_exp = Exp.Var retval in
  let call_instr =
    let fun_new = fun_exp_from_name alloc_kind in
    let sizeof_exp =
      Exp.Sizeof {typ= sizeof_typ; nbytes= None; dynamic_length= sizeof_len; subtype= Subtype.exact}
    in
    let args = [(sizeof_exp, Typ.mk (Tptr (ret_typ, Typ.Pk_pointer)))] in
    Sil.Call (Some (retval, ret_typ), fun_new, args, env.pc, cf_alloc)
  in
  (inhabited_exp, env_add_instr call_instr env)

(* TODO: this should be done in a differnt way: just make typ a param of the harness procedure *)

(** find or create a Sil expression with type typ *)
let rec inhabit_typ tenv typ cfg env =
  try (TypMap.find typ env.cache, env)
  with Not_found ->
    let inhabit_internal typ env =
      match typ.Typ.desc with
      | Typ.Tptr ({desc= Tarray (inner_typ, Some _, _)}, Typ.Pk_pointer)
       -> let len = Exp.Const (Const.Cint IntLit.one) in
          let arr_typ = Typ.mk (Tarray (inner_typ, Some IntLit.one, None)) in
          inhabit_alloc arr_typ (Some len) typ BuiltinDecl.__new_array env
      | Typ.Tptr (typ, Typ.Pk_pointer)
       -> (* TODO (t4575417): this case does not work correctly for enums, but they are currently
           * broken in Infer anyway (see t4592290) *)
          let allocated_obj_exp, env = inhabit_alloc typ None typ BuiltinDecl.__new env in
          (* select methods that are constructors and won't force us into infinite recursion because
           * we are already inhabiting one of their argument types *)
          let get_all_suitable_constructors (typ: Typ.t) =
            match typ.desc with
            | Tstruct name when Typ.Name.is_class name -> (
              match Tenv.lookup tenv name with
              | Some {methods}
               -> let is_suitable_constructor p =
                    let try_get_non_receiver_formals p =
                      get_non_receiver_formals (formals_from_name cfg p)
                    in
                    Typ.Procname.is_constructor p
                    && List.for_all
                         ~f:(fun (_, typ) -> not (TypSet.mem typ env.cur_inhabiting))
                         (try_get_non_receiver_formals p)
                  in
                  List.filter ~f:(fun p -> is_suitable_constructor p) methods
              | _
               -> [] )
            | _
             -> []
          in
          let env, typ_class_name =
            match get_all_suitable_constructors typ with
            | constructor :: _
             -> (* arbitrarily choose a constructor for typ and invoke it. eventually, we may want to
                 * nondeterministically call all possible constructors instead *)
                let env = inhabit_constructor tenv constructor (allocated_obj_exp, typ) cfg env in
                (* try to get the unqualified name as a class (e.g., Object for java.lang.Object so we
                 * we can use it as a descriptive local variable name in the harness *)
                let typ_class_name =
                  match constructor with
                  | Typ.Procname.Java pname_java
                   -> Typ.Procname.java_get_simple_class_name pname_java
                  | _
                   -> create_fresh_local_name ()
                in
                (env, Mangled.from_string typ_class_name)
            | []
             -> (env, Mangled.from_string (create_fresh_local_name ()))
          in
          (* add the instructions *& local = [allocated_obj_exp]; id = *& local, where local and id are
           * both fresh. the only point of this is to add a descriptive local name that makes error
           * reports from the harness look nicer -- it's not necessary to make symbolic execution work *)
          let fresh_local_exp =
            Exp.Lvar (Pvar.mk typ_class_name (Typ.Procname.Java env.harness_name))
          in
          let write_to_local_instr = Sil.Store (fresh_local_exp, typ, allocated_obj_exp, env.pc) in
          let env' = env_add_instr write_to_local_instr env in
          let fresh_id = Ident.create_fresh Ident.knormal in
          let read_from_local_instr = Sil.Load (fresh_id, fresh_local_exp, typ, env'.pc) in
          (Exp.Var fresh_id, env_add_instr read_from_local_instr env')
      | Typ.Tint _
       -> (Exp.Const (Const.Cint IntLit.zero), env)
      | Typ.Tfloat _
       -> (Exp.Const (Const.Cfloat 0.0), env)
      | _
       -> L.internal_error "Couldn't inhabit typ: %a@." (Typ.pp Pp.text) typ ;
          assert false
    in
    let inhabited_exp, env' =
      inhabit_internal typ {env with cur_inhabiting= TypSet.add typ env.cur_inhabiting}
    in
    ( inhabited_exp
    , {env' with cache= TypMap.add typ inhabited_exp env.cache; cur_inhabiting= env.cur_inhabiting}
    )

(** inhabit each of the types in the formals list *)
and inhabit_args tenv formals cfg env =
  let inhabit_arg (_, formal_typ) (args, env) =
    let exp, env = inhabit_typ tenv formal_typ cfg env in
    ((exp, formal_typ) :: args, env)
  in
  List.fold_right ~f:inhabit_arg formals ~init:([], env)

(** create Sil that calls the constructor in constr_name on allocated_obj and inhabits the
 * remaining arguments *)
and inhabit_constructor tenv constr_name (allocated_obj, obj_type) cfg env =
  try
    (* this lookup can fail when we try to get the procdesc of a procedure from a different
     * module. this could be solved with a whole - program class hierarchy analysis *)
    let args, env =
      let non_receiver_formals = tl_or_empty (formals_from_name cfg constr_name) in
      inhabit_args tenv non_receiver_formals cfg env
    in
    let constr_instr =
      let fun_exp = fun_exp_from_name constr_name in
      Sil.Call (None, fun_exp, (allocated_obj, obj_type) :: args, env.pc, CallFlags.default)
    in
    env_add_instr constr_instr env
  with Not_found -> env

let inhabit_call_with_args procname procdesc args env =
  let retval =
    let ret_typ = Procdesc.get_ret_type procdesc in
    let is_void = Typ.equal_desc ret_typ.Typ.desc Typ.Tvoid in
    if is_void then None else Some (Ident.create_fresh Ident.knormal, ret_typ)
  in
  let call_instr =
    let fun_exp = fun_exp_from_name procname in
    let flags =
      {CallFlags.default with CallFlags.cf_virtual= not (Typ.Procname.java_is_static procname)}
    in
    Sil.Call (retval, fun_exp, args, env.pc, flags)
  in
  env_add_instr call_instr env

(** create Sil that inhabits args to and calls proc_name *)
let inhabit_call tenv (procname, receiver) cfg env =
  try
    match procdesc_from_name cfg procname with
    | Some procdesc
     -> (* swap the type of the 'this' formal with the receiver type, if there is one *)
        let formals =
          match (Procdesc.get_formals procdesc, receiver) with
          | (name, _) :: formals, Some receiver
           -> (name, receiver) :: formals
          | formals, None
           -> formals
          | [], Some _
           -> failwithf "Expected at least one formal to bind receiver to in method %a"
                Typ.Procname.pp procname
        in
        let args, env = inhabit_args tenv formals cfg env in
        inhabit_call_with_args procname procdesc args env
    | None
     -> env
  with Not_found -> env

(** create a dummy file for the harness and associate them in the exe_env *)
let create_dummy_harness_filename harness_name =
  let dummy_file_dir = Filename.temp_dir_name in
  let file_str =
    Typ.Procname.java_get_class_name harness_name ^ "_" ^ Typ.Procname.java_get_method harness_name
    ^ ".java"
  in
  Filename.concat dummy_file_dir file_str

(* TODO (t3040429): fill this file up with Java-like code that matches the SIL *)

(** write the SIL for the harness to a file *)
let write_harness_to_file harness_instrs harness_file_name =
  let harness_file = Utils.create_outfile harness_file_name in
  let pp_harness fmt =
    List.iter
      ~f:(fun instr -> Format.fprintf fmt "%a@\n" (Sil.pp_instr Pp.text) instr)
      harness_instrs
  in
  Utils.do_outf harness_file (fun outf -> pp_harness outf.fmt ; Utils.close_outf outf)

(** add the harness proc to the cg and make sure its callees can be looked up by sym execution *)
let add_harness_to_cg harness_name harness_node cg =
  Cg.add_defined_node cg (Typ.Procname.Java harness_name) ;
  List.iter
    ~f:(fun p -> Cg.add_edge cg (Typ.Procname.Java harness_name) p)
    (Procdesc.Node.get_callees harness_node)

(** create and fill the appropriate nodes and add them to the harness cfg. also add the harness
 * proc to the cg *)
let setup_harness_cfg harness_name env cg cfg =
  (* each procedure has different scope: start names from id 0 *)
  Ident.NameGenerator.reset () ;
  let procname = Typ.Procname.Java harness_name in
  let proc_attributes =
    { (ProcAttributes.default procname Config.Java) with
      ProcAttributes.is_defined= true; loc= env.pc }
  in
  let procdesc = Cfg.create_proc_desc cfg proc_attributes in
  let harness_node =
    (* important to reverse the list or there will be scoping issues! *)
    let instrs = List.rev env.instrs in
    let nodekind = Procdesc.Node.Stmt_node "method_body" in
    Procdesc.create_node procdesc env.pc nodekind instrs
  in
  let start_node, exit_node =
    let create_node kind = Procdesc.create_node procdesc env.pc kind [] in
    let start_kind = Procdesc.Node.Start_node procname in
    let exit_kind = Procdesc.Node.Exit_node procname in
    (create_node start_kind, create_node exit_kind)
  in
  Procdesc.set_start_node procdesc start_node ;
  Procdesc.set_exit_node procdesc exit_node ;
  Procdesc.Node.add_locals_ret_declaration start_node proc_attributes [] ;
  Procdesc.node_set_succs_exn procdesc start_node [harness_node] [exit_node] ;
  Procdesc.node_set_succs_exn procdesc harness_node [exit_node] [exit_node] ;
  add_harness_to_cg harness_name harness_node cg

(** create a procedure named harness_name that calls each of the methods in trace in the specified
 * order with the specified receiver and add it to the execution environment *)
let inhabit_trace tenv trace harness_name cg cfg =
  if List.length trace > 0 then
    let source_file = Cg.get_source cg in
    let harness_filename = create_dummy_harness_filename harness_name in
    let start_line = 1 in
    let empty_env =
      let pc = {Location.line= start_line; col= 1; file= source_file} in
      {instrs= []; cache= TypMap.empty; pc; cur_inhabiting= TypSet.empty; harness_name}
    in
    (* invoke lifecycle methods *)
    let env'' =
      List.fold ~f:(fun env to_call -> inhabit_call tenv to_call cfg env) ~init:empty_env trace
    in
    try
      setup_harness_cfg harness_name env'' cg cfg ;
      write_harness_to_file (List.rev env''.instrs) harness_filename
    with Not_found -> ()
