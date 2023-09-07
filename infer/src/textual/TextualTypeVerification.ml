(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
open Textual

module TypeNameBridge = struct
  include TypeName

  (** the name of the Textual string type *)
  let sil_string = {value= "String"; loc= Unknown}

  (** the name of the Textual type of all types (do not ask what it the type of this type itself
      please...) *)
  let sil_type_of_types = {value= "TYPE"; loc= Unknown}
end

(** is it safe to assign a value of type [given] to a variable of type [assigned] *)
let rec compat ~assigned:(t1 : Typ.t) ~given:(t2 : Typ.t) =
  match (t1, t2) with
  | Int, Int ->
      true
  | Float, Float ->
      true
  | Void, _ ->
      true
  | _, Void ->
      true
  | Int, Ptr _ ->
      true
  | Ptr t1, Ptr t2 ->
      compat ~assigned:t1 ~given:t2
  | Ptr _, Null ->
      true
  | Struct _, Struct _ ->
      true (* no subtyping check yet *)
  | Array t1, Array t2 ->
      compat ~assigned:t1 ~given:t2
  | _, _ ->
      false


let is_ptr = function Typ.Ptr _ -> true | _ -> false

let is_ptr_struct = function Typ.Ptr (Struct _) | Typ.Void -> true | _ -> false

let is_int = function Typ.Int -> true | _ -> false

let sub_int = function ty -> compat ~assigned:Int ~given:ty

(** for type errors *)
type expected_kind = Ptr | PtrArray | PtrStruct | Typ | SubTypeOf of Typ.t | SuperTypeOf of Typ.t

let pp_expected_kind fmt expected =
  match expected with
  | Ptr ->
      F.fprintf fmt "a pointer type"
  | PtrArray ->
      F.fprintf fmt "a pointer of array type"
  | PtrStruct ->
      F.fprintf fmt "a pointer of struct type"
  | Typ ->
      F.fprintf fmt "the type of all types"
  | SubTypeOf typ ->
      F.fprintf fmt "a subtype of %a" Typ.pp typ
  | SuperTypeOf typ ->
      F.fprintf fmt "a supertype of %a" Typ.pp typ


type error =
  | TypeMismatch of {exp: Exp.t; typ: Typ.t; expected: expected_kind; loc: Location.t}
  | WrongNumberBuiltinArgs of
      { proc: qualified_procname
      ; expected: int
      ; at_least: bool (* is the number of expected arguments a lower bound *)
      ; given: int
      ; loc: Location.t }
  | IdentAssignedTwice of {id: Ident.t; typ1: Typ.t; typ2: Typ.t; loc1: Location.t; loc2: Location.t}
  | IdentReadBeforeWrite of {id: Ident.t; loc: Location.t}
  | VarTypeNotDeclared of {var: VarName.t; loc: Location.t}
  | MissingDeclaration of {proc: qualified_procname; loc: Location.t}
  | ArityMismatch of {length1: int; length2: int; loc: Location.t}

let error_loc = function
  | TypeMismatch {loc; _} ->
      loc
  | WrongNumberBuiltinArgs {loc; _} ->
      loc
  | IdentAssignedTwice {loc1; _} ->
      loc1
  | IdentReadBeforeWrite {loc; _} ->
      loc
  | VarTypeNotDeclared {loc; _} ->
      loc
  | MissingDeclaration {loc} ->
      loc
  | ArityMismatch {loc} ->
      loc


let pp_error sourcefile fmt error =
  let primary_error_loc = error_loc error in
  F.fprintf fmt "%a, %a: textual type error: " SourceFile.pp sourcefile Location.pp
    primary_error_loc ;
  match error with
  | TypeMismatch {exp; typ; expected; _} ->
      F.fprintf fmt "expression %a has type %a, while %a was expected" Exp.pp exp Typ.pp typ
        pp_expected_kind expected
  | WrongNumberBuiltinArgs {proc; expected; at_least; given; _} ->
      F.fprintf fmt "builtin %a is called with %d arguments while it expects %s%d"
        pp_qualified_procname proc given
        (if at_least then "at least " else "")
        expected
  | IdentAssignedTwice {id; typ1; typ2; loc2; _} ->
      F.fprintf fmt "ident %a is given the type %a, but it has already been given the type %a at %a"
        Ident.pp id Typ.pp typ1 Typ.pp typ2 Location.pp_line loc2
  | IdentReadBeforeWrite {id; _} ->
      F.fprintf fmt "ident %a is read before being written" Ident.pp id
  | VarTypeNotDeclared {var; _} ->
      F.fprintf fmt "variable %a has not been declared" VarName.pp var
  | MissingDeclaration {proc} ->
      F.fprintf fmt "procname %a should be user-declared or a builtin" pp_qualified_procname proc
  | ArityMismatch {length1; length2; loc} ->
      F.fprintf fmt "iter2 was run on lists of different lengths at %a: %d vs %d" Location.pp loc
        length1 length2


let rec loc_of_exp exp =
  match (exp : Exp.t) with
  | Var _ ->
      None
  | Lvar {loc} ->
      Some loc
  | Field {exp} ->
      loc_of_exp exp
  | Index (exp, _) ->
      loc_of_exp exp
  | Const _ ->
      None
  | Call {proc} ->
      Some proc.name.loc
  | Typ _ ->
      None


let mk_type_mismatch_error expected loc exp typ : error =
  let loc = loc_of_exp exp |> Option.value ~default:loc in
  TypeMismatch {exp; typ; expected; loc}


let mk_missing_declaration_error (proc : qualified_procname) : error =
  let name = qualified_procname_name proc in
  let {ProcName.loc} = name in
  MissingDeclaration {proc; loc}


(** state + error monad *)
type state =
  { decls: TextualDecls.t  (** read only *)
  ; pdesc: ProcDesc.t  (** read only *)
  ; loc: Location.t  (** the current location of the instruction being type checked *)
  ; idents: (Typ.t * Location.t) Ident.Map.t
        (** the type of each ident seen so far, together with the location were the ident was
            assigned *)
  ; vars: Typ.t VarName.Map.t  (** the type of each variable seen so far *)
  ; nodes_from_label: Node.t NodeName.Map.t
  ; seen_nodes: NodeName.Set.t  (** seen set of nodes during the dfs *)
  ; dfs_stack: NodeName.t List.t
  ; errors: error list
        (** we accumulate as many type errors as possible instead of stopping at the first seen *)
  }

type 'a result = Value of 'a | Abort

type 'a monad = state -> 'a result * state

let abort : 'a monad = fun state -> (Abort, state)

let ret (a : 'a) : 'a monad = fun state -> (Value a, state)

let bind (x : 'a monad) (f : 'a -> 'b monad) : 'b monad =
 fun state -> match x state with Value a, state -> f a state | Abort, state -> (Abort, state)


let ( let* ) a f = bind a f

let catch (x : 'a monad) : unit monad =
 fun state ->
  let _, state = x state in
  (Value (), state)


let option_value_map (o : 'a option) ~(none : 'b monad) ~(some : 'a -> 'b monad) : 'b monad =
 fun state -> Option.value_map o ~default:(none state) ~f:(fun a -> some a state)


(** state accessors *)

(** add an error and continue normally *)
let add_error e : unit monad = fun state -> (Value (), {state with errors= e :: state.errors})

let get_location : Location.t monad = fun state -> (Value state.loc, state)

let set_location loc : unit monad = fun state -> (Value (), {state with loc})

let get_result_type : Typ.t monad = fun state -> (Value state.pdesc.procdecl.result_type.typ, state)

let get_lang : Lang.t option monad = fun state -> (Value (TextualDecls.lang state.decls), state)

let iter (l : 'a list) ~(f : 'a -> unit monad) : unit monad =
 fun state -> ret () (List.fold l ~init:state ~f:(fun state a -> snd (f a state)))


let iter2 ?(strict = true) loc (l1 : 'a list) (l2 : 'b list) ~(f : 'a -> 'b -> unit monad) :
    unit monad =
 fun state ->
  match List.fold2 l1 l2 ~init:state ~f:(fun state a b -> snd (f a b state)) with
  | Ok state ->
      ret () state
  | Unequal_lengths when not strict ->
      ret () state
  | Unequal_lengths ->
      let length1 = List.length l1 in
      let length2 = List.length l2 in
      let err = add_error (ArityMismatch {length1; length2; loc}) in
      bind err (fun () -> abort) state


let typeof_ident id : (Typ.t * Location.t) monad =
 fun state ->
  let optional_typ_and_loc = Ident.Map.find_opt id state.idents in
  option_value_map optional_typ_and_loc state
    ~none:
      (let* loc = get_location in
       let* () = add_error (IdentReadBeforeWrite {id; loc}) in
       abort )
    ~some:ret


let typeof_var var : Typ.t monad =
 fun state ->
  let optional_typ = VarName.Map.find_opt var state.vars in
  option_value_map optional_typ state
    ~none:
      (let loc = var.VarName.loc in
       let* () = add_error (VarTypeNotDeclared {var; loc}) in
       abort )
    ~some:ret


let typeof_field field : Typ.t monad =
 fun state ->
  match TextualDecls.get_fielddecl state.decls field with
  | None when TypeName.equal field.enclosing_class TypeName.wildcard ->
      ret Typ.Void state
  | None ->
      (* such an error should have been caught in TextualVerification *)
      L.die InternalError "Textual type verification: field %a is unknown" pp_qualified_fieldname
        field
  | Some f ->
      ret f.typ state


let set_ident_type id typ : unit monad =
 fun state ->
  let otyp = Ident.Map.find_opt id state.idents in
  match otyp with
  | None ->
      (Value (), {state with idents= Ident.Map.add id (typ, state.loc) state.idents})
  | Some (typ0, _) when Typ.equal typ0 typ ->
      (Value (), state)
  | Some (typ0, loc0) ->
      let loc1 = state.loc in
      ( Abort
      , { state with
          errors= IdentAssignedTwice {id; typ1= typ; typ2= typ0; loc1; loc2= loc0} :: state.errors
        } )


let dfs_push (label : NodeName.t) : unit monad =
 fun state -> (Value (), {state with dfs_stack= label :: state.dfs_stack})


let dfs_pop : NodeName.t option monad =
 fun state ->
  match state.dfs_stack with
  | [] ->
      (Value None, state)
  | label :: stack ->
      ret (Some label) {state with dfs_stack= stack}


let mark_node_as_seen (node : Node.t) : unit monad =
 fun state -> (Value (), {state with seen_nodes= NodeName.Set.add node.label state.seen_nodes})


let has_node_been_seen (node : Node.t) : bool monad =
 fun state -> (Value (NodeName.Set.mem node.label state.seen_nodes), state)


let get_node (label : NodeName.t) : Node.t monad =
 (* should never fail because labels have been verified in TextualVerification *)
 fun state -> (Value (NodeName.Map.find label state.nodes_from_label), state)


let typeof_const (const : Const.t) : Typ.t =
  match const with
  | Int _ ->
      Int
  | Null ->
      Null
  | Str _ ->
      Ptr (Struct TypeNameBridge.sil_string)
  | Float _ ->
      Float


let typeof_reserved_proc (proc : qualified_procname) : (Typ.t * Typ.t list option) monad =
  if ProcDecl.to_binop proc |> Option.is_some then ret (Typ.Int, Some [Typ.Int; Typ.Int])
  else if ProcDecl.to_unop proc |> Option.is_some then ret (Typ.Int, Some [Typ.Int])
  else
    let* () = add_error (mk_missing_declaration_error proc) in
    abort


(* Since procname can be both defined and declared in a file we should account for unknown formals in declarations. *)
let typeof_procname (procsig : ProcSig.t) : (Typ.t * Typ.t list option) monad =
 fun state ->
  match TextualDecls.get_procdecl state.decls procsig with
  | Some (procdecl : ProcDecl.t) ->
      let formals_types =
        procdecl.formals_types
        |> Option.map ~f:(fun formals_types -> List.map formals_types ~f:(fun {Typ.typ} -> typ))
      in
      ret (procdecl.result_type.typ, formals_types) state
  | None when ProcSig.to_qualified_procname procsig |> qualified_procname_contains_wildcard ->
      ret (Typ.Void, None) state
  | None ->
      (typeof_reserved_proc (ProcSig.to_qualified_procname procsig)) state


let rec typecheck_exp exp ~check ~expected ~loc : unit monad =
  catch
    (let* typ = typeof_exp exp in
     if check typ then ret () else add_error (mk_type_mismatch_error expected loc exp typ) )


and get_typeof_array_content exp : Typ.t monad =
  let* typ = typeof_exp exp in
  match (typ : Typ.t) with
  | Ptr (Array content_typ) ->
      Typ.Ptr content_typ |> ret
  | _ ->
      let* loc = get_location in
      let* () = add_error (mk_type_mismatch_error PtrArray loc exp typ) in
      abort


and get_typeof_ptr_content exp : Typ.t monad =
  let* typ = typeof_exp exp in
  match (typ : Typ.t) with Ptr typ -> ret typ | _ -> abort


and typeof_exp (exp : Exp.t) : Typ.t monad =
  match exp with
  | Var id ->
      let* typ, _ = typeof_ident id in
      ret typ
  | Lvar varname ->
      let* typ = typeof_var varname in
      Typ.Ptr typ |> ret
  | Field {exp; field} ->
      let* loc = get_location in
      let* () = typecheck_exp exp ~check:is_ptr_struct ~expected:PtrStruct ~loc in
      (* remark: we could check if field is declared in the type of exp, but this may be too
         strong for some weakly typed frontend langages *)
      let* field_typ = typeof_field field in
      Typ.Ptr field_typ |> ret
  | Index (exp1, exp2) ->
      let* loc = get_location in
      let* () = typecheck_exp exp2 ~check:is_int ~expected:(SubTypeOf Int) ~loc in
      get_typeof_array_content exp1
  | Const const ->
      typeof_const const |> ret
  | Call {proc; args} when ProcDecl.is_allocate_object_builtin proc ->
      typeof_allocate_builtin proc args
  | Call {proc; args} when ProcDecl.is_allocate_array_builtin proc ->
      typeof_allocate_array_builtin proc args
  | Call {proc; args} when ProcDecl.is_cast_builtin proc ->
      typeof_cast_builtin proc args
  | Call {proc; args} when ProcDecl.is_instanceof_builtin proc ->
      typeof_instanceof_builtin proc args
  | Call {proc; args} when ProcDecl.is_lazy_class_initialize_builtin proc ->
      typeof_allocate_builtin proc args
  | Call {proc; args} ->
      let* lang = get_lang in
      let procsig = Exp.call_sig proc args lang in
      let* result_type, formals_types = typeof_procname procsig in
      let* loc = get_location in
      let* () =
        match formals_types with
        | None ->
            ret ()
        | Some formals_types ->
            iter2 ~strict:true loc args formals_types ~f:(fun exp assigned ->
                typecheck_exp exp
                  ~check:(fun given -> compat ~assigned ~given)
                  ~expected:(SubTypeOf assigned) ~loc )
      in
      ret result_type
  | Typ _ ->
      ret (Typ.Struct TypeNameBridge.sil_type_of_types)


and typeof_allocate_builtin (proc : qualified_procname) args =
  match args with
  | [Exp.Typ typ] ->
      Typ.Ptr typ |> ret
  | [exp] ->
      let* loc = get_location in
      let* typ = typeof_exp exp in
      let* () = add_error (mk_type_mismatch_error Typ loc exp typ) in
      abort
  | _ ->
      let* loc = get_location in
      let* () =
        add_error
          (WrongNumberBuiltinArgs {proc; expected= 1; given= List.length args; at_least= false; loc})
      in
      abort


and typeof_allocate_array_builtin (proc : qualified_procname) args =
  match args with
  | Exp.Typ typ :: dim :: dims ->
      let* loc = get_location in
      let* () =
        iter (dim :: dims) ~f:(fun exp ->
            typecheck_exp exp ~check:is_int ~expected:(SubTypeOf Int) ~loc )
      in
      Typ.Ptr typ |> ret
  | exp1 :: exp2 :: _ ->
      let* loc = get_location in
      let* typ = typeof_exp exp1 in
      let* () = add_error (mk_type_mismatch_error Typ loc exp1 typ) in
      let* () = typecheck_exp exp2 ~check:is_int ~expected:(SubTypeOf Int) ~loc in
      abort
  | _ ->
      let* loc = get_location in
      let* () =
        add_error
          (WrongNumberBuiltinArgs {proc; expected= 2; given= List.length args; at_least= true; loc})
      in
      abort


and typeof_cast_builtin (proc : qualified_procname) args =
  match args with
  | [Exp.Typ typ; exp] ->
      let* _old_typ = typeof_exp exp in
      ret typ
  | [exp; _] ->
      let* loc = get_location in
      let* typ = typeof_exp exp in
      let* () = add_error (mk_type_mismatch_error Typ loc exp typ) in
      abort
  | _ ->
      let* loc = get_location in
      let* () =
        add_error
          (WrongNumberBuiltinArgs {proc; expected= 2; given= List.length args; at_least= false; loc})
      in
      abort


and typeof_instanceof_builtin (proc : qualified_procname) args =
  match args with
  | [exp; Exp.Typ _] ->
      let* _ = typeof_exp exp in
      ret Typ.Int
  | [_; exp] ->
      let* loc = get_location in
      let* typ = typeof_exp exp in
      let* () = add_error (mk_type_mismatch_error Typ loc exp typ) in
      abort
  | _ ->
      let* loc = get_location in
      let* () =
        add_error
          (WrongNumberBuiltinArgs {proc; expected= 2; given= List.length args; at_least= false; loc})
      in
      abort


let typecheck_instr (instr : Instr.t) : unit monad =
  match instr with
  | Load {id; exp; typ; loc} ->
      let* () = set_location loc in
      option_value_map typ
        ~some:(fun typ ->
          let* () =
            typecheck_exp exp
              ~check:(fun given -> compat ~assigned:(Ptr typ) ~given)
              ~expected:(SubTypeOf (Ptr typ)) ~loc
          in
          set_ident_type id typ )
        ~none:
          (let* typ = get_typeof_ptr_content exp in
           set_ident_type id typ )
  | Store {exp1; typ; exp2; loc} ->
      let* () = set_location loc in
      option_value_map typ
        ~some:(fun typ ->
          let* () =
            typecheck_exp exp2
              ~check:(fun given -> compat ~assigned:typ ~given)
              ~expected:(SubTypeOf typ) ~loc
          in
          typecheck_exp exp1
            ~check:(fun assigned -> compat ~assigned ~given:(Ptr typ))
            ~expected:(SuperTypeOf (Ptr typ)) ~loc )
        ~none:
          (let* typ = typeof_exp exp2 in
           typecheck_exp exp1
             ~check:(fun assigned -> compat ~assigned ~given:(Ptr typ))
             ~expected:(SuperTypeOf (Ptr typ)) ~loc )
  | Prune {exp; loc} ->
      let* () = set_location loc in
      typecheck_exp exp ~check:sub_int ~expected:(SubTypeOf Int) ~loc
  | Let {id; exp; loc} ->
      let* () = set_location loc in
      let* typ = typeof_exp exp in
      set_ident_type id typ


let typecheck_node_call loc ({label; ssa_args} : Terminator.node_call) : unit monad =
  let* node = get_node label in
  iter2 loc ssa_args node.Node.ssa_parameters ~f:(fun exp (_, assigned) ->
      typecheck_exp exp
        ~check:(fun given -> compat ~assigned ~given)
        ~expected:(SubTypeOf assigned) ~loc )


let rec typecheck_terminator loc (term : Terminator.t) : unit monad =
  let* () = set_location loc in
  match term with
  | If {bexp= _; then_; else_} ->
      let* () = typecheck_terminator loc then_ in
      let* () = typecheck_terminator loc else_ in
      ret ()
  | Ret exp ->
      let* result_typ = get_result_type in
      typecheck_exp exp
        ~check:(fun given -> compat ~assigned:result_typ ~given)
        ~expected:(SubTypeOf result_typ) ~loc
  | Jump node_calls ->
      iter node_calls ~f:(typecheck_node_call loc)
  | Throw exp ->
      typecheck_exp exp ~check:is_ptr ~expected:Ptr ~loc
  | Unreachable ->
      ret ()


let all_successors (node : Node.t) : NodeName.t list =
  let node_call_succ ({label} : Terminator.node_call) = label in
  let rec normal_succs (terminator : Terminator.t) =
    match terminator with
    | If {then_; else_} ->
        normal_succs then_ @ normal_succs else_
    | Ret _ | Throw _ | Unreachable ->
        []
    | Jump node_calls ->
        List.map ~f:node_call_succ node_calls
  in
  normal_succs node.last @ node.exn_succs


(** nodes are typechecked in dfs order to make sure every idents that are read in a node, have been
    assigned a type in a dfs predecessor *)
let rec typecheck_node (node : Node.t) : unit monad =
  let* seen = has_node_been_seen node in
  if seen then visit_next ()
  else
    let* () = mark_node_as_seen node in
    let* () = iter node.ssa_parameters ~f:(fun (id, typ) -> set_ident_type id typ) in
    let* () = iter node.instrs ~f:typecheck_instr in
    let* () = typecheck_terminator node.last_loc node.last in
    let* () = all_successors node |> iter ~f:dfs_push in
    visit_next ()


and visit_next () : unit monad =
  let* next = dfs_pop in
  match next with
  | None ->
      ret ()
  | Some next ->
      let* next_node = get_node next in
      typecheck_node next_node


let typecheck_procdesc decls globals_types (pdesc : ProcDesc.t) errors : error list =
  let vars_with_params =
    match
      List.fold2 pdesc.params (ProcDesc.formals pdesc)
        ~f:(fun map vname {Typ.typ} -> VarName.Map.add vname typ map)
        ~init:globals_types
    with
    | Ok map ->
        map
    | Unequal_lengths ->
        L.die InternalError
          "Function %a should have the same number of parameter names and parameter types"
          ProcDecl.pp pdesc.procdecl
  in
  let vars_with_locals =
    List.fold pdesc.locals ~init:vars_with_params ~f:(fun map (vname, {Typ.typ}) ->
        VarName.Map.add vname typ map )
  in
  let init : state =
    { decls
    ; pdesc
    ; loc= Location.Unknown
    ; seen_nodes= NodeName.Set.empty
    ; nodes_from_label=
        List.fold pdesc.nodes ~init:NodeName.Map.empty ~f:(fun map (node : Node.t) ->
            NodeName.Map.add node.label node map )
    ; idents= Ident.Map.empty
    ; vars= vars_with_locals
    ; dfs_stack= [pdesc.start]
    ; errors }
  in
  let _, {errors} = visit_next () init in
  errors


let run (module_ : Module.t) decls_env : error list =
  let globals_type =
    TextualDecls.fold_globals decls_env ~init:VarName.Map.empty ~f:(fun map varname global ->
        VarName.Map.add varname global.typ map )
  in
  let errors =
    List.fold module_.decls ~init:[] ~f:(fun errors (decl : Module.decl) ->
        match decl with
        | Global _ | Struct _ | Procdecl _ ->
            errors
        | Proc pdesc ->
            typecheck_procdesc decls_env globals_type pdesc errors )
  in
  List.rev errors


type type_check_result = Ok | Decl_errors of TextualDecls.error list | Type_errors of error list

let type_check module_ =
  let decls_errors, decls_env = TextualDecls.make_decls module_ in
  if not (List.is_empty decls_errors) then Decl_errors decls_errors
  else
    let errors = run module_ decls_env in
    if List.is_empty errors then Ok else Type_errors errors
