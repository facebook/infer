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
      { proc: QualifiedProcName.t
      ; expected: int
      ; at_least: bool (* is the number of expected arguments a lower bound *)
      ; given: int
      ; loc: Location.t }
  | IdentAssignedTwice of {id: Ident.t; typ1: Typ.t; typ2: Typ.t; loc1: Location.t; loc2: Location.t}
  | IdentReadBeforeWrite of {id: Ident.t; loc: Location.t}
  | VarTypeNotDeclared of {var: VarName.t; loc: Location.t}
  | MissingDeclaration of {proc: QualifiedProcName.t; loc: Location.t}
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
        QualifiedProcName.pp proc given
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
      F.fprintf fmt "procname %a should be user-declared or a builtin" QualifiedProcName.pp proc
  | ArityMismatch {length1; length2; loc} ->
      F.fprintf fmt "iter2 was run on lists of different lengths at %a: %d vs %d" Location.pp loc
        length1 length2


let rec loc_of_exp exp =
  match (exp : Exp.t) with
  | Var _ ->
      None
  | Lvar {loc} ->
      Some loc
  | Load {exp} | Field {exp} ->
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


let mk_missing_declaration_error (proc : QualifiedProcName.t) : error =
  let name = QualifiedProcName.name proc in
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
  ; typechecked_nodes: Node.t NodeName.Map.t
        (** to keep track of nodes that have been typecked during the typechecking dfs, together
            with the updated node (with all type holes filled) *)
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

let map (x : 'a monad) (f : 'a -> 'b) : 'b monad =
  let* a = x in
  ret (f a)


let ( let+ ) a f = map a f

let catch ~(with_ : 'a) (x : 'a monad) : 'a monad =
 fun state -> match x state with Abort, state -> (Value with_, state) | res -> res


let option_value_map (o : 'a option) ~(none : 'b monad) ~(some : 'a -> 'b monad) : 'b monad =
 fun state -> Option.value_map o ~default:(none state) ~f:(fun a -> some a state)


(** state accessors *)

(** add an error and continue normally *)
let add_error e : unit monad = fun state -> (Value (), {state with errors= e :: state.errors})

let get_location : Location.t monad = fun state -> (Value state.loc, state)

let set_location loc : unit monad = fun state -> (Value (), {state with loc})

let get_result_type : Typ.t monad = fun state -> (Value state.pdesc.procdecl.result_type.typ, state)

let get_lang : Lang.t option monad = fun state -> (Value (TextualDecls.lang state.decls), state)

let fold (l : 'a list) ~(init : 'acc monad) ~(f : 'acc -> 'a -> 'acc monad) : 'acc monad =
  List.fold l ~init ~f:(fun monad a ->
      let* acc = monad in
      f acc a )


let iter (l : 'a list) ~(f : 'a -> unit monad) : unit monad =
  fold l ~init:(ret ()) ~f:(fun () a -> f a)


let mapM (l : 'a list) ~(f : 'a -> 'b monad) : 'b list monad =
  let+ rev_res =
    fold l ~init:(ret []) ~f:(fun l a ->
        let+ b = f a in
        b :: l )
  in
  List.rev rev_res


let fold2 loc (l1 : 'a list) (l2 : 'b list) ~(init : 'acc monad)
    ~(f : 'acc -> 'a -> 'b -> 'acc monad) : 'acc monad =
  match
    List.fold2 l1 l2 ~init ~f:(fun monad a b ->
        let* acc = monad in
        f acc a b )
  with
  | Ok res ->
      res
  | Unequal_lengths ->
      let length1 = List.length l1 in
      let length2 = List.length l2 in
      let* () = add_error (ArityMismatch {length1; length2; loc}) in
      abort


let mapM2 loc (l1 : 'a list) (l2 : 'b list) ~(f : 'a -> 'b -> 'c monad) : 'c list monad =
  let+ rev_res =
    fold2 loc l1 l2 ~init:(ret []) ~f:(fun l a b ->
        let+ c = f a b in
        c :: l )
  in
  List.rev rev_res


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


let mark_node_as_typechecked (node : Node.t) : unit monad =
 fun state ->
  ( Value ()
  , {state with typechecked_nodes= NodeName.Map.add node.label node state.typechecked_nodes} )


let has_node_been_typechecked (node : Node.t) : bool monad =
 fun state -> (Value (NodeName.Map.mem node.label state.typechecked_nodes), state)


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


let typeof_reserved_proc (proc : QualifiedProcName.t) : (Typ.t * Typ.t list option) monad =
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
  | None when ProcSig.to_qualified_procname procsig |> QualifiedProcName.contains_wildcard ->
      ret (Typ.Void, None) state
  | None ->
      (typeof_reserved_proc (ProcSig.to_qualified_procname procsig)) state


(* In all the typecheck/typeof function below, when typechecking/type-computation succeeds
   we return a copy of the argument but with all type holes filled *)
let rec typecheck_exp exp ~check ~expected ~loc : Exp.t monad =
  catch
    (let* exp, typ = typeof_exp exp in
     if check typ then ret exp
     else
       let+ () = add_error (mk_type_mismatch_error expected loc exp typ) in
       exp )
    ~with_:exp


and get_typeof_array_content exp : (Exp.t * Typ.t) monad =
  let* exp, typ = typeof_exp exp in
  match (typ : Typ.t) with
  | Ptr (Array content_typ) ->
      ret (exp, Typ.Ptr content_typ)
  | _ ->
      let* loc = get_location in
      let* () = add_error (mk_type_mismatch_error PtrArray loc exp typ) in
      abort


and get_typeof_ptr_content exp : (Exp.t * Typ.t) monad =
  let* exp, typ = typeof_exp exp in
  match (typ : Typ.t) with Ptr typ -> ret (exp, typ) | _ -> abort


and typeof_exp (exp : Exp.t) : (Exp.t * Typ.t) monad =
  match exp with
  | Var id ->
      let+ typ, _ = typeof_ident id in
      (exp, typ)
  | Load {exp; typ} ->
      let* loc = get_location in
      option_value_map typ
        ~some:(fun typ ->
          let+ exp =
            typecheck_exp exp
              ~check:(fun given -> compat ~assigned:(Ptr typ) ~given)
              ~expected:(SubTypeOf (Ptr typ)) ~loc
          in
          (Exp.Load {exp; typ= Some typ}, typ) )
        ~none:
          (let+ exp, typ = get_typeof_ptr_content exp in
           (Exp.Load {exp; typ= Some typ}, typ) )
  | Lvar varname ->
      let+ typ = typeof_var varname in
      (exp, Typ.Ptr typ)
  | Field {exp; field} ->
      let* loc = get_location in
      let* exp = typecheck_exp exp ~check:is_ptr_struct ~expected:PtrStruct ~loc in
      (* remark: we could check if field is declared in the type of exp, but this may be too
         strong for some weakly typed frontend langages *)
      let+ field_typ = typeof_field field in
      (Exp.Field {exp; field}, Typ.Ptr field_typ)
  | Index (exp1, exp2) ->
      let* loc = get_location in
      let* exp2 = typecheck_exp exp2 ~check:is_int ~expected:(SubTypeOf Int) ~loc in
      let+ exp1, typ = get_typeof_array_content exp1 in
      (Exp.Index (exp1, exp2), typ)
  | Const const ->
      ret (exp, typeof_const const)
  | Call {proc; args}
    when ProcDecl.is_allocate_object_builtin proc
         || ProcDecl.is_lazy_class_initialize_builtin proc
         || ProcDecl.is_get_lazy_class_builtin proc ->
      typeof_allocate_builtin proc args
  | Call {proc; args} when ProcDecl.is_allocate_array_builtin proc ->
      typeof_allocate_array_builtin proc args
  | Call {proc; args} when ProcDecl.is_cast_builtin proc ->
      typeof_cast_builtin proc args
  | Call {proc; args} when ProcDecl.is_instanceof_builtin proc ->
      typeof_instanceof_builtin proc args
  | Call {proc; args; kind} ->
      let* lang = get_lang in
      let procsig = Exp.call_sig proc args lang in
      let* result_type, formals_types = typeof_procname procsig in
      let* loc = get_location in
      let+ args =
        match formals_types with
        | None ->
            ret args
        | Some formals_types ->
            mapM2 loc args formals_types ~f:(fun exp assigned ->
                let+ exp =
                  typecheck_exp exp
                    ~check:(fun given -> compat ~assigned ~given)
                    ~expected:(SubTypeOf assigned) ~loc
                in
                exp )
      in
      (Exp.Call {proc; args; kind}, result_type)
  | Typ _ ->
      ret (exp, Typ.Struct TypeNameBridge.sil_type_of_types)


and typeof_allocate_builtin (proc : QualifiedProcName.t) args =
  match args with
  | [Exp.Typ typ] ->
      ret (Exp.Call {proc; args; kind= Exp.NonVirtual}, Typ.Ptr typ)
  | [exp] ->
      let* loc = get_location in
      let* _, typ = typeof_exp exp in
      let* () = add_error (mk_type_mismatch_error Typ loc exp typ) in
      abort
  | _ ->
      let* loc = get_location in
      let* () =
        add_error
          (WrongNumberBuiltinArgs {proc; expected= 1; given= List.length args; at_least= false; loc})
      in
      abort


and typeof_allocate_array_builtin (proc : QualifiedProcName.t) args =
  match args with
  | Exp.Typ typ :: dim :: dims ->
      let* loc = get_location in
      let+ args =
        mapM (dim :: dims) ~f:(fun exp ->
            typecheck_exp exp ~check:is_int ~expected:(SubTypeOf Int) ~loc )
      in
      (Exp.Call {proc; args= Exp.Typ typ :: args; kind= Exp.NonVirtual}, Typ.Ptr typ)
  | exp1 :: exp2 :: _ ->
      let* loc = get_location in
      let* _, typ = typeof_exp exp1 in
      let* () = add_error (mk_type_mismatch_error Typ loc exp1 typ) in
      let* _ = typecheck_exp exp2 ~check:is_int ~expected:(SubTypeOf Int) ~loc in
      abort
  | _ ->
      let* loc = get_location in
      let* () =
        add_error
          (WrongNumberBuiltinArgs {proc; expected= 2; given= List.length args; at_least= true; loc})
      in
      abort


and typeof_cast_builtin (proc : QualifiedProcName.t) args =
  match args with
  | [Exp.Typ typ; exp] ->
      let+ exp, _old_typ = typeof_exp exp in
      (Exp.Call {proc; args= [Exp.Typ typ; exp]; kind= Exp.NonVirtual}, typ)
  | [exp; _] ->
      let* loc = get_location in
      let* _, typ = typeof_exp exp in
      let* () = add_error (mk_type_mismatch_error Typ loc exp typ) in
      abort
  | _ ->
      let* loc = get_location in
      let* () =
        add_error
          (WrongNumberBuiltinArgs {proc; expected= 2; given= List.length args; at_least= false; loc})
      in
      abort


and typeof_instanceof_builtin (proc : QualifiedProcName.t) args =
  match args with
  | [exp1; (Exp.Typ _ as exp2)] ->
      let+ exp1, _ = typeof_exp exp1 in
      (Exp.Call {proc; args= [exp1; exp2]; kind= Exp.NonVirtual}, Typ.Int)
  | [_; exp] ->
      let* loc = get_location in
      let* _, typ = typeof_exp exp in
      let* () = add_error (mk_type_mismatch_error Typ loc exp typ) in
      abort
  | _ ->
      let* loc = get_location in
      let* () =
        add_error
          (WrongNumberBuiltinArgs {proc; expected= 2; given= List.length args; at_least= false; loc})
      in
      abort


let typecheck_instr (instr : Instr.t) : Instr.t monad =
  match instr with
  | Load {id; exp; typ; loc} ->
      let* () = set_location loc in
      option_value_map typ
        ~some:(fun typ ->
          let* exp =
            typecheck_exp exp
              ~check:(fun given -> compat ~assigned:(Ptr typ) ~given)
              ~expected:(SubTypeOf (Ptr typ)) ~loc
          in
          let+ () = set_ident_type id typ in
          Instr.Load {id; exp; typ= Some typ; loc} )
        ~none:
          (let* exp, typ = get_typeof_ptr_content exp in
           let+ () = set_ident_type id typ in
           Instr.Load {id; exp; typ= Some typ; loc} )
  | Store {exp1; typ; exp2; loc} ->
      let* () = set_location loc in
      option_value_map typ
        ~some:(fun typ ->
          let* exp2 =
            typecheck_exp exp2
              ~check:(fun given -> compat ~assigned:typ ~given)
              ~expected:(SubTypeOf typ) ~loc
          in
          let+ exp1 =
            typecheck_exp exp1
              ~check:(fun assigned -> compat ~assigned ~given:(Ptr typ))
              ~expected:(SuperTypeOf (Ptr typ)) ~loc
          in
          Instr.Store {exp1; typ= Some typ; exp2; loc} )
        ~none:
          (let* exp2, typ = typeof_exp exp2 in
           let+ exp1 =
             typecheck_exp exp1
               ~check:(fun assigned -> compat ~assigned ~given:(Ptr typ))
               ~expected:(SuperTypeOf (Ptr typ)) ~loc
           in
           Instr.Store {exp1; typ= Some typ; exp2; loc} )
  | Prune {exp; loc} ->
      let* () = set_location loc in
      let+ exp = typecheck_exp exp ~check:sub_int ~expected:(SubTypeOf Int) ~loc in
      Instr.Prune {exp; loc}
  | Let {id; exp; loc} ->
      let* () = set_location loc in
      let* exp, typ = typeof_exp exp in
      let+ () = set_ident_type id typ in
      Instr.Let {id; exp; loc}


let typecheck_node_call loc ({label; ssa_args} : Terminator.node_call) : Terminator.node_call monad
    =
  let* node = get_node label in
  let+ ssa_args =
    mapM2 loc ssa_args node.Node.ssa_parameters ~f:(fun exp (_, assigned) ->
        typecheck_exp exp
          ~check:(fun given -> compat ~assigned ~given)
          ~expected:(SubTypeOf assigned) ~loc )
  in
  {Terminator.label; ssa_args}


let rec typecheck_bool_exp loc (bexp : BoolExp.t) : BoolExp.t monad =
  match bexp with
  | Exp exp ->
      let+ exp = typecheck_exp exp ~check:sub_int ~expected:(SubTypeOf Int) ~loc in
      BoolExp.Exp exp
  | Not bexp ->
      let+ bexp = typecheck_bool_exp loc bexp in
      BoolExp.Not bexp
  | And (bexp1, bexp2) ->
      let* bexp1 = typecheck_bool_exp loc bexp1 in
      let+ bexp2 = typecheck_bool_exp loc bexp2 in
      BoolExp.And (bexp1, bexp2)
  | Or (bexp1, bexp2) ->
      let* bexp1 = typecheck_bool_exp loc bexp1 in
      let+ bexp2 = typecheck_bool_exp loc bexp2 in
      BoolExp.Or (bexp1, bexp2)


let rec typecheck_terminator loc (term : Terminator.t) : Terminator.t monad =
  let* () = set_location loc in
  match term with
  | If {bexp; then_; else_} ->
      let* bexp = typecheck_bool_exp loc bexp in
      let* then_ = typecheck_terminator loc then_ in
      let+ else_ = typecheck_terminator loc else_ in
      Terminator.If {bexp; then_; else_}
  | Ret exp ->
      let* result_typ = get_result_type in
      let+ exp =
        typecheck_exp exp
          ~check:(fun given -> compat ~assigned:result_typ ~given)
          ~expected:(SubTypeOf result_typ) ~loc
      in
      Terminator.Ret exp
  | Jump node_calls ->
      let+ node_calls = mapM node_calls ~f:(typecheck_node_call loc) in
      Terminator.Jump node_calls
  | Throw exp ->
      let+ exp = typecheck_exp exp ~check:is_ptr ~expected:Ptr ~loc in
      Terminator.Throw exp
  | Unreachable ->
      ret Terminator.Unreachable


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


(* TODO: typecheck unreachable nodes too? *)

(** nodes are typechecked in dfs order to make sure every idents that are read in a node, have been
    assigned a type in a dfs predecessor *)
let rec typecheck_node (node : Node.t) : unit monad =
  let* already_typechecked = has_node_been_typechecked node in
  if already_typechecked then visit_next ()
  else
    let* () = iter node.ssa_parameters ~f:(fun (id, typ) -> set_ident_type id typ) in
    let* instrs = mapM node.instrs ~f:(fun instr -> catch (typecheck_instr instr) ~with_:instr) in
    let* last = typecheck_terminator node.last_loc node.last in
    let node = {node with instrs; last} in
    let* () = mark_node_as_typechecked node in
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


let typecheck_procdesc decls globals_types (pdesc : ProcDesc.t) errors : ProcDesc.t * error list =
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
    ; typechecked_nodes= NodeName.Map.empty
    ; nodes_from_label=
        List.fold pdesc.nodes ~init:NodeName.Map.empty ~f:(fun map (node : Node.t) ->
            NodeName.Map.add node.label node map )
    ; idents= Ident.Map.empty
    ; vars= vars_with_locals
    ; dfs_stack= [pdesc.start]
    ; errors }
  in
  let _, {errors; typechecked_nodes} = visit_next () init in
  let nodes =
    (* note: this filter also removes nodes that are not reachable from the entry node *)
    List.filter_map pdesc.nodes ~f:(fun node ->
        NodeName.Map.find_opt node.Node.label typechecked_nodes )
  in
  let pdesc = {pdesc with nodes} in
  (pdesc, errors)


let run (module_ : Module.t) decls_env : (Module.t, error list) Result.t =
  let globals_type =
    TextualDecls.fold_globals decls_env ~init:VarName.Map.empty ~f:(fun map varname global ->
        VarName.Map.add varname global.typ map )
  in
  let decls, errors =
    List.fold module_.decls ~init:([], []) ~f:(fun (decls, errors) (decl : Module.decl) ->
        match decl with
        | Global _ | Struct _ | Procdecl _ ->
            (decl :: decls, errors)
        | Proc pdesc ->
            let pdesc, errors = typecheck_procdesc decls_env globals_type pdesc errors in
            (Module.Proc pdesc :: decls, errors) )
  in
  if List.is_empty errors then
    let decls = List.rev decls in
    Ok {module_ with decls}
  else Error (List.rev errors)


type type_check_result =
  | Ok of Module.t
  | Decl_errors of TextualDecls.error list
  | Type_errors of error list

let type_check module_ =
  let decls_errors, decls_env = TextualDecls.make_decls module_ in
  if not (List.is_empty decls_errors) then Decl_errors decls_errors
  else
    match run module_ decls_env with Ok module_ -> Ok module_ | Error errors -> Type_errors errors
