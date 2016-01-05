(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(* Module for implementing an algorithm for propagating dynamic types. *)

module L = Logging
open Utils

let initial_methods = ref Procname.Set.empty

(* Signature of a module that can be passed as argument to the functor     *)
(* Control_Flow below.                                                     *)
module type TODO_MAP =
sig
  type t
  type t'
  type ret_t
  type map_value
  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t'
  type map = map_value Map.t
  type context
  type field_context
  val collect_items : Exe_env.t -> Cfg.cfg -> Sil.tenv -> t -> map ->
    context -> field_context -> context * field_context * map * ret_t list
  val to_t : ret_t -> t
  val save_items_to_set : bool
  val t_to_string : t -> string
  val t'_to_string : t' -> string
  val map_value_to_string : map_value -> string
  val choose_elem : Set.t -> t option -> t
end

(* Functor for implementating the following generic algorithm: map : Node  *)
(* -> Domain TODO subsetof Node Algorithm: 1. Start with an initial TODO   *)
(* set. 2. Choose a node (remove it from TODO), update the map. 3. While   *)
(* updating the map, add nodes for which the map changed back to TODO. 4.  *)
(* Until the set is empty.                                                 *)
module Control_flow =
  functor (TM : TODO_MAP) ->
  struct

    let set_to_string set =
      let aux value = print_string ("\n item: \n"^(TM.t_to_string value)) in
      TM.Set.iter aux set

    (* The invariant holds: old_element notin todo *)
    let rec update_todo exe_env cfg0 tenv old_elem todo (map, items) context field_context =
      (* print_endline "\ntodo set: \n"; (set_to_string todo); *)
      let element = TM.choose_elem todo old_elem in
      let todo = TM.Set.remove element todo in
      let context, field_context, map, new_set_items =
        TM.collect_items exe_env cfg0 tenv element map context field_context in
      let add_to_todo set_item todo = TM.Set.add set_item todo in
      let new_set_items' = items @ new_set_items in
      let todo' =
        if (TM.save_items_to_set) then
          let new_set_items'' = IList.map TM.to_t new_set_items' in
          IList.fold_right add_to_todo new_set_items'' todo
        else todo in
      let items =
        if (TM.save_items_to_set) then []
        else new_set_items in
      if (TM.Set.is_empty todo') then (context, field_context, map, items)
      else update_todo exe_env cfg0 tenv (Some element) todo' (map, items) context field_context

  end

let get_formals cfg procname =
  let pdesc = match Cfg.Procdesc.find_from_name cfg procname with
    | Some pdesc -> pdesc
    | None -> assert false in
  Cfg.Procdesc.get_formals pdesc
  |> IList.map (fun (p, t) -> (Mangled.to_string p, t))

(* Module for defining the map to be updated: in this case it is a map     *)
(* from procedure names to a set of types for each of the procedure's      *)
(* arguments.                                                              *)
module Type_map =
struct
  type key = Procname.t

  let key_to_string procname =
    if Procname.is_constructor procname
    then (Procname.java_get_simple_class procname)^"()"
    else (Procname.java_get_simple_class procname)^"."^(Procname.java_get_method procname)

  module Map = Procname.Map

  type type_signature = (string * Sil.typ) list

  let rec type_to_string typ =
    match typ with
    | Sil.Tptr (typ , _) -> type_to_string typ
    | Sil.Tstruct (_, _, Csu.Class, Some mangled, _, _, _)
    | Sil.Tvar (Typename.TN_csu (Csu.Class, (mangled))) -> Mangled.to_string mangled
    | _ -> Sil.typ_to_string typ

  let string_typ_to_string (s, typ) =
    if s = "this" then None
    else Some (s^" -> "^(type_to_string typ))

  let rec type_signature_to_string list =
    match list with
    | [] -> ""
    | [s, typ] ->
        (match string_typ_to_string (s, typ) with
         | Some s -> s
         | None -> "")
    | (s, typ):: rest ->
        match string_typ_to_string (s, typ) with
        | Some s -> s^", "^(type_signature_to_string rest)
        | None -> (type_signature_to_string rest)

  let pair_compare = Utils.pair_compare Pervasives.compare Sil.typ_compare

  module TypeSet = Set.Make(struct
      type t = type_signature
      let compare = IList.compare pair_compare
    end)

  let map_value_to_string set =
    let elem_to_string typ s =
      let st = type_signature_to_string typ in
      if s = "" then "["^st^"]" else (s^" and  ["^st^"]") in
    (TypeSet.fold elem_to_string set "")

  type map = TypeSet.t Map.t

  type map_value = TypeSet.t

  let find_dyn_types procname map =
    try
      (Map.find procname map)
    with
      Not_found -> TypeSet.empty

  let get_set_from_map procname typ_bundle map =
    let set = find_dyn_types procname map in
    let ext_set = TypeSet.add typ_bundle set in
    ext_set

  let add_to_map procname_arg typ map =
    let ext_set = get_set_from_map procname_arg typ map in
    (Map.add procname_arg ext_set map)

  let add_set_to_map procname_arg set map =
    (Map.add procname_arg set map)
end

(* Module for defining a context to be used in the type propagation        *)
(* algorithm. A context is a map from variable names to a stack of types.  *)
(* The stack is used to model the types that are added in the different    *)
(* blocks. Each item of the stack contains a type for the variable and     *)
(* either a content type if the variable is an array or types for paths    *)
(* starting from the variable.                                             *)
module Context_map =
struct
  type key = Sil.pvar

  let key_to_string key =
    Mangled.to_string (Sil.pvar_get_name key)

  module Map = Map.Make (struct
      type t = key
      let compare = Sil.pvar_compare end)

  type var_kind =
    | VarArray of Sil.typ
    | VarBasic

  let path_equal p1 p2 =
    if (IList.length p1) != (IList.length p2) then false
    else IList.for_all2 (fun el1 el2 -> Ident.fieldname_equal el1 el2) p1 p2

  let typ_to_var_kind typ =
    match typ with
    | Sil.Tarray (typ, _)
    | Sil.Tptr(Sil.Tarray (typ, _), _) -> VarArray typ
    | _ -> VarBasic

  let var_kind_to_string (var_kind : var_kind) =
    match var_kind with
    | VarBasic -> "basic"
    | VarArray typ -> "array - content: "^(Sil.typ_to_string typ)

  type level = int

  type map_value = {
    var_level : level;
    type_stack : (Sil.typ * var_kind * level) Stack.t
  }

  type map = map_value Map.t

  let print_stack stack =
    let aux (typ, var_kind, level) =
      print_endline (
        (string_of_int level)^":"^
        (Sil.typ_to_string typ)^"-"^
        (var_kind_to_string var_kind)) in
    Stack.iter aux stack

  let print_map_value map_value =
    print_int map_value.var_level;
    print_string ":";
    print_stack map_value.type_stack

  let print_map map =
    let aux key value =
      print_string ((key_to_string key)^"->");
      print_map_value value in
    (Map.iter aux map)

  (* Updates the type of a path or adds the path with its type if it       *)
  (* wasn't there.                                                         *)
  let update_var_kind path new_typ var_kind =
    match var_kind, path with
    | VarArray typ, [] -> VarArray new_typ
    | _ -> assert false

  (* Returns the type of a path that is in the context, in case that the   *)
  (* path appears in the context. Otherwise finds the type in the tenv.    *)
  let get_type_var_kind tenv var_type path var_kind =
    match var_kind, path with
    | VarArray typ, [] -> typ
    | _ -> assert false

  (* Adds a new type for a variable. It replaces the top of the stack if   *)
  (* the level if the same as the current level, or it adds a new item to  *)
  (* the stack if it is a new level.                                       *)
  let add_type var new_typ curr_level context =
    try
      let map_value = Map.find var context in
      let stack = map_value.type_stack in
      let (typ, var_kind, curr_level) =
        if map_value.var_level = curr_level then Stack.pop stack
        else Stack.top stack in
      let _ = Stack.push (new_typ, var_kind, curr_level) stack in
      let new_map_value = { map_value with type_stack = stack } in
      Map.add var new_map_value context
    with Not_found ->
      let var_kind = typ_to_var_kind new_typ in
      let stack = Stack.create () in
      let _ = Stack.push (new_typ, var_kind, curr_level) stack in
      let map_value = { var_level = curr_level; type_stack = stack } in
      Map.add var map_value context

  (* Adds a type to a path starting from a variable. It replaces the top   *)
  (* of the stack if the level if the same as the current level, or it     *)
  (* adds a new item to the stack if it is a new level.                    *)
  let add_type_content var path new_typ curr_level context =
    try
      let map_value = Map.find var context in
      let stack = map_value.type_stack in
      let (typ, var_kind, curr_level) =
        if map_value.var_level = curr_level then Stack.pop stack
        else Stack.top stack in
      (* print_string ((key_to_string var)^"->"); print_endline            *)
      (* (var_kind_to_string var_kind); print_string "the path is ";       *)
      (* print_endline (Utils.list_to_string Ident.fieldname_to_string     *)
      (* path);                                                            *)
      let var_kind = update_var_kind path new_typ var_kind in
      let _ = Stack.push (typ, var_kind, curr_level) stack in
      let new_map_value = { map_value with type_stack = stack } in
      Map.add var new_map_value context
    with Not_found -> assert false

  (* Adds a method's parameters to the context *)
  let add_params_to_context pname type_signature context =
    let aux context (name, typ) =
      let varname = Mangled.from_string name in
      let pvar = Sil.mk_pvar varname pname in
      add_type pvar typ 0 context in
    IList.fold_left aux context type_signature

  (* Returns the top type of a variable in the context *)
  let get_type pvar context =
    let map_value =
      try Map.find pvar context
      with Not_found -> assert false in
    match Stack.top map_value.type_stack with
    | (typ, var_kind, level) ->
        typ

  (* Returns the type of a path starting from a variable in the context *)
  let get_type_content tenv pvar path context =
    let map_value =
      try Map.find pvar context
      with Not_found -> assert false in
    match Stack.top map_value.type_stack with
    | (typ, var_kind, level) ->
        (* print_string ((key_to_string pvar)^"->"); print_endline ("typ is    *)
           (* "^(Sil.typ_to_string typ)); print_endline (var_kind_to_string       *)
        (* var_kind); print_string "the path is "; print_endline               *)
        (* (Utils.list_to_string Ident.fieldname_to_string path);              *)
        get_type_var_kind tenv typ path var_kind

end

let defined_methods = ref Procname.Set.empty

let initial_node = ref (Cfg.Node.dummy ())

let rec super tenv t =
  match t with
  | Sil.Tstruct (_, _, Csu.Class, Some c2, class_name :: rest, _, _) ->
      Sil.tenv_lookup tenv class_name
  | Sil.Tarray (dom_type, _) -> None
  | Sil.Tptr (dom_type, p) ->
      let super_dom_type = super tenv dom_type in
      (match super_dom_type with
       | None -> None
       | Some super -> Some (Sil.Tptr (super, p)))
  | _ -> None

let rec lub tenv t1 t2 =
  let t1 = Sil.expand_type tenv t1 in
  let t2 = Sil.expand_type tenv t2 in
  if (Sil.typ_equal t1 t2) then t1
  else if (Prover.check_subtype tenv t1 t2) then t2
  else if (Prover.check_subtype tenv t2 t1) then t1
  else
    let st1 = (super tenv t1) in
    let st2 = (super tenv t2) in
    match st1, st2 with
    | Some st1, Some st2 -> lub tenv st1 st2
    | _ -> t1

module Field_context =
struct

  module Map = Map.Make (struct
      type t = Ident.fieldname
      let compare = Ident.fieldname_compare end)

  type map = Sil.typ Map.t

  let field_context_to_string field_context =
    let aux key value s =
      (Ident.fieldname_to_string key)^"->"^(Sil.typ_to_string value)^"\n" in
    Map.fold aux field_context ""

  let add_type tenv field typ field_context =
    let old_typ =
      try
        Map.find field field_context
      with Not_found -> typ in
    let new_typ = lub tenv old_typ typ in
    Map.add field new_typ field_context

end

(* Module for one instance of the TODO set: a set of cfg nodes. *)
module Node_TM =
struct
  type t = Cfg.Node.t

  let t_to_string node = Cfg.Node.get_description Utils.pe_text node

  type t' = Type_map.key

  let t'_to_string = Type_map.key_to_string

  type ret_t = Procname.t

  let save_items_to_set = false

  let to_t p = assert false

  type context = Context_map.map

  type field_context = Field_context.map

  module Set = Cfg.NodeSet

  module IdContext = Map.Make (struct
      type t = Ident.t
      let compare = Ident.compare end)

  type id_map_value =
    | Exp of Sil.exp
    | Typ of Sil.typ

  (* Local context for the type propagation inside one node. Because the   *)
  (* ids from one node are not visible in another node, we write their     *)
  (* types or the expressions they are identified with in a separate local *)
  (* context.                                                              *)
  type id_context = id_map_value IdContext.t

  let id_context_to_string id_context =
    let aux key value s =
      let value_to_string value =
        match value with
        | Exp exp -> (Sil.exp_to_string exp)
        | Typ typ -> Sil.typ_to_string typ in
      (Ident.to_string key)^"->"^(value_to_string value)^"\n" in
    IdContext.fold aux id_context ""

  (* Returns the type of constants. Some cases are still TODO. *)
  let get_const_type const =
    match const with
    | Sil.Cint i -> Sil.Tint Sil.IInt
    | Sil.Cfloat fl -> Sil.Tfloat Sil.FFloat
    | Sil.Cfun fn -> assert false
    | Sil.Cstr str ->
        Sil.Tptr (
          Sil.Tvar ( Typename.TN_csu (Csu.Class, (Mangled.from_string ( "java.lang.String")))),
          Sil.Pk_pointer)
    | Sil.Cattribute atr -> assert false
    | Sil.Cexn e -> assert false
    | Sil.Cclass cl -> assert false
    | Sil.Cptr_to_fld _ -> assert false
    | Sil.Ctuple _ -> assert false

  let get_id_exptyp id id_context =
    try IdContext.find id id_context
    with Not_found -> (print_endline (Ident.to_string id)); assert false

  let rec retrieve_type tenv field typ =
    match typ with
    | Sil.Tptr (ityp, _) -> retrieve_type tenv field ityp
    | _ ->
        let ityp = Sil.expand_type tenv typ in
        match ityp with
        | Sil.Tstruct (fields, sftal, csu, nameo, supers, def_mthds, iann) ->
            let (_, typ, _) =
              try ((IList.find (fun (f, t, _) -> Ident.fieldname_equal f field)) fields)
              with Not_found -> assert false in
            typ
        | _ -> assert false

  (* Returns a type for an expression taking into account the types of     *)
  (* variables in the context and the context of ids.                      *)
  let get_type tenv exp id_context context field_context =
    let rec aux exp =
      match exp with
      | Sil.Var id ->
          (match get_id_exptyp id id_context with
           | Exp exp -> aux exp
           | Typ typ -> typ)
      | Sil.UnOp (unop, exp, typ) -> aux exp
      | Sil.BinOp (binop, exp1, exp2) -> aux exp1
      | Sil.Const const -> get_const_type const
      | Sil.Cast (typ, exp) -> typ
      | Sil.Lfield (e, fld, typ) ->
          (try Field_context.Map.find fld field_context
           with Not_found -> retrieve_type tenv fld typ)
      | Sil.Lindex (Sil.Var id, i) ->
          (match get_id_exptyp id id_context with
           | Exp (Sil.Lvar pvar) ->
               Context_map.get_type_content tenv pvar [] context
           | _ -> assert false)
      | Sil.Sizeof (typ, sub) -> assert false
      | Sil.Lvar pvar ->
          Context_map.get_type pvar context
      | _ -> assert false in
    aux exp

  module Map = Type_map.Map

  let map_value_to_string = Type_map.map_value_to_string

  type map = Type_map.TypeSet.t Map.t

  type map_value = Type_map.TypeSet.t

  (* Chooses the next node to be analysed. It will be the successor of the *)
  (* current node. When the node doesn't have a successor it goes back the *)
  (* same path it analysed already and chooses the first of the ancestors  *)
  (* that has a successor.                                                 *)
  let choose_elem set el =
    let choose_start_node () =
      if Set.mem !initial_node set then !initial_node else Set.min_elt set in
    let rec aux old_node =
      (* print_endline "old node in aux is "; print_endline (t_to_string   *)
      (* old_node);                                                        *)
      let backtrack () =
        (* print_endline "backtracking..."; *)
        let preds = Cfg.Node.get_preds old_node in
        let pred =
          try IList.find (fun p -> not (Set.mem p set)) preds
          with Not_found ->
            try IList.hd preds
            with Failure "hd" -> Set.min_elt set in
        (aux pred) in
      if (Set.mem old_node set) then backtrack ()
      else
        let succs = Cfg.Node.get_succs old_node in
        let node =
          try IList.find (fun n -> ( Set.mem n set)) succs
          with Not_found -> backtrack () in
        node in
    match el with
    | Some old_node ->
        (* print_endline "choosing an element when old_element is ";           *)
        (* print_endline (t_to_string old_node);                               *)
        aux old_node
    | None -> choose_start_node ()

  let instr_to_string instr =
    let pp fmt () = Sil.pp_instr Utils.pe_text fmt instr in
    Utils.pp_to_string pp ()

  (* Goes through the instructions of a node and propagates the types.     *)
  (* When it analyses a virtual method call it adds the current dynamic    *)
  (* types that the method is called with to the map. It also collects the *)
  (* procedure names when their map changes so that they can be            *)
  (* reanalysed.                                                           *)
  let collect_items exe_env cfg0 tenv node map context field_context =
    (* print_endline "\n\nAnalyzing node: "; print_endline (t_to_string    *)
    (* node);                                                              *)
    let set_ids ids rtype id_context =
      match ids with
      | [ret_id] -> IdContext.add ret_id (Typ rtype) id_context
      | _ -> id_context in
    let aux (id_context, context, field_context, map, list) instr =
      (* print_string "\nAnalyzing instruction: "; print_endline                 *)
      (* (instr_to_string instr);                                                *)
      match instr with
      | Sil.Letderef (id, exp, typ, loc) ->
          let id_context = IdContext.add id (Exp exp) id_context in
          id_context, context, field_context, map, list
      | Sil.Set (exp1, typ, exp, loc) ->
          let exp_typ = get_type tenv exp id_context context field_context in
          let context, field_context =
            (match exp1 with
             | Sil.Lvar pvar ->
                 (* print_endline ("trying to add variable "^(Context_map.key_to_string     *)
                 (* pvar) ); print_endline ("with type "^(Sil.typ_to_string exp_typ));      *)
                 (* print_endline "Context"; Context_map.print_map context;                 *)
                 Context_map.add_type pvar exp_typ 0 context, field_context
             | Sil.Lfield (e, fld, typ) ->
                 context, Field_context.add_type tenv fld exp_typ field_context
             | Sil.Lindex (Sil.Var id, _) ->
                 (match get_id_exptyp id id_context with
                  | Exp (Sil.Lvar pvar) ->
                      Context_map.add_type_content pvar [] exp_typ 0 context, field_context
                  | _ -> assert false)
             | _ -> assert false) in
          id_context, context, field_context, map, list
      | Sil.Call (ret_ids, Sil.Const (Sil.Cfun callee_pname), actual_params, loc, call_flags)
        when not (SymExec.function_is_builtin callee_pname) ->
          (* TODO: constraint for virtual calls *)
          let cfg =
            if (Procname.Set.mem callee_pname !defined_methods) then
              Exe_env.get_cfg exe_env callee_pname
            else cfg0 in
          let pdesc = match Cfg.Procdesc.find_from_name cfg callee_pname with
            | Some pdesc -> pdesc
            | None -> assert false in
          let return_type = Cfg.Procdesc.get_ret_type pdesc in
          let id_context = set_ids ret_ids return_type id_context in
          if (Procname.Set.mem callee_pname !defined_methods) then
            let formals = Cfg.Procdesc.get_formals pdesc in
            let create_typ_bundle (exp, typ) (name, typ2) =
              (Mangled.to_string name, (get_type tenv exp id_context context field_context)) in
            let typ_bundle = IList.map2 create_typ_bundle actual_params formals in
            let set = Type_map.find_dyn_types callee_pname map in
            if Type_map.TypeSet.mem typ_bundle set
            then id_context, context, field_context, map, list
            else
              let ext_set = Type_map.TypeSet.add typ_bundle set in
              let map' = Type_map.add_set_to_map callee_pname ext_set map in
              let list = callee_pname:: list in
              id_context, context, field_context, map', list
          else id_context, context, field_context, map, list
      | Sil.Call (ret_ids, Sil.Const (Sil.Cfun callee_pname), [(exp, class_type)], loc, call_flags)
        when Procname.equal callee_pname SymExec.ModelBuiltins.__new ->
          let id_context = set_ids ret_ids class_type id_context in
          id_context, context, field_context, map, list
      | Sil.Call (ret_ids, Sil.Const (Sil.Cfun callee_pname),
                  [(array_size, array_type)], loc, call_flags)
        when Procname.equal callee_pname SymExec.ModelBuiltins.__new_array ->
          let id_context = set_ids ret_ids array_type id_context in
          id_context, context, field_context, map, list
      | Sil.Call (ret_ids, Sil.Const (Sil.Cfun callee_pname),
                  [(sil_ex, type_of_ex); (Sil.Sizeof (typ, _), Sil.Tvoid)], loc, call_flags)
        when Procname.equal callee_pname SymExec.ModelBuiltins.__cast ->
          let id_context = set_ids ret_ids typ id_context in
          id_context, context, field_context, map, list
      | Sil.Call (ret_ids, Sil.Const (Sil.Cfun callee_pname),
                  [(sil_ex, type_of_ex); (_, Sil.Tvoid)], loc, call_flags)
        when Procname.equal callee_pname SymExec.ModelBuiltins.__instanceof ->
          let id_context = set_ids ret_ids (Sil.Tint Sil.IBool) id_context in
          id_context, context, field_context, map, list
      | _ -> id_context, context, field_context, map, list in
    let instrs = Cfg.Node.get_instrs node in
    let id_context, context, field_context, map, items =
      IList.fold_left aux (IdContext.empty, context, field_context, map, []) instrs in
    context, field_context, map, items

end

(* Module for the second instance of the TODO set: a set of procedure      *)
(* names.                                                                  *)
module Typeprop_node = Control_flow (Node_TM)

module TM =
struct

  type t = Procname.t

  let t_to_string = Procname.to_string

  type t' = Type_map.key

  let t'_to_string = Type_map.key_to_string

  type ret_t = Procname.t

  module Map = Type_map.Map

  let map_value_to_string = Type_map.map_value_to_string

  type map = Type_map.TypeSet.t Map.t

  type map_value = Type_map.TypeSet.t

  module Set = Procname.Set

  let set_to_string set =
    let aux value = print_string ("\n item: "^(t_to_string value)) in
    Set.iter aux set

  let choose_elem set el =
    let element = Set.min_elt set in
    element

  let save_items_to_set = true

  let to_t p = p

  type context = Context_map.map

  type field_context = Field_context.map

  let map_to_string map =
    let aux key value s =
      s^(t'_to_string key)^" -> "^(map_value_to_string value)^"\n\n" in
    (Map.fold aux map "")

  let get_initial_node cfg proc_name =
    let pdesc =
      match Cfg.Procdesc.find_from_name cfg proc_name with
      | Some pdesc -> pdesc
      | None ->
          L.out "#### ERROR: cannot find %a ####@.@." Procname.pp proc_name;
          assert false in
    let start_node = Cfg.Procdesc.get_start_node pdesc in
    start_node

  (* Collects all the nodes from a procedure. Ignores the exceptions nodes *)
  (* for now for simplicity.                                               *)
  let collect_nodes pname initial_node =
    let rec aux nodes set =
      match nodes with
      | [] -> set
      | node:: rest ->
          if (Cfg.NodeSet.mem node set) then (aux rest set)
          else
            let set' = Cfg.NodeSet.add node set in
            let succs = Cfg.Node.get_succs node in
            (* let exns = Cfg.Node.get_exn node in *)
            (aux (succs(*@exns*)@rest) set') in
    (aux [initial_node] Cfg.NodeSet.empty)

  (* For each of the types in the set of types assigned to the method,     *)
  (* execute the type propagation algorithm and collect the updated map of *)
  (* types and list of new procedures that were updated and need to be     *)
  (* analysed again.                                                       *)
  let collect_items exe_env cfg0 tenv pname map context field_context =
    let cfg = Exe_env.get_cfg exe_env pname in
    let tenv = Exe_env.get_tenv exe_env pname in
    let init = get_initial_node cfg pname in
    initial_node := init;
    let nodes_todo = collect_nodes pname init in
    let set = Type_map.find_dyn_types pname map in
    let process_type_bundle type_bundle (map, items) =
      let context = Context_map.add_params_to_context pname type_bundle context in
      let context, field_context, map, items =
        Typeprop_node.update_todo exe_env cfg tenv None
          nodes_todo (map, items) context field_context in
      map, items in
    let map, items = Type_map.TypeSet.fold process_type_bundle set (map, []) in
    Context_map.Map.empty, field_context, map, items

end

module Typeprop = Control_flow (TM)

let map_to_string map =
  let aux key value s =
    let initial =
      try ignore(Procname.Set.find key !initial_methods); true
      with Not_found -> false in
    if initial then s
    else s^(TM.t'_to_string key)^" is called with types: "^(TM.map_value_to_string value)^"\n\n" in
  (TM.Map.fold aux map "")

let arg_desc =
  let base_arg =
    let options_to_keep = ["-results_dir"] in
    let filter arg_desc =
      IList.filter (fun desc ->
          let (option_name, _, _, _) = desc in
          IList.mem string_equal option_name options_to_keep)
        arg_desc in
    let desc = (filter Utils.base_arg_desc) in
    Utils.Arg.create_options_desc false "Parsing Options" desc in
  base_arg

let usage =
  "Usage: Typeprop -results_dir out \n"

let () =
  Utils.Arg.parse arg_desc (fun arg -> ()) usage

(* Initialises the map of types of the methods that are never called with  *)
(* the static types.                                                       *)
let initialize_map exe_env methods =
  let init_method exe_env pname map =
    let cfg = Exe_env.get_cfg exe_env pname in
    let formals = get_formals cfg pname in
    initial_methods := Procname.Set.add pname !initial_methods;
    Type_map.add_to_map pname formals map in
  let meth_list = Procname.Set.elements methods in
  let map' = (IList.fold_right (init_method exe_env) meth_list Type_map.Map.empty) in
  map'

(* Collects all the methods that are defined in the program. *)
let collect_methods exe_env =
  let global_cg = Exe_env.get_cg exe_env in
  let nodes, edges = Cg.get_nodes_and_edges global_cg in
  let do_node (n, defined, restricted) defined_methods =
    if defined then
      Procname.Set.add n defined_methods
    else defined_methods in
  let do_edge (n1, n2) no_main_methods =
    if Cg.node_defined global_cg n1 && Cg.node_defined global_cg n2 then
      Procname.Set.add n2 no_main_methods
    else no_main_methods in
  let defined = IList.fold_right do_node nodes Procname.Set.empty in
  let no_main_methods = IList.fold_right do_edge edges Procname.Set.empty in
  let main_methods = Procname.Set.diff defined no_main_methods in
  defined_methods := defined;
  (* TM.set_to_string main_methods; *)
  main_methods

(* Performs type propagation for a program *)
let type_prop_do exe_env =
  let main_methods = collect_methods exe_env in
  let map = initialize_map exe_env main_methods in
  let tenv = Sil.create_tenv () in
  let context, field_context, map, list =
    Typeprop.update_todo exe_env (Obj.magic ()) tenv None main_methods
      (map, []) Context_map.Map.empty Field_context.Map.empty in
  map

(* Loads the local control graphs of a program to create a global control  *)
(* graph.                                                                  *)
let load_cg_files _exe_env (source_dirs : DB.source_dir list) =
  let load_cg_file (_exe_env: Exe_env.initial) (source_dir : DB.source_dir) =
    match Exe_env.add_cg _exe_env source_dir with
    | None -> ()
    | Some cg ->
        (*L.err "loaded %s@." (DB.source_dir_to_string source_dir) *) () in
  IList.iter (fun source_dir -> load_cg_file _exe_env source_dir) source_dirs;
  let exe_env = Exe_env.freeze _exe_env in
  exe_env

(* Loads the control graph and executes the type propagation algorithm.    *)
(* TODO: serialize and save the map after its computation.                 *)
let type_prop () =
  let source_dirs = DB.find_source_dirs () in
  let _exe_env = Exe_env.create None in
  let exe_env = load_cg_files _exe_env source_dirs in
  let map = type_prop_do exe_env in
  print_endline "\n";
  print_endline (map_to_string map);
  print_endline "\n";
  ()

let () = type_prop ()
