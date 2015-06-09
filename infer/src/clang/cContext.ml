(*
* Copyright (c) 2013 - Facebook.
* All rights reserved.
*)

(** Contains current class and current method to be translated as well as local variables, *)
(** and the cg, cfg, and tenv corresponding to the current file. *)

open Utils
open CFrontend_utils

type varStack = (Mangled.t * Sil.typ * int) Stack.t

type varMap = varStack StringMap.t

type pointerVarMap = Sil.pvar StringMap.t

module L = Logging

type curr_class =
  | ContextCls of string * string option * string list
  (*class name and name of (optional) super class , and a list of protocols *)
  | ContextCategory of string * string (* category name and corresponding class *)
  | ContextProtocol of string  (* category name and corresponding class *)
  | ContextNoCls

type t =
  {
    tenv : Sil.tenv;
    cg : Cg.t;
    cfg : Cfg.cfg;
    procdesc : Cfg.Procdesc.t;
    is_objc_method : bool;
    is_instance : bool;
    curr_class: curr_class;
    is_callee_expression : bool;
    namespace: string option; (* contains the name of the namespace if we are in the scope of one*)
    mutable local_vars : (Mangled.t * Sil.typ * bool) list; (* (name, type, is_static flag) *)
    mutable captured_vars : (Mangled.t * Sil.typ * bool) list; (* (name, type, is_static flag) *)
    mutable local_vars_stack : varMap;
    mutable local_vars_pointer : pointerVarMap
  }

module LocalVars =
struct

  module Block =
  struct
    let depth_counter = ref 0

    let enter () =
      depth_counter := !depth_counter + 1

    let leave () =
      depth_counter := !depth_counter - 1

    let reset () =
      depth_counter := 0

    let depth () = !depth_counter

  end

  let lookup_var_map context var_name =
    try
      StringMap.find var_name context.local_vars_stack
    with Not_found -> Stack.create ()

  let lookup_var_formals context procname var_name =
    let formals = Cfg.Procdesc.get_formals context.procdesc in
    let arg, typ = list_find (fun (arg, typ) -> arg = var_name) formals in
    let var = Sil.mk_pvar (Mangled.from_string var_name) procname in
    (var, typ)

  let lookup_var_captured context procname var_name =
    let cv, typ, _ = list_find (fun (arg, typ, _) -> arg = Mangled.from_string var_name) context.captured_vars in
    let var = Sil.mk_pvar cv procname in
    (var, typ)

  let lookup_var_globals context procname name =
    let var_name = Mangled.from_string name in
    let global_var = CGlobal_vars.find var_name in
    let var = CGlobal_vars.var_get_name global_var in
    Printing.log_out ~fmt:"   ...Variable '%s' found in globals!!\n" (Sil.pvar_to_string var);
    let typ = CGlobal_vars.var_get_typ global_var in
    var, typ

  let is_captured_var context name =
    list_exists (fun (v, _, _) -> (Mangled.to_string v) = name) context.captured_vars

  let is_global_var context name =
    try
      let procname = Cfg.Procdesc.get_proc_name context.procdesc in
      ignore (lookup_var_globals context procname name);
      true
    with Not_found -> false

  let print_locals context =
    let print_stack var_name stack =
      Stack.iter
        (fun (var_name, typ, level) ->
              Printing.log_out ~fmt:"var item %s:" (Mangled.to_string var_name);
              Printing.log_out ~fmt:"%s" (Sil.typ_to_string typ);
              Printing.log_out ~fmt:"- %s \n%!" (string_of_int level)) stack in
    Printing.log_out "LOCAL VARS:%s\n";
    StringMap.iter print_stack context.local_vars_stack

  let print_pointer_vars context =
    let print_pointer_var pointer var =
      Printing.log_out ~fmt:"%s ->" pointer;
      Printing.log_out ~fmt:" %s\n" (Sil.pvar_to_string var) in
    Printing.log_out "POINTER VARS:\n";
    StringMap.iter print_pointer_var context.local_vars_pointer

  let add_pointer_var pointer var context =
    Printing.log_out ~fmt:"     ...Adding pointer '%s' " pointer;
    Printing.log_out ~fmt:"to the map with variable '%s'\n%!" (Sil.pvar_to_string var);
    context.local_vars_pointer <- StringMap.add pointer var context.local_vars_pointer

  let find_var_with_pointer context pointer =
    try
      StringMap.find pointer context.local_vars_pointer
    with Not_found ->
        (Printing.log_err ~fmt:"   ...Variable for pointer %s not found!!\n%!" pointer);
        print_pointer_vars context;
        assert false

  let lookup_var_locals context procname var_name =
    let stack = lookup_var_map context var_name in
    let (var_name, typ, level) = Stack.top stack in
    Printing.log_out ~fmt:"   ...Variable %s found in locals!!\n%!" (Mangled.to_string var_name);
    (Sil.mk_pvar var_name procname), typ

  let lookup_var context pointer var_name kind =
    let procname = Cfg.Procdesc.get_proc_name context.procdesc in
    if (kind = `Var) then
      try
        Some (fst (lookup_var_locals context procname var_name))
      with Stack.Empty ->
          try
            Some (fst (lookup_var_globals context procname var_name))
          with Not_found ->
              if is_captured_var context var_name then
                try (* if it's a captured variable we need to look at the parameters list*)
                  Some (fst (lookup_var_formals context procname var_name))
                with Not_found ->
                    Printing.log_err ~fmt:"Variable %s not found!!\n%!" var_name;
                    print_locals context;
                    None
              else None
    else if (kind = `ParmVar) then
      try
        Some (fst (lookup_var_formals context procname var_name))
      with Not_found ->
          let list_to_string = list_to_string (fun (a, typ) -> a^":"^(Sil.typ_to_string typ)) in
          Printing.log_err ~fmt:"Warning: Parameter %s not found!!\n%!" var_name;
          Printing.log_err ~fmt:"Formals of procdesc %s" (Procname.to_string procname);
          Printing.log_err ~fmt:" are %s\n%!" (list_to_string (Cfg.Procdesc.get_formals context.procdesc));
          Printing.print_failure_info pointer;
          assert false
    else if (kind = `Function || kind = `ImplicitParam) then (
      (* ImplicitParam are 'self' and '_cmd'. These are never defined but they can be referred to in the code. *)
      Printing.log_err ~fmt:"Creating a variable for '%s' \n%!" var_name;
      Some (Sil.mk_pvar (Mangled.from_string var_name) procname))
    else if (kind = `EnumConstant) then
      (Printing.print_failure_info pointer;
        assert false)
    else (Printing.log_err ~fmt:"WARNING: In lookup_var kind %s not handled. Giving up!\n%!" (Clang_ast_j.string_of_decl_kind kind);
      Printing.print_failure_info pointer;
      assert false)

  let get_variable_name name =
    Mangled.mangled name ((string_of_int(Block.depth ())))

  let add_local_var context var_name typ pointer is_static =
    Printing.log_out ~fmt:"     ...Creating var %s" var_name;
    Printing.log_out ~fmt:" with pointer %s\n" pointer;
    if not (is_global_var context var_name) || is_static then
      let var = get_variable_name var_name in
      context.local_vars <- context.local_vars@[(var, typ, is_static)] ;
      let stack = lookup_var_map context var_name in
      let procname = Cfg.Procdesc.get_proc_name context.procdesc in
      let pvar = Sil.mk_pvar var procname in
      Stack.push (var, typ, Block.depth ()) stack;
      context.local_vars_stack <- StringMap.add var_name stack context.local_vars_stack;
      add_pointer_var pointer pvar context

  let reset_local_vars context =
    context.local_vars <- []

  let get_local_vars context =
    context.local_vars

  let remove_top_level_local_vars context =
    let remove_top var_name stack =
      try
        let (top_var, top_typ, top_level) = Stack.top stack in
        if top_level == (Block.depth ()) then
          (ignore (Stack.pop stack);
            context.local_vars_stack <-
            StringMap.add var_name stack context.local_vars_stack)
        else ()
      with Stack.Empty -> () in
    StringMap.iter remove_top context.local_vars_stack

  let enter_and_leave_scope context f lstmt =
    Block.enter ();
    f context lstmt;
    remove_top_level_local_vars context;
    Block.leave ()

  let reset_block = Block.reset

end

let create_context tenv cg cfg procdesc ns curr_class is_objc_method cv is_instance =
  { tenv = tenv;
    cg = cg;
    cfg = cfg;
    procdesc = procdesc;
    curr_class = curr_class;
    is_callee_expression = false;
    is_objc_method = is_objc_method;
    is_instance = is_instance;
    namespace = ns;
    local_vars = [];
    captured_vars = cv;
    local_vars_stack = StringMap.empty;
    local_vars_pointer = StringMap.empty
  }

let get_cfg context = context.cfg

let get_cg context = context.cg

let get_tenv context = context.tenv

let get_procdesc context = context.procdesc

let is_objc_method context = context.is_objc_method

let get_curr_class context = context.curr_class

let get_curr_class_name curr_class =
  match curr_class with
  | ContextCls (name, _, _) -> name
  | ContextCategory (name, cls) -> cls
  | ContextProtocol name -> name
  | ContextNoCls -> assert false

let curr_class_to_string curr_class =
  match curr_class with
  | ContextCls (name, _, _) -> ("class "^name)
  | ContextCategory (name, cls) -> ("category "^name^" of class "^cls)
  | ContextProtocol name -> ("protocol "^name)
  | ContextNoCls -> "no class"

let curr_class_compare curr_class1 curr_class2 =
  match curr_class1, curr_class2 with
  | ContextCls (name1, _, _), ContextCls (name2, _, _) ->
      String.compare name1 name2
  | ContextCls (_, _, _), _ -> -1
  | _, ContextCls (_, _, _) -> 1
  | ContextCategory (name1, cls1), ContextCategory (name2, cls2) ->
      Utils.pair_compare String.compare String.compare (name1, cls1) (name2, cls2)
  | ContextCategory (_, _), _ -> -1
  | _, ContextCategory (_, _) -> 1
  | ContextProtocol name1, ContextProtocol name2 ->
      String.compare name1 name2
  | ContextProtocol _, _ -> -1
  | _, ContextProtocol _ -> 1
  | ContextNoCls, ContextNoCls -> 0

let curr_class_equal curr_class1 curr_class2 =
  curr_class_compare curr_class1 curr_class2 == 0

let curr_class_hash curr_class =
  match curr_class with
  | ContextCls (name, _, _) -> Hashtbl.hash name
  | ContextCategory (name, cls) -> Hashtbl.hash (name, cls)
  | ContextProtocol name -> Hashtbl.hash name
  | ContextNoCls -> Hashtbl.hash "no class"

let get_qt_curr_class curr_class =
  (get_curr_class_name curr_class)^" *"

let get_captured_vars context = context.captured_vars



