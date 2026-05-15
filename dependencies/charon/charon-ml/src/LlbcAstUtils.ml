include GAstUtils
open Utils
open Collections
open Meta
open MetaUtils
open Types
open LlbcAst

(** Returns a list of all functions in a crate *)
let fun_decl_list_from_crate (crate : crate) : fun_decl list =
  snd (List.split (FunDeclId.Map.bindings crate.fun_decls))

(** Returns a list option of all arguments of a functions If a function does not
    have a body, it cannot access the locals field where the arguments are so it
    returns None *)
let get_fun_args (fun_decl : fun_decl) : local list option =
  match fun_decl.body with
  | Some body -> Some (GAstUtils.locals_get_input_vars body.locals)
  | None -> None

(** Check if a {!type:Charon.LlbcAst.statement} contains loops *)
let block_has_loops (blk : block) : bool =
  let obj =
    object
      inherit [_] iter_statement
      method! visit_Loop _ _ = raise Found
    end
  in
  try
    obj#visit_block () blk;
    false
  with Found -> true

(** Check if a {!type:Charon.LlbcAst.fun_decl} contains loops *)
let fun_decl_has_loops (fd : fun_decl) : bool =
  match fd.body with
  | Some body -> block_has_loops body.body
  | None -> false

let crate_get_item_meta (m : crate) (id : item_id) : Types.item_meta option =
  match id with
  | IdType id ->
      Option.map
        (fun (d : Types.type_decl) -> d.item_meta)
        (Types.TypeDeclId.Map.find_opt id m.type_decls)
  | IdFun id ->
      Option.map
        (fun (d : fun_decl) -> d.item_meta)
        (FunDeclId.Map.find_opt id m.fun_decls)
  | IdGlobal id ->
      Option.map
        (fun (d : global_decl) -> d.item_meta)
        (GlobalDeclId.Map.find_opt id m.global_decls)
  | IdTraitDecl id ->
      Option.map
        (fun (d : trait_decl) -> d.item_meta)
        (TraitDeclId.Map.find_opt id m.trait_decls)
  | IdTraitImpl id ->
      Option.map
        (fun (d : trait_impl) -> d.item_meta)
        (TraitImplId.Map.find_opt id m.trait_impls)

class ['self] map_crate =
  object (self : 'self)
    inherit [_] map_statement

    method visit_expr_body env (body : expr_body) : expr_body =
      let { span; locals; bound_body_regions; body } = body in
      let span = self#visit_span env span in
      let locals = self#visit_locals env locals in
      let body = self#visit_block env body in
      { span; locals; bound_body_regions; body }

    method visit_fun_decl env (decl : fun_decl) : fun_decl =
      let {
        def_id;
        item_meta;
        generics;
        signature;
        src;
        is_global_initializer;
        body;
      } =
        decl
      in
      let def_id = self#visit_fun_decl_id env def_id in
      let item_meta = self#visit_item_meta env item_meta in
      let generics = self#visit_generic_params env generics in
      let signature = self#visit_fun_sig env signature in
      let src = self#visit_item_source env src in
      let is_global_initializer =
        self#visit_option self#visit_global_decl_id env is_global_initializer
      in
      let body = self#visit_option self#visit_expr_body env body in
      {
        def_id;
        item_meta;
        generics;
        signature;
        src;
        is_global_initializer;
        body;
      }

    method visit_declaration_group env (g : declaration_group) :
        declaration_group =
      match g with
      | TypeGroup g -> TypeGroup (self#visit_type_declaration_group env g)
      | FunGroup g -> FunGroup (self#visit_fun_declaration_group env g)
      | GlobalGroup g -> GlobalGroup (self#visit_global_declaration_group env g)
      | TraitDeclGroup g ->
          TraitDeclGroup (self#visit_trait_declaration_group env g)
      | TraitImplGroup g -> TraitImplGroup (self#visit_trait_impl_group env g)
      | MixedGroup g -> MixedGroup (self#visit_mixed_declaration_group env g)

    method visit_type_declaration_group env (g : type_declaration_group) =
      g_declaration_group_map (self#visit_type_decl_id env) g

    method visit_fun_declaration_group env (g : fun_declaration_group) =
      g_declaration_group_map (self#visit_fun_decl_id env) g

    method visit_global_declaration_group env (g : global_declaration_group) =
      g_declaration_group_map (self#visit_global_decl_id env) g

    method visit_trait_declaration_group env (g : trait_declaration_group) =
      g_declaration_group_map (self#visit_trait_decl_id env) g

    method visit_trait_impl_group env (g : trait_impl_group) =
      g_declaration_group_map (self#visit_trait_impl_id env) g

    method visit_mixed_declaration_group env (g : mixed_declaration_group) =
      g_declaration_group_map (self#visit_item_id env) g

    method visit_cli_options env (option : cli_options) : cli_options = option

    method visit_target_info env (target_info : target_info) : target_info =
      target_info

    method visit_crate env (crate : crate) : crate =
      let {
        name;
        options;
        target_information;
        declarations;
        type_decls;
        fun_decls;
        global_decls;
        trait_decls;
        trait_impls;
        unit_metadata;
      } =
        crate
      in
      let name = self#visit_string env name in
      let options = self#visit_cli_options env options in
      let target_information = self#visit_target_info env target_information in
      let declarations =
        List.map (self#visit_declaration_group env) declarations
      in
      let type_decls =
        TypeDeclId.Map.map (self#visit_type_decl env) type_decls
      in
      let fun_decls = FunDeclId.Map.map (self#visit_fun_decl env) fun_decls in
      let global_decls =
        GlobalDeclId.Map.map (self#visit_global_decl env) global_decls
      in
      let trait_decls =
        TraitDeclId.Map.map (self#visit_trait_decl env) trait_decls
      in
      let trait_impls =
        TraitImplId.Map.map (self#visit_trait_impl env) trait_impls
      in
      let unit_metadata = self#visit_global_decl_ref env unit_metadata in
      {
        name;
        options;
        target_information;
        declarations;
        type_decls;
        fun_decls;
        global_decls;
        trait_decls;
        trait_impls;
        unit_metadata;
      }
  end

class ['self] iter_crate =
  object (self : 'self)
    inherit [_] iter_statement

    method visit_expr_body env (body : expr_body) : unit =
      let { span; locals; bound_body_regions; body } = body in
      self#visit_int env bound_body_regions;
      self#visit_span env span;
      self#visit_locals env locals;
      self#visit_block env body

    method visit_fun_decl env (decl : fun_decl) : unit =
      let {
        def_id;
        item_meta;
        generics;
        signature;
        src;
        is_global_initializer;
        body;
      } =
        decl
      in
      self#visit_fun_decl_id env def_id;
      self#visit_item_meta env item_meta;
      self#visit_generic_params env generics;
      self#visit_fun_sig env signature;
      self#visit_item_source env src;
      self#visit_option self#visit_global_decl_id env is_global_initializer;
      self#visit_option self#visit_expr_body env body

    method visit_declaration_group env (g : declaration_group) : unit =
      match g with
      | TypeGroup g -> self#visit_type_declaration_group env g
      | FunGroup g -> self#visit_fun_declaration_group env g
      | GlobalGroup g -> self#visit_global_declaration_group env g
      | TraitDeclGroup g -> self#visit_trait_declaration_group env g
      | TraitImplGroup g -> self#visit_trait_impl_group env g
      | MixedGroup g -> self#visit_mixed_declaration_group env g

    method visit_type_declaration_group env (g : type_declaration_group) : unit
        =
      let ids = g_declaration_group_to_list g in
      List.iter (self#visit_type_decl_id env) ids

    method visit_fun_declaration_group env (g : fun_declaration_group) : unit =
      let ids = g_declaration_group_to_list g in
      List.iter (self#visit_fun_decl_id env) ids

    method visit_global_declaration_group env (g : global_declaration_group) :
        unit =
      let ids = g_declaration_group_to_list g in
      List.iter (self#visit_global_decl_id env) ids

    method visit_trait_declaration_group env (g : trait_declaration_group) :
        unit =
      let ids = g_declaration_group_to_list g in
      List.iter (self#visit_trait_decl_id env) ids

    method visit_trait_impl_group env (g : trait_impl_group) : unit =
      let ids = g_declaration_group_to_list g in
      List.iter (self#visit_trait_impl_id env) ids

    method visit_mixed_declaration_group env (g : mixed_declaration_group) :
        unit =
      let ids = g_declaration_group_to_list g in
      List.iter (self#visit_item_id env) ids

    method visit_cli_options env (option : cli_options) : unit = ()

    method visit_crate env (crate : crate) : unit =
      let {
        name;
        options;
        target_information;
        declarations;
        type_decls;
        fun_decls;
        global_decls;
        trait_decls;
        trait_impls;
        unit_metadata;
      } =
        crate
      in
      self#visit_string env name;
      self#visit_cli_options env options;
      List.iter (self#visit_declaration_group env) declarations;
      TypeDeclId.Map.iter (fun _ -> self#visit_type_decl env) type_decls;
      FunDeclId.Map.iter (fun _ -> self#visit_fun_decl env) fun_decls;
      GlobalDeclId.Map.iter (fun _ -> self#visit_global_decl env) global_decls;
      TraitDeclId.Map.iter (fun _ -> self#visit_trait_decl env) trait_decls;
      TraitImplId.Map.iter (fun _ -> self#visit_trait_impl env) trait_impls;
      self#visit_global_decl_ref env unit_metadata
  end

(** This visitor keeps track of the last (most precise) span it found, together
    with the id of the declaration it is currently exploring (the environment it
    carries is a pair (item_id, span)). *)
class ['self] map_crate_with_span =
  object (self)
    inherit [_] map_crate as super

    method! visit_statement decl_span_info st =
      let decl_span_info =
        Option.map (fun (decl_id, _) -> (decl_id, st.span)) decl_span_info
      in
      super#visit_statement decl_span_info st

    method! visit_block decl_span_info block =
      let decl_span_info =
        Option.map (fun (decl_id, _) -> (decl_id, block.span)) decl_span_info
      in
      super#visit_block decl_span_info block

    method! visit_variant decl_span_info (variant : variant) =
      let decl_span_info =
        Option.map (fun (decl_id, _) -> (decl_id, variant.span)) decl_span_info
      in
      super#visit_variant decl_span_info variant

    method! visit_trait_param decl_span_info (clause : trait_param) =
      let decl_span_info =
        match (decl_span_info, clause.span) with
        | Some (decl_id, _), Some span -> Some (decl_id, span)
        | _ -> decl_span_info
      in
      super#visit_trait_param decl_span_info clause

    method! visit_field decl_span_info (field : field) =
      let decl_span_info =
        Option.map (fun (decl_id, _) -> (decl_id, field.span)) decl_span_info
      in
      super#visit_field decl_span_info field

    method! visit_expr_body (decl_span_info : (item_id * span) option)
        (body : expr_body) : expr_body =
      let decl_span_info =
        Option.map (fun (decl_id, _) -> (decl_id, body.span)) decl_span_info
      in
      super#visit_expr_body decl_span_info body

    method! visit_fun_decl (_ : (item_id * span) option) (decl : fun_decl) :
        fun_decl =
      let decl_span_info = Some (IdFun decl.def_id, decl.item_meta.span) in
      super#visit_fun_decl decl_span_info decl

    method! visit_global_decl (_ : (item_id * span) option) (decl : global_decl)
        =
      let decl_span_info = Some (IdGlobal decl.def_id, decl.item_meta.span) in
      super#visit_global_decl decl_span_info decl

    method! visit_trait_decl (_ : (item_id * span) option) (decl : trait_decl) =
      let decl_span_info =
        Some (IdTraitDecl decl.def_id, decl.item_meta.span)
      in
      super#visit_trait_decl decl_span_info decl

    method! visit_trait_impl (_ : (item_id * span) option) (decl : trait_impl) =
      let decl_span_info =
        Some (IdTraitImpl decl.def_id, decl.item_meta.span)
      in
      super#visit_trait_impl decl_span_info decl
  end

(** This visitor keeps track of the last (most precise) span it found, together
    with the id of the declaration it is currently exploring (the environment it
    carries is a pair (item_id, span)). *)
class ['self] iter_crate_with_span =
  object (self)
    inherit [_] iter_crate as super

    method! visit_statement decl_span_info st =
      let decl_span_info =
        Option.map (fun (decl_id, _) -> (decl_id, st.span)) decl_span_info
      in
      super#visit_statement decl_span_info st

    method! visit_block decl_span_info block =
      let decl_span_info =
        Option.map (fun (decl_id, _) -> (decl_id, block.span)) decl_span_info
      in
      super#visit_block decl_span_info block

    method! visit_variant decl_span_info (variant : variant) =
      let decl_span_info =
        Option.map (fun (decl_id, _) -> (decl_id, variant.span)) decl_span_info
      in
      super#visit_variant decl_span_info variant

    method! visit_trait_param decl_span_info (clause : trait_param) =
      let decl_span_info =
        match (decl_span_info, clause.span) with
        | Some (decl_id, _), Some span -> Some (decl_id, span)
        | _ -> decl_span_info
      in
      super#visit_trait_param decl_span_info clause

    method! visit_field decl_span_info (field : field) =
      let decl_span_info =
        Option.map (fun (decl_id, _) -> (decl_id, field.span)) decl_span_info
      in
      super#visit_field decl_span_info field

    method! visit_expr_body (decl_span_info : (item_id * span) option)
        (body : expr_body) : unit =
      let decl_span_info =
        Option.map (fun (decl_id, _) -> (decl_id, body.span)) decl_span_info
      in
      super#visit_expr_body decl_span_info body

    method! visit_fun_decl (_ : (item_id * span) option) (decl : fun_decl) :
        unit =
      let decl_span_info = Some (IdFun decl.def_id, decl.item_meta.span) in
      super#visit_fun_decl decl_span_info decl

    method! visit_global_decl (_ : (item_id * span) option) (decl : global_decl)
        =
      let decl_span_info = Some (IdGlobal decl.def_id, decl.item_meta.span) in
      super#visit_global_decl decl_span_info decl

    method! visit_trait_decl (_ : (item_id * span) option) (decl : trait_decl) =
      let decl_span_info =
        Some (IdTraitDecl decl.def_id, decl.item_meta.span)
      in
      super#visit_trait_decl decl_span_info decl

    method! visit_trait_impl (_ : (item_id * span) option) (decl : trait_impl) =
      let decl_span_info =
        Some (IdTraitImpl decl.def_id, decl.item_meta.span)
      in
      super#visit_trait_impl decl_span_info decl
  end

let map_statement (f : statement -> statement list) (b : block) : block =
  let visitor =
    object (self)
      inherit [_] map_statement_base

      method! visit_block env b =
        let update st = f (self#visit_statement env st) in
        { b with statements = List.flatten (List.map update b.statements) }
    end
  in
  visitor#visit_block () b
