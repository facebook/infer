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

let crate_get_item_meta (m : crate) (id : any_decl_id) : Types.item_meta option
    =
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

(** This visitor keeps track of the last (most precise) span it found, together
    with the id of the declaration it is currently exploring (the environment it
    carries is a pair (any_decl_id, span)). *)
class ['self] map_crate_with_span =
  object (self)
    inherit [_] map_statement as super

    method! visit_statement decl_span_info st =
      let decl_span_info =
        Option.map (fun (decl_id, _) -> (decl_id, st.span)) decl_span_info
      in
      super#visit_statement decl_span_info st

    method! visit_variant decl_span_info (variant : variant) =
      let decl_span_info =
        Option.map (fun (decl_id, _) -> (decl_id, variant.span)) decl_span_info
      in
      super#visit_variant decl_span_info variant

    method! visit_trait_clause decl_span_info (clause : trait_clause) =
      let decl_span_info =
        match (decl_span_info, clause.span) with
        | Some (decl_id, _), Some span -> Some (decl_id, span)
        | _ -> decl_span_info
      in
      super#visit_trait_clause decl_span_info clause

    method! visit_field decl_span_info (field : field) =
      let decl_span_info =
        Option.map (fun (decl_id, _) -> (decl_id, field.span)) decl_span_info
      in
      super#visit_field decl_span_info field

    method visit_expr_body (decl_span_info : (any_decl_id * span) option)
        (body : expr_body) : expr_body =
      let { span; locals; body } = body in
      let decl_span_info =
        Option.map (fun (decl_id, _) -> (decl_id, body.span)) decl_span_info
      in
      let span = self#visit_span decl_span_info span in
      let locals = self#visit_locals decl_span_info locals in
      let body = self#visit_block decl_span_info body in
      { span; locals; body }

    method visit_fun_decl (_ : (any_decl_id * span) option) (decl : fun_decl) :
        fun_decl =
      let { def_id; item_meta; signature; kind; is_global_initializer; body } =
        decl
      in
      let decl_span_info = Some (IdFun def_id, item_meta.span) in
      let def_id = self#visit_fun_decl_id decl_span_info def_id in
      let item_meta = self#visit_item_meta decl_span_info item_meta in
      let signature = self#visit_fun_sig decl_span_info signature in
      let kind = self#visit_item_kind decl_span_info kind in
      let is_global_initializer =
        self#visit_option self#visit_global_decl_id decl_span_info
          is_global_initializer
      in
      let body = self#visit_option self#visit_expr_body decl_span_info body in
      { def_id; item_meta; signature; kind; is_global_initializer; body }

    method! visit_global_decl (_ : (any_decl_id * span) option)
        (decl : global_decl) =
      let decl_span_info = Some (IdGlobal decl.def_id, decl.item_meta.span) in
      super#visit_global_decl decl_span_info decl

    method! visit_trait_decl (_ : (any_decl_id * span) option)
        (decl : trait_decl) =
      let decl_span_info =
        Some (IdTraitDecl decl.def_id, decl.item_meta.span)
      in
      super#visit_trait_decl decl_span_info decl

    method! visit_trait_impl (_ : (any_decl_id * span) option)
        (decl : trait_impl) =
      let decl_span_info =
        Some (IdTraitImpl decl.def_id, decl.item_meta.span)
      in
      super#visit_trait_impl decl_span_info decl

    method visit_declaration_group
        (decl_span_info : (any_decl_id * span) option) (g : declaration_group) :
        declaration_group =
      match g with
      | TypeGroup g ->
          TypeGroup (self#visit_type_declaration_group decl_span_info g)
      | FunGroup g ->
          FunGroup (self#visit_fun_declaration_group decl_span_info g)
      | GlobalGroup g ->
          GlobalGroup (self#visit_global_declaration_group decl_span_info g)
      | TraitDeclGroup g ->
          TraitDeclGroup (self#visit_trait_declaration_group decl_span_info g)
      | TraitImplGroup g ->
          TraitImplGroup (self#visit_trait_impl_group decl_span_info g)
      | MixedGroup g ->
          MixedGroup (self#visit_mixed_declaration_group decl_span_info g)

    method visit_type_declaration_group
        (decl_span_info : (any_decl_id * span) option)
        (g : type_declaration_group) =
      g_declaration_group_map (self#visit_type_decl_id decl_span_info) g

    method visit_fun_declaration_group
        (decl_span_info : (any_decl_id * span) option)
        (g : fun_declaration_group) =
      g_declaration_group_map (self#visit_fun_decl_id decl_span_info) g

    method visit_global_declaration_group
        (decl_span_info : (any_decl_id * span) option)
        (g : global_declaration_group) =
      g_declaration_group_map (self#visit_global_decl_id decl_span_info) g

    method visit_trait_declaration_group
        (decl_span_info : (any_decl_id * span) option)
        (g : trait_declaration_group) =
      g_declaration_group_map (self#visit_trait_decl_id decl_span_info) g

    method visit_trait_impl_group (decl_span_info : (any_decl_id * span) option)
        (g : trait_impl_group) =
      g_declaration_group_map (self#visit_trait_impl_id decl_span_info) g

    method visit_mixed_declaration_group
        (decl_span_info : (any_decl_id * span) option)
        (g : mixed_declaration_group) =
      g_declaration_group_map (self#visit_any_decl_id decl_span_info) g

    method visit_cli_options (decl_span_info : (any_decl_id * span) option)
        (option : cli_options) : cli_options =
      option

    method visit_target_info (decl_span_info : (any_decl_id * span) option)
        (target_info : target_info) : target_info =
      target_info

    method visit_crate (decl_span_info : (any_decl_id * span) option)
        (crate : crate) : crate =
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
      } =
        crate
      in
      let name = self#visit_string decl_span_info name in
      let options = self#visit_cli_options decl_span_info options in
      let target_information =
        self#visit_target_info decl_span_info target_information
      in
      let declarations =
        List.map (self#visit_declaration_group decl_span_info) declarations
      in
      let type_decls =
        TypeDeclId.Map.map (self#visit_type_decl decl_span_info) type_decls
      in
      let fun_decls =
        FunDeclId.Map.map (self#visit_fun_decl decl_span_info) fun_decls
      in
      let global_decls =
        GlobalDeclId.Map.map
          (self#visit_global_decl decl_span_info)
          global_decls
      in
      let trait_decls =
        TraitDeclId.Map.map (self#visit_trait_decl decl_span_info) trait_decls
      in
      let trait_impls =
        TraitImplId.Map.map (self#visit_trait_impl decl_span_info) trait_impls
      in
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
      }
  end

(** This visitor keeps track of the last (most precise) span it found, together
    with the id of the declaration it is currently exploring (the environment it
    carries is a pair (any_decl_id, span)). *)
class ['self] iter_crate_with_span =
  object (self)
    inherit [_] iter_statement as super

    method! visit_statement decl_span_info st =
      let decl_span_info =
        Option.map (fun (decl_id, _) -> (decl_id, st.span)) decl_span_info
      in
      super#visit_statement decl_span_info st

    method! visit_variant decl_span_info (variant : variant) =
      let decl_span_info =
        Option.map (fun (decl_id, _) -> (decl_id, variant.span)) decl_span_info
      in
      super#visit_variant decl_span_info variant

    method! visit_trait_clause decl_span_info (clause : trait_clause) =
      let decl_span_info =
        match (decl_span_info, clause.span) with
        | Some (decl_id, _), Some span -> Some (decl_id, span)
        | _ -> decl_span_info
      in
      super#visit_trait_clause decl_span_info clause

    method! visit_field decl_span_info (field : field) =
      let decl_span_info =
        Option.map (fun (decl_id, _) -> (decl_id, field.span)) decl_span_info
      in
      super#visit_field decl_span_info field

    method visit_expr_body (decl_span_info : (any_decl_id * span) option)
        (body : expr_body) : unit =
      let { span; locals; body } = body in
      let decl_span_info =
        Option.map (fun (decl_id, _) -> (decl_id, body.span)) decl_span_info
      in
      self#visit_span decl_span_info span;
      self#visit_locals decl_span_info locals;
      self#visit_block decl_span_info body

    method visit_fun_decl (_ : (any_decl_id * span) option) (decl : fun_decl) :
        unit =
      let { def_id; item_meta; signature; kind; is_global_initializer; body } =
        decl
      in
      let decl_span_info = Some (IdFun def_id, item_meta.span) in
      self#visit_fun_decl_id decl_span_info def_id;
      self#visit_item_meta decl_span_info item_meta;
      self#visit_fun_sig decl_span_info signature;
      self#visit_item_kind decl_span_info kind;
      self#visit_option self#visit_global_decl_id decl_span_info
        is_global_initializer;
      self#visit_option self#visit_expr_body decl_span_info body

    method! visit_global_decl (_ : (any_decl_id * span) option)
        (decl : global_decl) =
      let decl_span_info = Some (IdGlobal decl.def_id, decl.item_meta.span) in
      super#visit_global_decl decl_span_info decl

    method! visit_trait_decl (_ : (any_decl_id * span) option)
        (decl : trait_decl) =
      let decl_span_info =
        Some (IdTraitDecl decl.def_id, decl.item_meta.span)
      in
      super#visit_trait_decl decl_span_info decl

    method! visit_trait_impl (_ : (any_decl_id * span) option)
        (decl : trait_impl) =
      let decl_span_info =
        Some (IdTraitImpl decl.def_id, decl.item_meta.span)
      in
      super#visit_trait_impl decl_span_info decl

    method visit_declaration_group
        (decl_span_info : (any_decl_id * span) option) (g : declaration_group) :
        unit =
      match g with
      | TypeGroup g -> self#visit_type_declaration_group decl_span_info g
      | FunGroup g -> self#visit_fun_declaration_group decl_span_info g
      | GlobalGroup g -> self#visit_global_declaration_group decl_span_info g
      | TraitDeclGroup g -> self#visit_trait_declaration_group decl_span_info g
      | TraitImplGroup g -> self#visit_trait_impl_group decl_span_info g
      | MixedGroup g -> self#visit_mixed_declaration_group decl_span_info g

    method visit_type_declaration_group
        (decl_span_info : (any_decl_id * span) option)
        (g : type_declaration_group) : unit =
      let ids = g_declaration_group_to_list g in
      List.iter (self#visit_type_decl_id decl_span_info) ids

    method visit_fun_declaration_group
        (decl_span_info : (any_decl_id * span) option)
        (g : fun_declaration_group) : unit =
      let ids = g_declaration_group_to_list g in
      List.iter (self#visit_fun_decl_id decl_span_info) ids

    method visit_global_declaration_group
        (decl_span_info : (any_decl_id * span) option)
        (g : global_declaration_group) : unit =
      let ids = g_declaration_group_to_list g in
      List.iter (self#visit_global_decl_id decl_span_info) ids

    method visit_trait_declaration_group
        (decl_span_info : (any_decl_id * span) option)
        (g : trait_declaration_group) : unit =
      let ids = g_declaration_group_to_list g in
      List.iter (self#visit_trait_decl_id decl_span_info) ids

    method visit_trait_impl_group (decl_span_info : (any_decl_id * span) option)
        (g : trait_impl_group) : unit =
      let ids = g_declaration_group_to_list g in
      List.iter (self#visit_trait_impl_id decl_span_info) ids

    method visit_mixed_declaration_group
        (decl_span_info : (any_decl_id * span) option)
        (g : mixed_declaration_group) : unit =
      let ids = g_declaration_group_to_list g in
      List.iter (self#visit_any_decl_id decl_span_info) ids

    method visit_cli_options (decl_span_info : (any_decl_id * span) option)
        (option : cli_options) : unit =
      ()

    method visit_crate (decl_span_info : (any_decl_id * span) option)
        (crate : crate) : unit =
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
      } =
        crate
      in
      self#visit_string decl_span_info name;
      self#visit_cli_options decl_span_info options;
      List.iter (self#visit_declaration_group decl_span_info) declarations;
      TypeDeclId.Map.iter
        (fun _ -> self#visit_type_decl decl_span_info)
        type_decls;
      FunDeclId.Map.iter (fun _ -> self#visit_fun_decl decl_span_info) fun_decls;
      GlobalDeclId.Map.iter
        (fun _ -> self#visit_global_decl decl_span_info)
        global_decls;
      TraitDeclId.Map.iter
        (fun _ -> self#visit_trait_decl decl_span_info)
        trait_decls;
      TraitImplId.Map.iter
        (fun _ -> self#visit_trait_impl decl_span_info)
        trait_impls
  end

(** For error reporting: compute which local definitions (transitively) depend
    on a set of external definitions. This allows us to pinpoint to the user
    which parts of the code are responsible for an error stemming from a
    dependency. *)
let find_local_transitive_dep (m : crate) (marked_externals : AnyDeclIdSet.t) :
    span list =
  (* Compute the edges from: (decl_id, span) to (decl_id) *)
  let edges = ref [] in
  let visitor =
    object
      inherit [_] iter_crate_with_span

      method! visit_type_decl_id decl_span_info id =
        Option.iter
          (fun info -> edges := (info, IdType id) :: !edges)
          decl_span_info

      method! visit_fun_decl_id decl_span_info id =
        Option.iter
          (fun info -> edges := (info, IdFun id) :: !edges)
          decl_span_info

      method! visit_global_decl_id decl_span_info id =
        Option.iter
          (fun info -> edges := (info, IdGlobal id) :: !edges)
          decl_span_info

      method! visit_trait_decl_id decl_span_info id =
        Option.iter
          (fun info -> edges := (info, IdTraitDecl id) :: !edges)
          decl_span_info

      method! visit_trait_impl_id decl_span_info id =
        Option.iter
          (fun info -> edges := (info, IdTraitImpl id) :: !edges)
          decl_span_info
    end
  in
  (* Visit the crate *)
  visitor#visit_crate None m;
  (* We're using a union-find data-structure.

     All external dependencies which are in the set [external] or which
     transitively depend on declarations in this set are put in the same
     equivalence class.
  *)
  let ids =
    List.map (fun id -> IdType id) (TypeDeclId.Map.keys m.type_decls)
    @ List.map (fun id -> IdFun id) (FunDeclId.Map.keys m.fun_decls)
    @ List.map (fun id -> IdGlobal id) (GlobalDeclId.Map.keys m.global_decls)
    @ List.map (fun id -> IdTraitDecl id) (TraitDeclId.Map.keys m.trait_decls)
    @ List.map (fun id -> IdTraitImpl id) (TraitImplId.Map.keys m.trait_impls)
  in
  let uf_store = UF.new_store () in
  let external_ids =
    AnyDeclIdMap.of_list
      (List.filter_map
         (fun id ->
           let meta = crate_get_item_meta m id in
           match meta with
           | None -> None
           | Some meta ->
               if meta.is_local then None else Some (id, UF.make uf_store id))
         ids)
  in
  (* Merge the classes of the marked externals *)
  let marked_class =
    match AnyDeclIdSet.elements marked_externals with
    | id0 :: ids ->
        let c0 = AnyDeclIdMap.find id0 external_ids in
        List.iter
          (fun id ->
            let c = AnyDeclIdMap.find id external_ids in
            let _ = UF.union uf_store c0 c in
            ())
          ids;
        c0
    | _ -> raise (Failure "Unreachable")
  in
  (* Merge the classes by using the edges *)
  List.iter
    (fun ((id0, _), id1) ->
      match (crate_get_item_meta m id0, crate_get_item_meta m id1) with
      | Some meta0, Some meta1 ->
          if (not meta0.is_local) && not meta1.is_local then
            let c0 = AnyDeclIdMap.find id0 external_ids in
            let c1 = AnyDeclIdMap.find id1 external_ids in
            let _ = UF.union uf_store c0 c1 in
            ()
          else ()
      | _ -> ())
    !edges;
  (* We now compute a map from external id in the set to set of local
     declarations (and spans) which depend on this external id *)
  List.iter
    (fun ((id0, _), id1) ->
      match (crate_get_item_meta m id0, crate_get_item_meta m id1) with
      | Some meta0, Some meta1 ->
          if (not meta0.is_local) && not meta1.is_local then
            let c0 = AnyDeclIdMap.find id0 external_ids in
            let c1 = AnyDeclIdMap.find id1 external_ids in
            let _ = UF.union uf_store c0 c1 in
            ()
          else ()
      | _ -> ())
    !edges;
  (* The spans at which we transitively refer to a marked external definition *)
  let spans = ref SpanSet.empty in
  List.iter
    (fun ((id0, span), id1) ->
      match (crate_get_item_meta m id0, crate_get_item_meta m id1) with
      | Some meta0, Some meta1 ->
          if meta0.is_local && not meta1.is_local then
            let c1 = AnyDeclIdMap.find id1 external_ids in
            if UF.eq uf_store marked_class c1 then
              spans := SpanSet.add span !spans
            else ()
      | _ -> ())
    !edges;
  (* Return the spans *)
  SpanSet.elements !spans
