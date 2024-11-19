module String = struct
  include String

  let hash = Hashtbl.hash

  module Map = Map.Make (String)
end

module List = struct
  include List

  let equal p l l' =
    List.compare_lengths l l' = 0 && List.for_all2 p l l'

  let rec last l =
    match l with
    | [] -> failwith "last"
    | [item] -> item
    | _ :: tl -> last tl

  let rec filter_map f l =
    match l with
    | [] -> []
    | hd :: tl ->
        match f hd with
        | None -> filter_map f tl
        | Some hd' -> hd' :: filter_map f tl
end

module Name = struct
  type t = string Location.loc

  let equal (n : t) (n' : t) =
    n.txt = n'.txt
end

module Pair = struct
  type ('a, 'b) t = 'a * 'b

  let map_snd f (x, y) =
    (x, f y)

  let with_snd f (_x, y) =
    f y

  module Hashed (X : Hashtbl.HashedType) (Y : Hashtbl.HashedType) = struct
    type t = X.t * Y.t

    let equal (x, y) (x', y') =
      X.equal x x' && Y.equal y y'

    let hash (x, y) =
      Hashtbl.hash (X.hash x, Y.hash y)
  end
end

type isomorphism_kind = Ignore_attributes | Attributes_equal

let is_attribute_isomorphic kind (a : Parsetree.attribute)
    (b : Parsetree.attribute) =
  Name.equal a.attr_name b.attr_name

let is_attributes_isomorphic kind (a : Parsetree.attributes)
    (a' : Parsetree.attributes) =
  match kind with
  | Ignore_attributes -> true
  | Attributes_equal ->
      List.equal (is_attribute_isomorphic kind) a a'

let rec is_core_type_isomorphic kind (t : Parsetree.core_type)
    (t' : Parsetree.core_type) =
  is_attributes_isomorphic kind t.ptyp_attributes t'.ptyp_attributes &&
  match t.ptyp_desc, t'.ptyp_desc with
  | Ptyp_any, Ptyp_any -> true
  | Ptyp_var v, Ptyp_var v' -> kind = Ignore_attributes || v = v'
  | Ptyp_arrow (l, u, v), Ptyp_arrow (l', u', v') ->
      l = l' &&
      is_core_type_isomorphic kind u u' &&
      is_core_type_isomorphic kind v v'
  | Ptyp_tuple l, Ptyp_tuple l' ->
      List.equal (is_core_type_isomorphic kind) l l'
  | Ptyp_constr (c, args), Ptyp_constr (c', args') ->
      let eq_c, args, args' =
        match kind, (c.txt, args), (c'.txt, args') with
        | Ignore_attributes,
          (Lident "format6", [a; b; c; _; _; f] |
           Lident "format4", [a; b; c; f]),
          (Lident "format6", [a'; b'; c'; _; _; f'] |
           Lident "format4", [a'; b'; c'; f']) ->
          true, [a; b; c; f], [a'; b'; c'; f']
        | _ -> c.txt = c'.txt, args, args' in
      eq_c && List.equal (is_core_type_isomorphic kind) args args'
  | Ptyp_alias (c, v), Ptyp_alias (c', v') ->
      is_core_type_isomorphic kind c c' && v = v'
  | Ptyp_object (l, flag), Ptyp_object (l', flag') ->
      List.equal (is_object_field_isomorphic kind) l l' && flag = flag'
  | Ptyp_poly (l, t), Ptyp_poly (l', t') ->
      List.equal Name.equal l l' && is_core_type_isomorphic kind t t'
  | Ptyp_variant _, _ ->
      Format.fprintf Format.err_formatter "Unknown isomorphism variant: %a@." Pprintast.core_type t;
      false
  | _ ->
      Format.fprintf Format.err_formatter "Unknown isomorphism: %a@." Pprintast.core_type t;
      false

and is_object_field_isomorphic kind (f : Parsetree.object_field)
    (f' : Parsetree.object_field) =
  is_attributes_isomorphic kind f.pof_attributes f'.pof_attributes &&
  match f.pof_desc, f'.pof_desc with
  | Otag (l, t), Otag (l', t') ->
      l.txt = l'.txt &&
      is_core_type_isomorphic kind t t'
  | Oinherit t, Oinherit t' ->
      is_core_type_isomorphic kind t t'
  | _ -> false

let is_label_declaration_isomorphic kind (l : Parsetree.label_declaration)
    (l' : Parsetree.label_declaration) =
  Name.equal l.pld_name l'.pld_name &&
  l.pld_mutable = l'.pld_mutable &&
  is_core_type_isomorphic kind l.pld_type l'.pld_type

let is_constructor_arguments_isomorphic kind
    (a : Parsetree.constructor_arguments)
    (a' : Parsetree.constructor_arguments) =
  match a, a' with
  | Pcstr_tuple l, Pcstr_tuple l' ->
      List.equal (is_core_type_isomorphic kind) l l'
  | Pcstr_record l, Pcstr_record l' ->
      List.equal (is_label_declaration_isomorphic kind) l l'
  | _ -> false

let is_constructor_declaration_isomorphic kind
    (c : Parsetree.constructor_declaration)
    (c' : Parsetree.constructor_declaration) =
  Name.equal c.pcd_name c'.pcd_name &&
  is_attributes_isomorphic kind c.pcd_attributes c'.pcd_attributes &&
  is_constructor_arguments_isomorphic kind c.pcd_args c'.pcd_args &&
  Interface_tools.Option.equal (is_core_type_isomorphic kind)
    c.pcd_res c'.pcd_res

let no_self_manifest (d : Parsetree.type_declaration) =
  match d.ptype_manifest with
  | Some ({ ptyp_desc = Ptyp_constr ({ txt = Lident self }, _args)})
    when self = d.ptype_name.txt -> None
  | manifest -> manifest

let is_type_param_isomorphic kind (ty, (v, i)) (ty', (v', i')) =
  is_core_type_isomorphic kind ty ty' &&
  v = v' (*&&
  i = i'*)

let is_type_params_isomorphic kind params params' =
  List.equal (is_type_param_isomorphic kind) params params'

let is_type_declaration_isomorphic kind (d : Parsetree.type_declaration)
    (d' : Parsetree.type_declaration) =
  Name.equal d.ptype_name d'.ptype_name &&
  is_attributes_isomorphic kind d.ptype_attributes d'.ptype_attributes &&
  is_type_params_isomorphic kind d.ptype_params d'.ptype_params &&
  Interface_tools.Option.equal (is_core_type_isomorphic kind)
    (no_self_manifest d) (no_self_manifest d') &&
  match d.ptype_kind, d'.ptype_kind with
  | Ptype_abstract, Ptype_abstract
  | Ptype_open, Ptype_open ->
      true
  | Ptype_variant c, Ptype_variant c' ->
      List.equal (is_constructor_declaration_isomorphic kind) c c'
  | Ptype_record r, Ptype_record r' ->
      List.equal (is_label_declaration_isomorphic kind) r r'
  | _ -> false

module Type_declaration_block = struct
  type t = {
      rec_flag : Asttypes.rec_flag;
      type_decl : Parsetree.type_declaration list;
    }

  let get_first_type_name b =
    match b.type_decl with
    | [] -> failwith "empty type decl"
    | type_decl :: _ -> type_decl.ptype_name.txt

  let is_isomorphic kind b b' =
    (kind == Ignore_attributes || b.rec_flag = b'.rec_flag) &&
    List.equal (is_type_declaration_isomorphic kind) b.type_decl b'.type_decl
end

module Signature = struct
  type t = {
      values : Parsetree.value_description String.Map.t;
      types : Type_declaration_block.t String.Map.t;
      module_types : Parsetree.module_type_declaration String.Map.t;
      modules : Parsetree.module_declaration String.Map.t;
    }

  let empty = {
    values = String.Map.empty;
    types = String.Map.empty;
    module_types = String.Map.empty;
    modules = String.Map.empty;
  }

  let add_value (value_desc : Parsetree.value_description) s =
    let values = String.Map.add value_desc.pval_name.txt value_desc s.values in
    { s with values }

  let add_type_declaration (type_decl : Parsetree.type_declaration) block s =
    let types = String.Map.add type_decl.ptype_name.txt block s.types in
    { s with types }

  let add_type_declaration_block (block : Type_declaration_block.t) s =
    block.type_decl |>
      List.fold_left (fun s decl -> add_type_declaration decl block s) s

  let add_module_type_declaration
      (module_type_declaration : Parsetree.module_type_declaration)
      s =
    let module_types =
      String.Map.add module_type_declaration.pmtd_name.txt
        module_type_declaration s.module_types in
    { s with module_types }

  let add_module_declaration (module_declaration : Parsetree.module_declaration)
      s =
    let modules =
      String.Map.add (Option.get module_declaration.pmd_name.txt)
        module_declaration
        s.modules in
    { s with modules }

  let add_item (item : Parsetree.signature_item) s =
    match item.psig_desc with
    | Psig_value value_desc -> add_value value_desc s
    | Psig_type (_rec_flag, type_decl) ->
        add_type_declaration_block { rec_flag = Recursive; type_decl } s
    | Psig_modtype module_type_declaration ->
        add_module_type_declaration module_type_declaration s
    | Psig_module module_declaration ->
        add_module_declaration module_declaration s
    | _ -> s

  let of_parsetree s =
    s |> List.fold_left (fun s item -> add_item item s) empty
end

let lex_file filename parser =
  let in_channel = filename |> open_in in
  Fun.protect
    ~finally:(fun () -> in_channel |> close_in)
    (fun () ->
      let buf = in_channel |> Lexing.from_channel in
      Lexing.set_filename buf filename;
      buf |> parser)

module type Type = sig
  type t
end

module SignaturesTable =
  Hashtbl.Make (Pair.Hashed (Interface_tools.Version) (String))

let signatures = SignaturesTable.create 17

let loc_of_txt txt : 'a Location.loc =
  { txt; loc = Location.none }

let core_type_of_desc ptyp_desc : Parsetree.core_type =
  Ast_helper.Typ.mk ptyp_desc

let module_type_of_desc pmty_desc : Parsetree.module_type =
  { pmty_desc; pmty_loc = Location.none; pmty_attributes = [] }

let module_type_of_signature signature =
  module_type_of_desc (Pmty_signature signature)

let signature_of_module_type (modtype : Parsetree.module_type) =
  match modtype.pmty_desc with
  | Pmty_signature s -> s
  | _ -> failwith "signature_of_module_type"

let module_type_of_name version module_name =
  let root_module, submodule_path =
    match Longident.flatten module_name with
    | root_module :: submodule_path -> root_module, submodule_path
    | [] -> assert false in
  let root_signature =
    try
      prerr_endline "in cache";
      SignaturesTable.find signatures (version, root_module)
    with Not_found ->
      let root_module_mli =
        Printf.sprintf "../interfaces/%s/%s.mli"
          (Interface_tools.Version.to_string ~include_patch:false version)
          (String.uncapitalize_ascii root_module) in
      prerr_endline root_module_mli;
      let root_signature =
        match lex_file root_module_mli Parse.interface with
        | root_signature -> root_signature
        | exception (Sys_error _) -> [] in
      if root_signature = [] then
        prerr_endline "Parsed an empty signature!";
      SignaturesTable.add signatures (version, root_module) root_signature;
      root_signature in
  let rec lookup_submodule signature hd tl =
    Format.eprintf "Looking for submodule %s@." hd;
    if signature = [] then
      prerr_endline "Empty signature!";
    let module_type = Option.get (
      signature |> List.find_map (
      fun (signature_item : Parsetree.signature_item) ->
        match signature_item.psig_desc with
        | Psig_module module_declaration
          when Option.get module_declaration.pmd_name.txt = hd ->
            Some module_declaration.pmd_type
        | Psig_module module_declaration ->
            prerr_endline (Option.get module_declaration.pmd_name.txt);
            None
        | _ ->
            Format.eprintf "%a@."
              Pprintast.signature [signature_item];
            None)) in
    match tl with
    | [] -> module_type
    | hd :: tl ->
        let signature = signature_of_module_type module_type in
        lookup_submodule signature hd tl in
  match submodule_path with
  | [] -> module_type_of_signature root_signature
  | hd :: tl ->
      lookup_submodule root_signature hd tl

module PolymorphicHash (T : Type) = struct
  type t = T.t

  let equal = ( = )

  let hash = Hashtbl.hash
end

module ExternalsTable =
  Hashtbl.Make
    (Pair.Hashed (PolymorphicHash (Longident)) (Interface_tools.Version))

let externals_table = ExternalsTable.create 17

module StringHashtbl = Hashtbl.Make (String)

let normalize_prim version prim =
  if
    Interface_tools.Version.compare version
      { major = 3; minor = 8; patch = 0 } >= 0
  then
    prim
  else
    match prim with
    | "sys_file_exists" -> "caml_sys_file_exists"
    | "sys_is_directory" -> "caml_sys_is_directory"
    | "sys_remove" -> "caml_sys_remove"
    | "sys_rename" -> "caml_sys_rename"
    | "sys_getenv" -> "caml_sys_getenv"
    | "sys_system_command" -> "caml_sys_system_command"
    | "sys_time_unboxed" -> "caml_sys_time_unboxed"
    | "sys_chdir" -> "caml_sys_chdir"
    | "sys_getcwd" -> "caml_sys_getcwd"
    | "sys_read_directory" -> "caml_sys_read_directory"
    | "runtime_variant" -> "caml_runtime_variant"
    | "runtime_parameters" -> "caml_runtime_parameters"
    | "install_signal_handler" -> "caml_install_signal_handler"
    | "output_value_to_bytes" -> "caml_output_value_to_bytes"
    | "output_value_to_string" -> "caml_output_value_to_string"
    | "power_float" -> "caml_power_float"
    | "sqrt_float" -> "caml_sqrt_float"
    | "exp_float" -> "caml_exp_float"
    | "log_float" -> "caml_log_float"
    | "log10_float" -> "caml_log10_float"
    | "expm1" -> "caml_expm1"
    | "log1p" -> "caml_log1p"
    | "cos_float" -> "caml_cos_float"
    | "sin_float" -> "caml_sin_float"
    | "tan_float" -> "caml_tan_float"
    | "acos_float" -> "caml_acos_float"
    | "asin_float" -> "caml_asin_float"
    | "atan_float" -> "caml_atan_float"
    | "atan2_float" -> "caml_atan2_float"
    | "hypot" -> "caml_hypot"
    | "cosh_float" -> "caml_cosh_float"
    | "sinh_float" -> "caml_sinh_float"
    | "tanh_float" -> "caml_tanh_float"
    | "ceil_float" -> "caml_ceil_float"
    | "floor_float" -> "caml_floor_float"
    | "copysign" -> "caml_copysign"
    | "fmod_float" -> "caml_fmod_float"
    | "frexp_float" -> "caml_frexp_float"
    | "ldexp_float_unboxed" -> "caml_ldexp_float_unboxed"
    | "modf_float" -> "caml_modf_float"
    | "classify_float" -> "caml_classify_float"
    | "classify_float_unboxed" -> "caml_classify_float_unboxed"
    | "int_of_string" -> "caml_int_of_string"
    | "float_of_string" -> "caml_float_of_string"
    | "create_bytes" -> "caml_create_bytes"
    | "blit_bytes" -> "caml_blit_bytes"
    | "fill_bytes" -> "caml_fill_bytes"
    | "nativeint_of_float" -> "caml_nativeint_of_float"
    | "nativeint_of_float_unboxed" -> "caml_nativeint_of_float_unboxed"
    | "nativeint_to_float" -> "caml_nativeint_to_float"
    | "nativeint_to_float_unboxed" -> "caml_nativeint_to_float_unboxed"
    | "nativeint_of_string" -> "caml_nativeint_of_string"
    | "nativeint_format" -> "caml_nativeint_format"
    | "int32_of_float_unboxed" -> "caml_int32_of_float_unboxed"
    | "int32_to_float_unboxed" -> "caml_int32_to_float_unboxed"
    | "int32_of_string" -> "caml_int32_of_string"
    | "int32_bits_of_float" -> "caml_int32_bits_of_float"
    | "int32_bits_of_float_unboxed" -> "caml_int32_bits_of_float_unboxed"
    | "int32_float_of_bits" -> "caml_int32_float_of_bits"
    | "int32_float_of_bits_unboxed" -> "caml_int32_float_of_bits_unboxed"
    | "int32_format" -> "caml_int32_format"
    | "make_vect" -> "caml_make_vect"
    | "make_float_vect" -> "caml_make_float_vect"
    | "floatarray_create" -> "caml_floatarray_create"
    | "obj_tag" -> "caml_obj_tag"
    | "obj_reachable_words" -> "caml_obj_reachable_words"
    | "obj_set_tag" -> "caml_obj_set_tag"
    | "obj_block" -> "caml_obj_block"
    | "obj_dup" -> "caml_obj_dup"
    | "obj_truncate" -> "caml_obj_truncate"
    | "obj_add_offset" -> "caml_obj_add_offset"
    | "gc_stat" -> "caml_gc_stat"
    | "gc_quick_stat" -> "caml_gc_quick_stat"
    | "gc_counters" -> "caml_gc_counters"
    | "gc_minor_words" -> "caml_gc_minor_words"
    | "gc_minor_words_unboxed" -> "caml_gc_minor_words_unboxed"
    | "gc_get" -> "caml_gc_get"
    | "gc_set" -> "caml_gc_set"
    | "gc_minor" -> "caml_gc_minor"
    | "gc_major_slice" -> "caml_gc_major_slice"
    | "gc_major" -> "caml_gc_major"
    | "gc_full_major" -> "caml_gc_full_major"
    | "gc_compaction" -> "caml_gc_compaction"
    | "get_minor_free" -> "caml_get_minor_free"
    | "get_major_bucket" -> "caml_get_major_bucket"
    | "get_major_credit" -> "caml_get_major_credit"
    | "gc_huge_fallback_count" -> "caml_gc_huge_fallback_count"
    | "create_string" -> "caml_create_string"
    | "blit_string" -> "caml_blit_string"
    | "fill_string" -> "caml_fill_string"
    | "int64_of_float_unboxed" -> "caml_int64_of_float_unboxed"
    | "int64_to_float_unboxed" -> "caml_int64_to_float_unboxed"
    | "int64_of_string" -> "caml_int64_of_string"
    | "int64_bits_of_float" -> "caml_int64_bits_of_float"
    | "int64_bits_of_float_unboxed" -> "caml_int64_bits_of_float_unboxed"
    | "int64_float_of_bits" -> "caml_int64_float_of_bits"
    | "int64_float_of_bits_unboxed" -> "caml_int64_float_of_bits_unboxed"
    | "int64_format" -> "caml_int64_format"
    | "md5_chan" -> "caml_md5_chan"
    | prim -> prim



let get_externals module_name version =
  try
    ExternalsTable.find externals_table (module_name, version)
  with Not_found ->
    let modtype = module_type_of_name version module_name in
    let externals = StringHashtbl.create 17 in
    let add_external (item : Parsetree.signature_item) =
      match item.psig_desc with
      | Psig_value ({ pval_prim = prim :: _ } as value_description) ->
          StringHashtbl.add externals (normalize_prim version prim)
            value_description
      | _ -> () in
    modtype |> signature_of_module_type |> List.iter add_external;
    ExternalsTable.add externals_table (module_name, version) externals;
    externals

let string_of_longident longident =
  String.concat "." (Longident.flatten longident)

let qualify_type_decl ~module_name (type_decl : Parsetree.type_declaration) =
  match type_decl.ptype_private with
  | Private ->
      { type_decl with ptype_private = Public; ptype_manifest =
        Some (Ast_helper.Typ.constr
          { txt = Longident.Ldot (module_name, type_decl.ptype_name.txt);
            loc = type_decl.ptype_name.loc }
          (List.map fst type_decl.ptype_params)) }
  | Public ->
      let ptype_manifest =
        type_decl.ptype_manifest |>
        Option.map @@ fun (ty : Parsetree.core_type) ->
          match ty.ptyp_desc with
          | Ptyp_constr ({ txt = Lident "fpclass"; loc }, []) ->
              let txt = Longident.Ldot (Lident "Stdlib", "fpclass") in
              let ptyp_desc =
                Parsetree.Ptyp_constr ({ Location.txt; loc }, []) in
              { ty with ptyp_desc }
          | Ptyp_constr ({ txt = Lident ident; loc }, args)
            when ident <> "char" && ident <> "string" && ident <> "lazy_t"
                && ident <> "nativeint" && ident <> "int32" && ident <> "int64"
                && ident <> "format6" && ident <> "format4" && ident <> "bytes"
                && ident <> "float" && ident <> "result" && ident <> "option"
                 && ident <> "list" && ident <> "bool" && ident <> "array"
            && ident <> "exn" && ident <> "int" && ident <> "unit"
            && ident <> "in_channel" && ident <> "out_channel" ->
              let txt = Longident.Ldot (module_name, ident) in
              let ptyp_desc =
                Parsetree.Ptyp_constr ({ Location.txt; loc }, args) in
              { ty with ptyp_desc }
          | _ -> ty in
      { type_decl with ptype_manifest }

let format_signature_item ~module_name ~signatures formatter
    (item : Parsetree.signature_item) =
  match item.psig_desc with
  | Psig_type (rec_flag, type_decl) ->
      let item =
        { item with psig_desc = Parsetree.Psig_type (rec_flag, type_decl) } in
      Format.fprintf formatter "%a@." Pprintast.signature [item]
  | Psig_value value_desc ->
      ()
  | _ ->
      ()

let rec compat_core_type ~module_name (core_type : Parsetree.core_type) =
  let core_type = { core_type with ptyp_attributes = [] } in
  match core_type.ptyp_desc with
  | Ptyp_arrow (label, left, right) ->
      let ptyp_desc =
        Parsetree.Ptyp_arrow
          (label, compat_core_type ~module_name left,
           compat_core_type ~module_name right) in
      { core_type with ptyp_desc }
  | Ptyp_tuple args ->
      let args = List.map (compat_core_type ~module_name) args in
      let ptyp_desc =
        Parsetree.Ptyp_tuple args in
      { core_type with ptyp_desc }
  | Ptyp_constr ({ loc; txt = Ldot (Lident "CamlinternalLazy", "t") }, [arg]) ->
      let ptyp_desc =
        Parsetree.Ptyp_constr
          ({ loc; txt = Ldot (Lident "Stdcompat__init", "lazy_t") }, [arg]) in
      { core_type with ptyp_desc }
  | Ptyp_constr ({ loc; txt = Lident "bytes" }, []) ->
      let ptyp_desc =
        Parsetree.Ptyp_constr
          ({ loc; txt = Ldot (Lident "Stdcompat__init", "bytes") }, []) in
      { core_type with ptyp_desc }
  | Ptyp_constr ({ loc; txt = Lident "floatarray" }, []) ->
      let ptyp_desc =
        Parsetree.Ptyp_constr
          ({ loc; txt = Ldot (Lident "Stdcompat__init", "floatarray") }, []) in
      { core_type with ptyp_desc }
  | Ptyp_constr ({ loc; txt = Lident "result" }, [v; e]) ->
      let v = compat_core_type ~module_name v in
      let e = compat_core_type ~module_name e in
      let ptyp_desc =
        Parsetree.Ptyp_constr
          ({ loc; txt = Ldot (Lident "Stdcompat__pervasives", "result") },
           [v; e]) in
      { core_type with ptyp_desc }
  | Ptyp_constr ({ loc; txt = Ldot (Lident "Seq", "t") }, [arg]) ->
      let arg = compat_core_type ~module_name arg in
      let ptyp_desc =
        Parsetree.Ptyp_constr
          ({ loc; txt = Ldot (Lident "Stdcompat__seq", "t") },
           [compat_core_type ~module_name arg]) in
      { core_type with ptyp_desc }
  | Ptyp_constr ({ loc; txt = Ldot (Lident "Stdlib", t) }, args) ->
      let args = List.map (compat_core_type ~module_name) args in
      let ptyp_desc =
        Parsetree.Ptyp_constr
          ({ loc; txt = Ldot (Lident "Stdcompat__stdlib", t) },
           args) in
      { core_type with ptyp_desc }
  | Ptyp_constr ({ loc; txt = Ldot (Lident "Uchar", t) }, []) ->
      let ptyp_desc =
        Parsetree.Ptyp_constr
          ({ loc; txt = Ldot (Lident "Stdcompat__uchar", t) }, []) in
      { core_type with ptyp_desc }
  | Ptyp_constr ({ loc; txt = Ldot (Lident "Hashtbl", "statistics") }, []) ->
      let ptyp_desc =
        Parsetree.Ptyp_constr
          ({ loc; txt = Ldot (Lident "Stdcompat__hashtbl_ext", "statistics") },
           []) in
      { core_type with ptyp_desc }
  | Ptyp_constr ({ loc; txt =
      Ldot (Lapply (Ldot (Lident "Hashtbl", "MakeSeeded"),
        Lident "H"), "t") }, args) ->
      let args = List.map (compat_core_type ~module_name) args in
      let ptyp_desc =
        Parsetree.Ptyp_constr
          ({ loc; txt =
             Ldot (Lapply (Ldot (Lident "Stdcompat__hashtbl_ext",
               "MakeSeeded"), Lident "H"), "t") },
           args) in
      { core_type with ptyp_desc }
  | Ptyp_constr ({ loc; txt = Ldot (Lident "Either", "t") }, [a; b]) ->
      let ptyp_desc =
        Parsetree.Ptyp_constr
          ({ loc; txt = Ldot (Lident "Stdcompat__either", "t") }, [a; b]) in
      { core_type with ptyp_desc }
  | Ptyp_constr (constr, args) ->
      let rec remove_module_name (constr : Longident.t) : Longident.t =
        match constr with
        | Ldot (module_name', x) ->
            if module_name = module_name' then
              Lident x
            else
              Ldot (remove_module_name module_name', x)
        | _ -> constr in
      let constr = { constr with txt = remove_module_name constr.txt } in
      let ptyp_desc =
        Parsetree.Ptyp_constr
          (constr,
           List.map (compat_core_type ~module_name) args) in
      { core_type with ptyp_desc }
  | _ -> core_type

let compat_label_declaration ~module_name
    (label_declaration : Parsetree.label_declaration) =
  { label_declaration with
    pld_type = compat_core_type ~module_name label_declaration.pld_type }

let compat_constructor_arguments ~module_name
    (constructor_arguments : Parsetree.constructor_arguments) :
    Parsetree.constructor_arguments =
  match constructor_arguments with
  | Pcstr_tuple args ->
      Pcstr_tuple (args |> List.map (compat_core_type ~module_name))
  | Pcstr_record label_declarations ->
      Pcstr_record (label_declarations |>
      List.map (compat_label_declaration ~module_name))

let compat_constructor_declaration ~module_name
    (constructor_declaration : Parsetree.constructor_declaration) =
  { constructor_declaration with
    pcd_args =
    compat_constructor_arguments ~module_name constructor_declaration.pcd_args;
    pcd_res = constructor_declaration.pcd_res |>
    Interface_tools.Option.map (compat_core_type ~module_name);
  }

let compat_type_kind ~module_name
    (type_kind : Parsetree.type_kind) : Parsetree.type_kind =
  match type_kind with
  | Ptype_variant constructor_declarations ->
      Ptype_variant
        (constructor_declarations |>
        List.map (compat_constructor_declaration ~module_name))
  | Ptype_record label_declarations ->
      Ptype_record
        (label_declarations |> List.map (compat_label_declaration ~module_name))
  | _ -> type_kind

let remove_injectivity ptype_params =
  List.map (fun (ty, (v, _)) -> (ty, (v, Asttypes.NoInjectivity))) ptype_params

let compat_type_declaration ~module_name
    (type_decl : Parsetree.type_declaration) =
  match Longident.Ldot (module_name, type_decl.ptype_name.txt) with
  | Ldot (Lident "Pervasives", "format6") ->
    let ptype_manifest =
      match type_decl.ptype_manifest with
      |  Some (
        { ptyp_desc = Parsetree.Ptyp_constr ({ loc }, args) } as core_type) ->
          let ptyp_desc =
            Parsetree.Ptyp_constr
            ({ loc;
               txt = Ldot (Lident "Stdcompat__init", "format6") }, args) in
          Some { core_type with ptyp_desc }
      | _ -> assert false in
    { type_decl with ptype_manifest }
  | Lident "result" ->
    { type_decl with ptype_manifest =
      Some (core_type_of_desc
        (Ptyp_constr
           (loc_of_txt
              (Longident.Ldot
                 (Lident "Stdcompat__pervasives", "result")), []))) }
  | Ldot (Lident "Hashtbl", "statistics") ->
    { type_decl with ptype_manifest =
      Some (core_type_of_desc
        (Ptyp_constr
           (loc_of_txt
              (Longident.Ldot
                 (Lident "Stdcompat__hashtbl_ext", "statistics")), []))) }
  | Ldot (Lapply (Ldot (Lident "Hashtbl", "MakeSeeded"), Lident "H"), "t") ->
    { type_decl with ptype_manifest =
      Some (core_type_of_desc
        (Ptyp_constr
           (loc_of_txt
              (Longident.Ldot
                 (Lapply
                    (Ldot
                       (Lident "Stdcompat__hashtbl_ext", "MakeSeeded"),
                     Lident "H"), "t")),
            type_decl.ptype_params |> List.map fst))) }
  | _ ->
(*
    match type_decl.ptype_manifest with
    |  Some (
      { ptyp_desc =
        Parsetree.Ptyp_constr ({ loc; txt = Lident "bytes" }, args) }
        as core_type) ->
          let ptyp_desc =
            Parsetree.Ptyp_constr
            ({ loc;
               txt = Ldot (Lident "Stdcompat__init", "bytes") }, args) in
          let ptype_manifest = Some { core_type with ptyp_desc } in
          { type_decl with ptype_manifest }
    | _ ->*)
    let ptype_params = remove_injectivity type_decl.ptype_params in
    let ptype_manifest =
      match type_decl.ptype_manifest with
      | None -> None
      | Some { ptyp_desc = Parsetree.Ptyp_constr ({ txt = Lident name }, _) }
        when name = type_decl.ptype_name.txt -> None
      | Some { ptyp_desc =
          Parsetree.Ptyp_constr
            ({ txt = Ldot (module_name', name) }, _) }
        when name = type_decl.ptype_name.txt && module_name = module_name' ->
          None
      |  Some (
        { ptyp_desc =
          Parsetree.Ptyp_constr ({ loc; txt = Lident "bytes" }, args) }
        as core_type) ->
          let ptyp_desc =
            Parsetree.Ptyp_constr
            ({ loc;
               txt = Ldot (Lident "Stdcompat__init", "bytes") }, args) in
          Some { core_type with ptyp_desc }
      | Some ptype_manifest ->
          Some (compat_core_type ~module_name ptype_manifest) in
    { type_decl with
      ptype_params;
      ptype_manifest;
      ptype_kind = compat_type_kind ~module_name type_decl.ptype_kind
    }

let compat_prim ~version prim =
  match prim with
  | ["caml_create_string"] ->
      if
        Interface_tools.Version.compare version
          { major = 3; minor = 8; patch = 0 } >= 0 then
        ["caml_create_string"]
      else
        ["create_string"]
  | ["%string_safe_set"]
  | ["%string_unsafe_set"]
  | ["%identity"] -> prim
  | ["caml_blit_string"]
  | ["caml_fill_string"] ->
      if
        Interface_tools.Version.compare version
          { major = 3; minor = 8; patch = 0 } >= 0 then
        prim @ ["noalloc"]
      else
        if prim = ["caml_blit_string"] then ["blit_string"; "noalloc"]
        else ["fill_string"; "noalloc"]
  | ["%raise_notrace"] -> ["%raise"]
  | _ -> []

let compat_value_description ~module_name ~version
    (value_description : Parsetree.value_description) =
  { value_description with
    pval_prim = compat_prim ~version value_description.pval_prim;
    pval_type = compat_core_type ~module_name value_description.pval_type;
    pval_attributes = [];
  }

let rec compat_signature_item ~module_name ~reference_version ~version
    (item : Parsetree.signature_item) =
  match item.psig_desc with
  | Psig_type (rec_flag, type_decl) ->
      let type_decl =
        type_decl |> List.map (compat_type_declaration ~module_name) in
      let item =
        { item with psig_desc = Parsetree.Psig_type (rec_flag, type_decl) } in
      item
  | Psig_value value_desc ->
      let value_desc = compat_value_description ~module_name ~version value_desc in
      { item with psig_desc = Psig_value value_desc}
  | Psig_module module_declaration ->
      let module_name =
        Longident.Ldot (module_name, Option.get module_declaration.pmd_name.txt) in
      let pmd_type =
        module_declaration.pmd_type |>
        compat_module_type ~module_name ~reference_version ~version in
      { item with psig_desc = Psig_module { module_declaration with pmd_type }}
  | Psig_modtype module_type_declaration ->
      let pmtd_type =
        module_type_declaration.pmtd_type |> Interface_tools.Option.map @@
        compat_module_type ~module_name ~reference_version ~version in
      { item with psig_desc =
        Psig_modtype { module_type_declaration with pmtd_type }}
  | _ ->
      item

and compat_module_type ~module_name ~reference_version ~version
    (module_type : Parsetree.module_type) =
  match module_type.pmty_desc with
  | Pmty_ident
      ({ txt = Ldot (Lident "Hashtbl", "SeededHashedType") } as longident) ->
        let longident =
          { longident with txt =
            if module_name = Ldot (Lident "Hashtbl", "MakeSeeded") then
              Longident.Lident "SeededHashedType"
            else
              Longident.Ldot
                (Lident "Stdcompat__hashtbl", "SeededHashedType") } in
        { module_type with pmty_desc = Pmty_ident longident }
  | Pmty_signature signature ->
      let signature =
        List.map
          (compat_signature_item ~module_name ~reference_version ~version)
          signature in
      { module_type with pmty_desc = Pmty_signature signature }
  | Pmty_functor (Named (var, arg), body) ->
      let arg =
        Interface_tools.Option.map
          (compat_module_type ~module_name ~reference_version ~version)
          (Some arg) in
      let module_name =
        Longident.Lapply (module_name, Lident (Option.get var.txt)) in
      let body =
        compat_module_type ~module_name ~reference_version ~version body in
      { module_type with pmty_desc = Pmty_functor (Named (var, Option.get arg), body) }
  | Pmty_alias ident ->
      compat_module_type ~module_name ~reference_version ~version
        (module_type_of_name reference_version ident.txt)
  | _ -> module_type

(*
      Pprintast.signature formatter [item]
*)

let rec make_version_range is_equal versions =
  match versions with
  | [] -> []
  | (real, version, item) :: tail ->
      match make_version_range is_equal tail with
      | (real', version', item') :: tail
        when real == real' && is_equal ~version item ~version' item' ->
          (real', version', item') :: tail
      | tail ->
          (real, version, item) :: tail

let is_prim_isomorphic kind p p' =
  match kind with
  | Ignore_attributes -> true
  | Attributes_equal -> List.equal String.equal p p'

let is_value_description_isomorphic kind
    (value_desc : Parsetree.value_description)
    (value_desc' : Parsetree.value_description) =
  Name.equal value_desc.pval_name value_desc'.pval_name &&
  is_core_type_isomorphic kind value_desc.pval_type value_desc'.pval_type &&
  is_prim_isomorphic kind value_desc.pval_prim value_desc'.pval_prim &&
  is_attributes_isomorphic kind value_desc.pval_attributes
    value_desc'.pval_attributes

let rec is_module_type_desc_isomorphic kind
    ~version (module_type_desc : Parsetree.module_type_desc)
    ~version' (module_type_desc' : Parsetree.module_type_desc) =
  match module_type_desc, module_type_desc' with
  | Pmty_ident ident, Pmty_ident ident' ->
      ident.txt = ident'.txt
  | Pmty_signature s, Pmty_signature s' ->
      List.equal (is_signature_item_isomorphic kind ~version ~version') s s'
  | Pmty_alias ident, Pmty_alias ident' ->
      (kind = Ignore_attributes || ident.txt = ident'.txt) &&
      is_module_type_isomorphic kind
        ~version (module_type_of_name version ident.txt)
        ~version' (module_type_of_name version ident'.txt)
  | (Pmty_alias _, _ | _, Pmty_alias _) when kind = Attributes_equal ->
      false
  | Pmty_alias ident, _ ->
      is_module_type_isomorphic kind
        ~version (module_type_of_name version ident.txt)
        ~version' (module_type_of_desc module_type_desc')
  | _, Pmty_alias ident' ->
      is_module_type_isomorphic kind
        ~version (module_type_of_desc module_type_desc)
        ~version' (module_type_of_name version ident'.txt)
  | Pmty_functor (Named (x, arg), body), Pmty_functor (Named (x', arg'), body') ->
      Option.get x.txt = Option.get x'.txt &&
      Interface_tools.Option.equal
        (is_module_type_isomorphic kind ~version ~version')
        (Some arg) (Some arg') &&
      is_module_type_isomorphic kind ~version body ~version' body'
  | Pmty_with _, Pmty_with _ -> true
  | Pmty_typeof _, Pmty_typeof _ -> true
  | _ -> false

and is_module_type_isomorphic kind
    ~version (module_type : Parsetree.module_type)
    ~version' (module_type' : Parsetree.module_type) =
  is_module_type_desc_isomorphic kind ~version module_type.pmty_desc
    ~version' module_type'.pmty_desc &&
  is_attributes_isomorphic kind module_type.pmty_attributes
    module_type'.pmty_attributes

and is_module_declaration_isomorphic kind
    ~version (module_declaration : Parsetree.module_declaration)
    ~version' (module_declaration' : Parsetree.module_declaration) =
  Option.get module_declaration.pmd_name.txt =
  Option.get module_declaration'.pmd_name.txt &&
  is_module_type_isomorphic kind ~version module_declaration.pmd_type
    ~version' module_declaration'.pmd_type &&
  is_attributes_isomorphic kind module_declaration.pmd_attributes
    module_declaration'.pmd_attributes

and is_module_type_declaration_isomorphic kind
    ~version (module_type_declaration : Parsetree.module_type_declaration)
    ~version'
    (module_type_declaration' : Parsetree.module_type_declaration) =
  Name.equal module_type_declaration.pmtd_name
    module_type_declaration'.pmtd_name &&
  Interface_tools.Option.equal
    (is_module_type_isomorphic kind ~version ~version')
    module_type_declaration.pmtd_type
    module_type_declaration'.pmtd_type &&
  is_attributes_isomorphic kind module_type_declaration.pmtd_attributes
    module_type_declaration'.pmtd_attributes

and is_extension_constructor_isomorphic kind ~version
    (extension_constructor : Parsetree.extension_constructor)
    ~version' (extension_constructor' : Parsetree.extension_constructor) =
  extension_constructor.pext_name.txt = extension_constructor'.pext_name.txt

and is_type_exception_isomorphic kind ~version
    (type_exception : Parsetree.type_exception)
    ~version' (type_exception' : Parsetree.type_exception) =
  is_extension_constructor_isomorphic kind ~version
    type_exception.ptyexn_constructor
    ~version' type_exception'.ptyexn_constructor

and is_signature_item_isomorphic kind
    ~version (item : Parsetree.signature_item)
    ~version' (item' : Parsetree.signature_item) =
  match item.psig_desc, item'.psig_desc with
  | Psig_type (rec_flag, type_decl), Psig_type (rec_flag', type_decl') ->
      let block = { Type_declaration_block.rec_flag; type_decl } in
      let block' =
        { Type_declaration_block.rec_flag = rec_flag';
          type_decl = type_decl' } in
      Type_declaration_block.is_isomorphic kind block block'
  | Psig_value value_desc, Psig_value value_desc' ->
      is_value_description_isomorphic kind value_desc value_desc'
  | Psig_module module_declaration, Psig_module module_declaration' ->
      is_module_declaration_isomorphic kind
        ~version module_declaration
        ~version' module_declaration'
  | Psig_modtype module_type_declaration,
      Psig_modtype module_type_declaration' ->
      is_module_type_declaration_isomorphic kind ~version
          module_type_declaration
        ~version' module_type_declaration'
  | Psig_exception extension_constructor,
        Psig_exception extension_constructor' ->
      is_type_exception_isomorphic kind ~version
            extension_constructor
            ~version' extension_constructor'
  | Psig_typext type_extension,
          Psig_typext type_extension' ->
            true
  | _ ->
     false

let find_prim_opt version pval_name prim =
  let modules : Longident.t list = [Lident "Pervasives"] in
  let modules : Longident.t list =
    if Interface_tools.Version.compare version
        { major = 4; minor = 6; patch = 0 } >= 0 then
      Ldot (Lident "Array", "Floatarray") :: modules
    else
      modules in
  modules |> List.find_map @@
  fun module_name ->
    let externals = get_externals module_name version in
    match StringHashtbl.find_opt externals prim with
    | None -> None
    | Some value_desc' ->
        Some { value_desc' with pval_name }

let rec fake_module_type ~module_name ~reference_version ~version
    (module_type : Parsetree.module_type) =
  match module_type.pmty_desc with
  | Pmty_signature s ->
      let s = s |> List.map @@
        fake_signature_item ~module_name ~reference_version ~version in
      { module_type with pmty_desc = Pmty_signature s }
  | Pmty_functor (Named (var, arg), body) ->
      let arg =
        fake_module_type ~module_name ~reference_version ~version arg in
      let body =
        fake_module_type ~module_name ~reference_version ~version body in
      { module_type with pmty_desc = Pmty_functor (Named (var, arg), body) }
  | _ -> module_type

and fake_signature_item ~module_name ~reference_version ~version
    (item : Parsetree.signature_item) =
  match item.psig_desc with
  | Psig_value ({ pval_prim = prim :: _ } as value_desc) ->
      begin
        match find_prim_opt version value_desc.pval_name prim with
        | None ->
            compat_signature_item ~module_name ~reference_version ~version
              { item with psig_desc =
                Psig_value { value_desc with pval_prim = [] }}
        | Some value_desc -> { item with psig_desc = Psig_value value_desc }
      end
  | _ -> compat_signature_item ~module_name ~reference_version ~version item

let version_signature_item ~reference_version ~module_name ~signatures
    (item : Parsetree.signature_item) =
  match item.psig_desc with
  | Psig_type (_rec_flag, type_decl) ->
      let block = { Type_declaration_block.rec_flag = Recursive; type_decl } in
      let first_type_name = Type_declaration_block.get_first_type_name block in
      Some (signatures |>
        List.map (fun (version, (s : Signature.t)) ->
          let real, type_decl =
            let block' = String.Map.find_opt first_type_name s.types in
            match
              if first_type_name = "lexbuf" then
                block'
              else
              Interface_tools.Option.filter
                (Type_declaration_block.is_isomorphic Ignore_attributes block)
                block'
            with
            | None ->
                false,
                type_decl |> List.map @@ compat_type_declaration ~module_name
            | Some block' ->
                true,
                block'.type_decl |>
                List.map @@ qualify_type_decl ~module_name in
              (real, version,
               { item with psig_desc = Psig_type (Recursive, type_decl) })))
  | Psig_value value_desc ->
      Some (signatures |>
        List.map (fun (version, (s : Signature.t)) ->
          let real, value_desc' =
            match
              Interface_tools.Option.filter
                (fun (real, value_desc') ->
                  is_value_description_isomorphic Ignore_attributes value_desc
                      value_desc') @@
                match String.Map.find_opt value_desc.pval_name.txt s.values with
                | None ->
                    begin
                      match value_desc.pval_prim with
                      | [] -> None
                      | prim :: _ ->
                          match
                            find_prim_opt version value_desc.pval_name prim
                          with
                          | None -> None
                          | Some value_desc' -> Some (false, value_desc')
                    end
                | Some value_desc' -> Some (true, value_desc')
            with
            | Some (real, value_desc') -> real, value_desc'
            | None ->
                false,
                compat_value_description ~module_name ~version value_desc in
          real, version, { item with psig_desc = Psig_value value_desc' }))
  | Psig_module module_declaration ->
      Some (signatures |>
        List.map (fun (version, (s : Signature.t)) ->
          let real, module_declaration' =
            match
              match
                String.Map.find_opt (Option.get module_declaration.pmd_name.txt) s.modules
              with
              | None -> None
              | Some module_declaration' ->
                  if is_module_declaration_isomorphic Ignore_attributes
                      ~version:reference_version module_declaration
                      ~version':version module_declaration' then
                    let module_declaration' =
                      if
                        Interface_tools.Version.compare version
                          { major = 4; minor = 2; patch = 0 } >= 0
                      then
                        { module_declaration with pmd_type = {
                          pmty_desc =
                          Pmty_alias
                            { loc = Location.none; txt =
                              Ldot
                                (module_name,
                                 (Option.get module_declaration.pmd_name.txt)) };
                          pmty_loc = Location.none;
                          pmty_attributes = [] }}
                      else
                        module_declaration' in
                    Some (true, module_declaration')
                  else
                    None
            with
            | None ->
                let pmd_type = fake_module_type
                    ~module_name ~reference_version ~version
                    module_declaration.pmd_type in
                false, { module_declaration with pmd_type }
            | Some (real, module_declaration')  -> real, module_declaration' in
          real, version,
          { item with psig_desc = Psig_module module_declaration' }))
  | Psig_modtype module_type_declaration ->
      Some (signatures |>
        List.map (fun (version, (s : Signature.t)) ->
          let real, module_type_declaration' =
            match
              String.Map.find_opt
                module_type_declaration.pmtd_name.txt s.module_types with
            | Some module_type_declaration' when
                is_module_type_declaration_isomorphic Ignore_attributes
                  ~version:reference_version module_type_declaration
                  ~version':version module_type_declaration' ->
                    true, module_type_declaration'
            | _ ->
                let pmtd_type = module_type_declaration.pmtd_type |>
                  Interface_tools.Option.map @@ compat_module_type ~module_name
                    ~reference_version ~version in
                let module_type_declaration' =
                  { module_type_declaration with pmtd_type } in
                false, module_type_declaration' in
          real, version,
          { item with psig_desc = Psig_modtype module_type_declaration' }))
  | Psig_exception extension_constructor ->
      Some ([true, reference_version, item])
  | _ ->
      None

let rec last_real_version versions =
  match versions with
  | [] | [None, _, _] -> assert false
  | [Some version, _, item]
  | (Some version, _, item) :: (None, _, _) :: _ -> version, item
  | _ :: tail -> last_real_version tail

let compare_versioned_signature versions versions' =
  try
    let version, (item : Parsetree.signature_item) =
      last_real_version versions in
    let version', (item' : Parsetree.signature_item) =
      last_real_version versions' in
    match item.psig_desc, item'.psig_desc with
    | Psig_value _, Psig_value _ ->
        - Interface_tools.Version.compare version version'
    | _, Psig_value _ -> -1
    | Psig_value _, _ -> 1
    | _ -> 0
  with _ ->
    prerr_endline "EMPTY!";
    0

let value_item (item : Parsetree.signature_item) =
  match item.psig_desc with
  | Psig_value _ -> true
  | _ -> false

let rec consume_similar_versions last_real_version version item versions =
  match versions with
  | (real', version', item') :: tail when
        is_signature_item_isomorphic Attributes_equal
          ~version item ~version' item'
        && ((last_real_version <> None) = real' || value_item item) ->
      let last_real_version =
        if real' then Some version'
        else last_real_version in
      consume_similar_versions last_real_version version' item tail
  | tail -> (last_real_version, version, item), tail

let rec gather_similar_versions versions =
  match versions with
  | [] -> []
  | (real, version, item) :: tail ->
      let last_real_version =
        if real then Some version
        else None in
      let head', tail' =
        consume_similar_versions last_real_version version item tail in
      head' :: (gather_similar_versions tail')

let type_of_desc ptyp_desc : Parsetree.core_type =
  Ast_helper.Typ.mk ptyp_desc

let format_block block sub formatter item =
  Format.fprintf formatter "\
    @@BEGIN_%s@@@.\
    %a@.\
    @@END_%s@@@.\
  " block sub item block

let format_block_prefix prefix block sub formatter item =
  format_block (prefix ^ "_" ^ block) sub formatter item

let format_with block = format_block_prefix "WITH" block

let format_without block = format_block_prefix "WITHOUT" block

let format_ver prefix ver =
  format_block_prefix prefix (Interface_tools.Version.to_string ~sep:"_" ver)

let format_before ver = format_ver "BEFORE" ver

let format_from ver = format_ver "FROM" ver

let format_without block sub formatter item =
  format_block ("WITHOUT_" ^ block) sub formatter item

let format_with_without block sub formatter item_with item_without =
  format_with block sub formatter item_with;
  format_without block sub formatter item_without

let format_from_before ver formatter sub_from item_from sub_before item_before =
  format_from ver sub_from formatter item_from;
  format_before ver sub_before formatter item_before

let attributed_decl (decl : Parsetree.type_declaration) =
  decl.ptype_attributes <> []

let gadt_decl (decl : Parsetree.type_declaration) =
  match decl.ptype_kind with
  | Ptype_variant constructors ->
      List.exists (fun (constructor : Parsetree.constructor_declaration) ->
        constructor.pcd_res <> None) constructors
  | _ -> false

let remove_gadt (decl : Parsetree.type_declaration) =
  match decl.ptype_kind with
  | Ptype_variant constructors ->
      let constructors =
        List.map (fun (constructor : Parsetree.constructor_declaration) ->
          { constructor with pcd_res = None }) constructors in
      { decl with ptype_kind = Ptype_variant constructors }
  | _ -> decl

(*
let make_rebind ~module_name (constructor : Parsetree.extension_constructor) =
  { constructor with pext_kind =
    Pext_rebind { loc = Location.none;
      txt = Longident.Ldot (module_name, constructor.pext_name.txt)}}
*)

let has_injective_param (decl : Parsetree.type_declaration) =
  List.exists (fun (_, (_, i)) -> i = Asttypes.Injective) decl.ptype_params

let remove_injective_param (decl : Parsetree.type_declaration) =
  { decl with ptype_params = remove_injectivity decl.ptype_params }

let remove_attributes (decl : Parsetree.type_declaration) =
  { decl with ptype_attributes = [] }

let is_private (decl : Parsetree.type_declaration) =
  decl.ptype_private = Private

let make_public (decl : Parsetree.type_declaration) =
  { decl with ptype_private = Public }

let rec format_sig_type formatter
      ((recursive : Asttypes.rec_flag),
        (decls : Parsetree.type_declaration list)) =
  if List.exists has_injective_param decls then
    begin
      let decls' = List.map remove_injective_param decls in
      format_from_before (Interface_tools.Version.mk 4 12 0) formatter
        Pprintast.signature [Ast_helper.Sig.type_ recursive decls]
        format_sig_type (recursive, decls')
    end
  else if List.exists attributed_decl decls then
    begin
      let decls' = List.map remove_attributes decls in
      format_from_before (Interface_tools.Version.mk 4 02 0) formatter
        Pprintast.signature [Ast_helper.Sig.type_ recursive decls]
        format_sig_type (recursive, decls')
    end
  else if List.exists gadt_decl decls then
    begin
      let decls' = List.map remove_gadt decls in
      format_from_before (Interface_tools.Version.mk 4 00 0) formatter
        Pprintast.signature [Ast_helper.Sig.type_ recursive decls]
        format_sig_type (recursive, decls')
    end
  else if List.exists is_private decls then
    begin
      let decls' = List.map make_public decls in
      format_from_before (Interface_tools.Version.mk 3 11 0) formatter
        Pprintast.signature [Ast_helper.Sig.type_ recursive decls]
        format_sig_type (recursive, decls')
    end
  else
    Pprintast.signature formatter [Ast_helper.Sig.type_ recursive decls]

let rec format_default_item ~module_name formatter
    (item : Parsetree.signature_item) =
  match item.psig_desc with
  | Psig_type (rec_flag, [{ ptype_name = { txt = "result" }} as type_decl]) ->
      let ptyp_desc = Parsetree.Ptyp_constr (
        { txt = Ldot (Lident "Result", "result"); loc = Location.none },
        [type_of_desc (Ptyp_var "a"); type_of_desc (Ptyp_var "b")]) in
      let manifest = type_of_desc ptyp_desc in
      let type_decl' = { type_decl with ptype_manifest = Some manifest } in
      format_with_without "UCHAR_PKG" format_sig_type formatter
        (rec_flag, [type_decl']) (rec_flag, [type_decl])
  | Psig_type (rec_flag, [{ ptype_name = { txt = "t" }} as type_decl])
      when module_name = Longident.Lident "Uchar" ->
      let ptyp_desc = Parsetree.Ptyp_constr (
        { txt = Ldot (Lident "Uchar", "t"); loc = Location.none },
        []) in
      let manifest = type_of_desc ptyp_desc in
      let type_decl' = { type_decl with ptype_manifest = Some manifest } in
      format_with_without "UCHAR_PKG" format_sig_type formatter
        (rec_flag, [type_decl']) (rec_flag, [type_decl])
  | Psig_type (rec_flag, [{ ptype_name = { txt = "t" }} as type_decl])
      when module_name = Longident.Lident "Either" ->
      let ptyp_desc = Parsetree.Ptyp_constr (
        { txt = Ldot (Lident "Stdcompat__init", "either"); loc = Location.none },
        List.map fst type_decl.ptype_params) in
      let manifest = type_of_desc ptyp_desc in
      let type_decl = { type_decl with ptype_manifest = Some manifest } in
      let psig_desc = Parsetree.Psig_type (rec_flag, [type_decl]) in
      let result_item = { item with psig_desc } in
      Pprintast.signature formatter [result_item]
  | Psig_type (_, _)
      when module_name = Longident.Lident "List" || module_name = Longident.Lident "ListLabels" ->
      Format.fprintf formatter "\
@@BEGIN_FROM_4_03_0@@
type 'a t = 'a list =
  | []
  | (::) of 'a * 'a list
@@END_FROM_4_03_0@@
@@BEGIN_BEFORE_4_03_0@@
type 'a t = 'a list
@@END_BEFORE_4_03_0@@"
  | Psig_type (_, _)
      when module_name = Longident.Lident "Seq" ->
      Format.fprintf formatter "\
type 'a t = unit -> 'a node
and 'a node = 'a Stdcompat__init.seq_node =
  | Nil
  | Cons of 'a * 'a t"
  | Psig_type (_, [{ ptype_name = { txt }; ptype_manifest = Some t; ptype_kind = Ptype_open }]) ->
      Format.fprintf formatter "\
@@BEGIN_FROM_4_02_0@@
type %s = %a = ..
@@END_FROM_4_02_0@@
@@BEGIN_BEFORE_4_02_0@@
type %s = %a
@@END_BEFORE_4_02_0@@" txt Pprintast.core_type t txt Pprintast.core_type t
  | Psig_type (_, [{ ptype_name = { txt }; ptype_kind = Ptype_open }]) ->
      Format.fprintf formatter "\
@@BEGIN_FROM_4_02_0@@
type %s = ..
@@END_FROM_4_02_0@@
@@BEGIN_BEFORE_4_02_0@@
type %s
@@END_BEFORE_4_02_0@@" txt txt
  | Psig_type (recursive, decls) ->
      format_sig_type formatter (recursive, decls)
  | Psig_value { pval_name = { txt = "ifprintf"; _ }; _ } when module_name = Lident "Printf" ->
      Format.fprintf formatter "\
@@BEGIN_FROM_4_03_0@@
val ifprintf : 'b -> ('a, 'b, 'c, unit) format4 -> 'a
(** @@since 4.03.0: val ifprintf : 'b -> ('a, 'b, 'c, unit) format4 -> 'a *)
@@END_FROM_4_03_0@@
@@BEGIN_BEFORE_4_03_0@@
@@BEGIN_FROM_3_10_0@@
val ifprintf : 'b -> ('a, 'b, unit) format -> 'a
@@END_FROM_3_10_0@@
@@BEGIN_BEFORE_3_10_0@@
val ifprintf : 'b -> ('a, 'b, 'c, unit) format4 -> 'a
@@END_BEFORE_3_10_0@@
@@END_BEFORE_4_03_0@@"
  | Psig_value { pval_name = { txt = "ikfprintf"; _ }; _ } when module_name = Lident "Printf" ->
      Format.fprintf formatter "\
@@BEGIN_FROM_4_03_0@@
val ikfprintf : ('b -> 'd) -> 'b -> ('a, 'b, 'c, 'd) format4 -> 'a
(** @@since 4.03.0: val ikfprintf : ('b -> 'd) -> 'b -> ('a, 'b, 'c, 'd) format4 -> 'a *)
@@END_FROM_4_03_0@@
@@BEGIN_BEFORE_4_03_0@@
@@BEGIN_FROM_4_01_0@@
val ikfprintf :
    (out_channel -> 'a) ->
      out_channel -> ('b, out_channel, unit, 'a) format4 -> 'b
@@END_FROM_4_01_0@@
@@BEGIN_BEFORE_4_01_0@@
val ikfprintf : ('b -> 'd) -> 'b -> ('a, 'b, 'c, 'd) format4 -> 'a
@@END_BEFORE_4_01_0@@
@@END_BEFORE_4_03_0@@"
  | Psig_module module_declaration when match module_declaration.pmd_type.pmty_desc with Pmty_alias _ -> false | _ -> true ->
      Format.fprintf formatter "@[module %s :@ @[%a@]@]"
        (Option.get module_declaration.pmd_name.txt)
        (format_default_module_type ~module_name) module_declaration.pmd_type
  | Psig_modtype module_type_declaration ->
      Format.fprintf formatter "@[module type %s =@ @[%a@]@]"
        module_type_declaration.pmtd_name.txt
        (format_default_module_type ~module_name)
        (Option.get module_type_declaration.pmtd_type)
(*
  | Psig_typext type_extension ->
      let type_extension = { type_extension with
        ptyext_constructors =
          List.map (make_rebind ~module_name)
            type_extension.ptyext_constructors } in
      Format.fprintf formatter "%a" Pprintast.signature
        [{ item with psig_desc = Psig_typext type_extension }]
  | Psig_exception type_exception ->
      let type_exception = { type_exception with
        ptyexn_constructor = make_rebind ~module_name
            type_exception.ptyexn_constructor } in
      Format.fprintf formatter "%a" Pprintast.signature
        [{ item with psig_desc = Psig_exception type_exception }]
*)
  | _ ->
      Format.fprintf formatter "%a" Pprintast.signature [item]

and format_default_module_type ~module_name formatter
      (module_type : Parsetree.module_type) =
  match module_type.pmty_desc with
  | Pmty_ident ident ->
      Format.fprintf formatter "%s" (string_of_longident ident.txt)
  | Pmty_signature signature ->
      Format.fprintf formatter "@[sig@ %a@ end@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
           (format_default_item ~module_name)) signature
  | Pmty_functor (Named (var, arg), body) ->
      Format.fprintf formatter "@[functor (%s : %a) ->@ %a@]"
        (Option.get var.txt) (format_default_module_type ~module_name) arg
        (format_default_module_type ~module_name) body
  | _ ->
      failwith "Not implemented"

let item_name module_name (item : Parsetree.signature_item) =
  let name =
    match item.psig_desc with
    | Psig_type (rec_flag, type_decl :: _) ->
        type_decl.ptype_name.txt
    | Psig_value value_desc ->
        value_desc.pval_name.txt
    | Psig_module module_declaration ->
        Option.get module_declaration.pmd_name.txt
    | Psig_modtype module_type_declaration ->
        module_type_declaration.pmtd_name.txt
    | Psig_exception extension_constructor ->
        extension_constructor.ptyexn_constructor.pext_name.txt
    | _ -> assert false in
  Printf.sprintf "%s.%s" (string_of_longident module_name) name

let add_self_type_manifest_to_type_decl ~(module_name : Longident.t)
    (type_decl : Parsetree.type_declaration) =
  match type_decl.ptype_manifest with
  | None ->
      let module_name : Longident.t =
        match module_name with
        | Lapply (Ldot (Lident "Hashtbl", "MakeSeeded"), Lident "H") ->
            Lapply (Ldot (Lident "Stdcompat__hashtbl_ext", "MakeSeeded"),
              Lident "H")
        | _ -> module_name in
      { type_decl with ptype_manifest =
        let params = type_decl.ptype_params |> List.map fst in
        Some (Ast_helper.Typ.constr
          ({ loc = Location.none; txt =
            Ldot (module_name, type_decl.ptype_name.txt) }) params)}
  | Some manifest -> type_decl

let rec add_self_type_manifest ~module_name (item : Parsetree.signature_item) =
  match item.psig_desc with
  | Psig_type (rec_flag, type_decl_list) ->
      let type_decl_list = type_decl_list |>
        List.map (add_self_type_manifest_to_type_decl ~module_name) in
      { item with psig_desc = Psig_type (rec_flag, type_decl_list) }
  | Psig_value _ | Psig_modtype _ | Psig_exception _ -> item
  | Psig_module module_declaration ->
      let module_name : Longident.t =
        Ldot (module_name, Option.get module_declaration.pmd_name.txt) in
      { item with psig_desc = Psig_module { module_declaration with
        pmd_type = module_declaration.pmd_type |>
          (add_self_type_manifest_to_module_type ~module_name) }}
(*  | Psig_modtype module_type_declaration ->
      { item with psig_desc = Psig_modtype { module_type_declaration with
        pmtd_type = module_type_declaration.pmtd_type |>
          Option.map (add_self_type_manifest_to_module_type ~module_name) }} *)
  | _ -> assert false

and add_self_type_manifest_to_module_type ~module_name
    (module_type : Parsetree.module_type) =
  match module_type.pmty_desc with
  | Pmty_signature signature ->
      let signature = signature |>
        List.map (add_self_type_manifest ~module_name) in
      { module_type with pmty_desc = Pmty_signature signature }
  | Pmty_functor (Named (var, arg), body) ->
(*
      let arg = arg |> Option.map
        (add_self_type_manifest_to_module_type ~module_name) in
*)
      let module_name : Longident.t =
        Lapply (module_name, Lident (Option.get var.txt)) in
      let body = body |> add_self_type_manifest_to_module_type ~module_name in
      { module_type with pmty_desc = Pmty_functor (Named (var, arg), body) }
  | Pmty_with (ty, cstr) ->
      { module_type with pmty_desc =
        Pmty_with
          (add_self_type_manifest_to_module_type ~module_name ty, cstr) }
  | _ -> module_type

let print_signature_item ~(module_name : Longident.t) real formatter
    (item : Parsetree.signature_item) =
  if real <> None || module_name = Lident "Hashtbl"
   || module_name = Lident "Set"
   || module_name = Lident "Map"
   || module_name = Lident "Weak" then
    let self_item = add_self_type_manifest ~module_name item in
    format_default_item ~module_name formatter self_item
  else
    format_default_item ~module_name formatter item

let format_versioned_signature ~module_name ~version_high ~version_low
    ~reference_version formatter versions =
  match versions with
  | [] ->
      assert false
  | [real, last_version, item] ->
      Format.fprintf formatter "%a@." (print_signature_item ~module_name real)
        item;
      let item_name = item_name module_name item in
      begin
        match real with
        | None -> ()
        | Some real_version ->
            if Interface_tools.Version.equal real_version last_version then
              Format.fprintf formatter "(** Alias for {!%s} *)@.@." item_name
            else
              Format.fprintf formatter "(** @[@since %s:@ %a@] *)@.@."
                (Interface_tools.Version.to_string real_version)
                Pprintast.signature [item]
      end
  | (real, last_version, item) :: next :: tail ->
      format_from last_version (print_signature_item ~module_name real)
        formatter item;
      let rec format_tail version next tail =
        let format formatter tail =
          let (real, last_version, item) = next in
          match tail with
          | [] ->
              Format.fprintf formatter "%a"
                (print_signature_item ~module_name real) item
          | next :: tail ->
              format_from last_version
                (print_signature_item ~module_name real) formatter item;
              format_tail last_version next tail in
        format_before version format formatter tail in
      format_tail last_version next tail;
      let format_doc formatter versions =
        let format_doc_version (real, _version, item) =
          match real with
          | None -> ()
          | Some real_version ->
            Format.fprintf formatter "@[@since %s:@ %a@]@."
              (Interface_tools.Version.to_string real_version)
              Pprintast.signature [item] in
        List.iter format_doc_version versions in
      Format.fprintf formatter "(** @[<v>%a@] *)@.@." format_doc versions

let generate channel module_name versions =
  let module_name = Longident.Lident module_name in
  let signatures =
    versions |> List.map @@ fun version ->
      let version = Interface_tools.Version.of_string version in
      let signature = module_type_of_name version module_name |>
        signature_of_module_type in
      version, signature in
  let reference_version, reference_signature =
    match signatures with
    | first :: _ -> first
    | _ -> failwith "No reference version" in
  let signatures = signatures |> List.map @@ fun (version, s) ->
    version, Signature.of_parsetree s in
  let versioned_signature =
    reference_signature |> List.filter_map @@
      version_signature_item ~reference_version
        ~module_name ~signatures in
  let version_high, _ = List.hd signatures in
  let version_low, _ = List.last signatures in
  let formatter = Format.formatter_of_out_channel channel in
  Format.fprintf formatter "module type S = sig@.";
  versioned_signature |>
    List.map gather_similar_versions |>
    List.sort compare_versioned_signature |>
    List.iter @@
      format_versioned_signature ~module_name
      ~version_high ~version_low
      ~reference_version
      formatter;
  Format.fprintf formatter "end@."
(*
  let mli_filenames = argv |> Array.to_list |> List.tl in
  let signatures = mli_filenames |> List.map read_interface in
  let main_signature, other_signatures =
    match signatures with
    | [] -> failwith "No mli file given"
    | main_signature :: other_signatures ->
        main_signature, other_signatures in
  main_signature |> List.iter @@ format_signature_item Format.std_formatter
*)

let do_module versions module_name =
  print_endline module_name;
  let target = Printf.sprintf "../stdcompat__%s_s.mli.in" (String.lowercase_ascii module_name) in
  print_endline target;
  Out_channel.with_open_text target (fun channel ->
    generate channel module_name versions)

let main _argv =
  let modules =
    ["Atomic";
    "Arg"; "Array"; "ArrayLabels"; "Bool"; "Buffer"; "Bytes"; "BytesLabels"; "Callback"; "Char";
    "Complex"; "Digest"; "Domain"; "Either"; "Ephemeron"; "Filename"; "Float"; "Format"; "Fun"; "Gc"; "Hashtbl"; "Int32"; 
    "Int64"; "Lazy"; "Lexing"; "List"; "ListLabels"; "Map"; "Marshal"; "MoreLabels"; "Nativeint"; "Obj"; "Oo";
    "Option"; "Parsing"; "Printexc"; "Printf"; "Queue"; "Random"; "Result"; "Scanf"; "Seq"; "Set";
    "Stack"; "StdLabels"; "String"; "StringLabels"; "Sys"; "Uchar"; "Weak"; "In_channel"; "Out_channel";
    "Unit"] in
  let versions = ["5.2"; "5.1"; "5.0"; "4.14"; "4.13"; "4.12"; "4.11"; "4.10"; "4.09"; "4.08"; "4.07"; "4.06"; "4.05"; "4.04"; "4.03"; "4.02"; "4.01"; "4.00"; "3.12"; "3.11"; "3.10"; "3.09"; "3.08"; "3.07"] in
  List.iter (do_module versions) modules

let () =
  if not !Sys.interactive then
    try
      Sys.argv |> main
    with e ->
      prerr_endline (Printexc.to_string e);
      Printexc.print_backtrace stderr;
      exit 1
