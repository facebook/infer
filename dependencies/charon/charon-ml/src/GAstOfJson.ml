(** Functions to load (U)LLBC ASTs from json.

    Initially, we used [ppx_derive_yojson] to automate this. However,
    [ppx_derive_yojson] expects formatting to be slightly different from what
    [serde_rs] generates (because it uses [Yojson.Safe.t] and not
    [Yojson.Basic.t]). *)

open Yojson.Basic
open OfJsonBasic
open Identifiers
open Meta
open Values
open Types
open Scalars
open Expressions
open GAst
include Generated_GAstOfJson

let rec maybe_opaque_body_of_json
    (body_of_json : of_json_ctx -> json -> ('body gexpr_body, string) result)
    (ctx : of_json_ctx) (js : json) : ('body gexpr_body option, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Ok", body) ] ->
        let* body = body_of_json ctx body in
        Ok (Some body)
    | `Assoc [ ("Err", `Null) ] -> Ok None
    | _ -> Error "")

(* This is written by hand because the corresponding rust type is not type-generic. *)
and gfun_decl_of_json
    (body_of_json : of_json_ctx -> json -> ('body gexpr_body, string) result)
    (ctx : of_json_ctx) (js : json) : ('body gfun_decl, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc
        [
          ("def_id", def_id);
          ("item_meta", item_meta);
          ("signature", signature);
          ("kind", kind);
          ("is_global_initializer", is_global_initializer);
          ("body", body);
        ] ->
        let* def_id = FunDeclId.id_of_json ctx def_id in
        let* item_meta = item_meta_of_json ctx item_meta in
        let* signature = fun_sig_of_json ctx signature in
        let* kind = item_kind_of_json ctx kind in
        let* is_global_initializer =
          option_of_json global_decl_id_of_json ctx is_global_initializer
        in
        let* body = maybe_opaque_body_of_json body_of_json ctx body in
        Ok { def_id; item_meta; signature; kind; is_global_initializer; body }
    | _ -> Error "")

(** Deserialize a map from file id to file name.

    In the serialized LLBC, the files in the loc spans are refered to by their
    ids, in order to save space. In a functional language like OCaml this is not
    necessary: we thus replace the file ids by the file name themselves in the
    AST. The "id to file" map is thus only used in the deserialization process.
*)
and id_to_file_of_json (js : json) : (of_json_ctx, string) result =
  combine_error_msgs js __FUNCTION__
    ((* The map is stored as a list of pairs (key, value): we deserialize
      * this list then convert it to a map *)
     let* files =
       list_of_json (option_of_json file_of_json) FileId.Map.empty js
     in
     let files_with_ids =
       List.filter_map
         (fun (i, file) ->
           match file with
           | None -> None
           | Some file -> Some (i, file))
         (List.mapi (fun i file -> (FileId.of_int i, file)) files)
     in
     Ok (FileId.Map.of_list files_with_ids))

(* This is written by hand because the corresponding rust type is not type-generic. *)
and gtranslated_crate_of_json
    (body_of_json : of_json_ctx -> json -> ('body gexpr_body, string) result)
    (js : json) : ('body gcrate, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc
        [
          ("crate_name", name);
          ("options", options);
          ("target_information", target_info);
          ("item_names", _);
          ("short_names", _);
          ("files", files);
          ("type_decls", types);
          ("fun_decls", functions);
          ("global_decls", globals);
          ("trait_decls", trait_decls);
          ("trait_impls", trait_impls);
          ("ordered_decls", declarations);
        ] ->
        let* ctx = id_to_file_of_json files in
        let* name = string_of_json ctx name in
        let* options = cli_options_of_json ctx options in
        let* target_information = target_info_of_json ctx target_info in

        let* declarations =
          list_of_json declaration_group_of_json ctx declarations
        in

        let* types =
          vector_of_json type_id_of_json type_decl_of_json ctx types
        in
        let* functions =
          vector_of_json fun_decl_id_of_json
            (gfun_decl_of_json body_of_json)
            ctx functions
        in
        let* globals =
          vector_of_json global_decl_id_of_json global_decl_of_json ctx globals
        in
        let* trait_decls =
          vector_of_json trait_decl_id_of_json trait_decl_of_json ctx
            trait_decls
        in
        let* trait_impls =
          vector_of_json trait_impl_id_of_json trait_impl_of_json ctx
            trait_impls
        in

        let type_decls =
          TypeDeclId.Map.of_list
            (List.map (fun (d : type_decl) -> (d.def_id, d)) types)
        in
        let fun_decls =
          FunDeclId.Map.of_list
            (List.map (fun (d : 'body gfun_decl) -> (d.def_id, d)) functions)
        in
        let global_decls =
          GlobalDeclId.Map.of_list
            (List.map (fun (d : global_decl) -> (d.def_id, d)) globals)
        in
        let trait_decls =
          TraitDeclId.Map.of_list
            (List.map (fun (d : trait_decl) -> (d.def_id, d)) trait_decls)
        in
        let trait_impls =
          TraitImplId.Map.of_list
            (List.map (fun (d : trait_impl) -> (d.def_id, d)) trait_impls)
        in

        Ok
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
    | _ -> Error "")

and gcrate_of_json
    (body_of_json : of_json_ctx -> json -> ('body gexpr_body, string) result)
    (js : json) : ('body gcrate, string) result =
  match js with
  | `Assoc [ ("charon_version", charon_version); ("translated", translated) ] ->
      (* Ensure the version is the one we support. *)
      let* charon_version = string_of_json () charon_version in
      if
        not (String.equal charon_version CharonVersion.supported_charon_version)
      then
        Error
          ("Incompatible version of charon: this program supports llbc emitted \
            by charon v" ^ CharonVersion.supported_charon_version
         ^ " but attempted to read a file emitted by charon v" ^ charon_version
         ^ ".")
      else gtranslated_crate_of_json body_of_json translated
  | _ -> combine_error_msgs js __FUNCTION__ (Error "")
