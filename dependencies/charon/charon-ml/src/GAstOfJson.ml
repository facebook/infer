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

let option_list_of_json of_json = list_of_json (option_of_json of_json)

(* This is written by hand because the corresponding rust type is not type-generic. *)
let rec gfun_decl_of_json
    (body_of_json :
      of_json_ctx -> json -> ('body gexpr_body option, string) result)
    (ctx : of_json_ctx) (js : json) : ('body gfun_decl, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc
        [
          ("def_id", def_id);
          ("item_meta", item_meta);
          ("generics", generics);
          ("signature", signature);
          ("src", src);
          ("is_global_initializer", is_global_initializer);
          ("body", body);
        ] ->
        let* def_id = FunDeclId.id_of_json ctx def_id in
        let* item_meta = item_meta_of_json ctx item_meta in
        let* generics = generic_params_of_json ctx generics in
        let* signature = fun_sig_of_json ctx signature in
        let* src = item_source_of_json ctx src in
        let* is_global_initializer =
          option_of_json global_decl_id_of_json ctx is_global_initializer
        in
        let* body = body_of_json ctx body in
        Ok
          {
            def_id;
            item_meta;
            generics;
            signature;
            src;
            is_global_initializer;
            body;
          }
    | _ -> Error "")

(** Deserialize a map from file id to file name.

    In the serialized LLBC, the files in the loc spans are refered to by their
    ids, in order to save space. In a functional language like OCaml this is not
    necessary: we thus replace the file ids by the file name themselves in the
    AST. The "id to file" map is thus only used in the deserialization process.
*)
and id_to_file_of_json (ctx : of_json_ctx) (js : json) :
    (of_json_ctx, string) result =
  combine_error_msgs js __FUNCTION__
    ((* The map is stored as a list of pairs (key, value): we deserialize
      * this list then convert it to a map *)
     let* files = list_of_json (option_of_json file_of_json) ctx js in
     let files_with_ids =
       List.filter_map
         (fun (i, file) ->
           match file with
           | None -> None
           | Some file -> Some (i, file))
         (List.mapi (fun i file -> (FileId.of_int i, file)) files)
     in
     let id_to_file_map = FileId.Map.of_list files_with_ids in
     Ok { ctx with id_to_file_map })

(* This is written by hand because the corresponding rust type is not
   type-generic. Note: because of hash-cons deduplication, we must make sure to
   deserialize in the exact same order as the rust side. *)
and gtranslated_crate_of_json
    (body_of_json :
      of_json_ctx -> json -> ('body gexpr_body option, string) result)
    (js : json) : ('body gcrate, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc
        [
          ("crate_name", crate_name);
          ("options", options);
          ("target_information", target_info);
          ("item_names", item_names);
          ("short_names", short_names);
          ("files", files);
          ("type_decls", type_decls);
          ("fun_decls", fun_decls);
          ("global_decls", global_decls);
          ("trait_decls", trait_decls);
          ("trait_impls", trait_impls);
          ("unit_metadata", unit_metadata);
          ("ordered_decls", ordered_decls);
        ] ->
        let ctx = empty_of_json_ctx in

        (* This can be deserialized out of order because it contains no hash-consed values *)
        let* ctx = id_to_file_of_json ctx files in

        let* crate_name = string_of_json ctx crate_name in
        let* options = cli_options_of_json ctx options in
        let* target_information = target_info_of_json ctx target_info in
        let* _item_names =
          list_of_json
            (key_value_pair_of_json item_id_of_json name_of_json)
            ctx item_names
        in
        let* _short_names =
          list_of_json
            (key_value_pair_of_json item_id_of_json name_of_json)
            ctx short_names
        in
        let* type_decls =
          option_list_of_json type_decl_of_json ctx type_decls
        in
        let* fun_decls =
          option_list_of_json (gfun_decl_of_json body_of_json) ctx fun_decls
        in
        let* global_decls =
          option_list_of_json global_decl_of_json ctx global_decls
        in
        let* trait_decls =
          option_list_of_json trait_decl_of_json ctx trait_decls
        in
        let* trait_impls =
          option_list_of_json trait_impl_of_json ctx trait_impls
        in
        let* unit_metadata = global_decl_ref_of_json ctx unit_metadata in
        let* ordered_decls =
          list_of_json declaration_group_of_json ctx ordered_decls
        in

        let type_decls = TypeDeclId.map_of_indexed_list type_decls in
        let fun_decls = FunDeclId.map_of_indexed_list fun_decls in
        let global_decls = GlobalDeclId.map_of_indexed_list global_decls in
        let trait_decls = TraitDeclId.map_of_indexed_list trait_decls in
        let trait_impls = TraitImplId.map_of_indexed_list trait_impls in

        Ok
          {
            name = crate_name;
            options;
            target_information;
            declarations = ordered_decls;
            type_decls;
            fun_decls;
            global_decls;
            trait_decls;
            trait_impls;
            unit_metadata;
          }
    | _ -> Error "")

and gcrate_of_json
    (body_of_json :
      of_json_ctx -> json -> ('body gexpr_body option, string) result)
    (js : json) : ('body gcrate, string) result =
  match js with
  | `Assoc [ ("charon_version", charon_version); ("translated", translated) ]
  | `Assoc [ ("charon_version", charon_version); ("translated", translated); _ ]
    ->
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
