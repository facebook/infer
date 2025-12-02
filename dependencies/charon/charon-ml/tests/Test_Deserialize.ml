open Charon
open Logging
module EL = Easy_logging.Logging

let log = main_log

(** [dir_is_empty dir] is true, if [dir] contains no files except * "." and ".."
*)
let dir_is_empty dir = Array.length (Sys.readdir dir) = 0

(** [dir_contents] returns the paths of all regular files that are * contained
    in [dir]. Each file is a path starting with [dir]. * From
    https://gist.github.com/lindig/be55f453026c65e761f4e7012f8ab9b5 *)
let dir_contents dir =
  let rec loop result = function
    | f :: fs when Sys.is_directory f ->
        Sys.readdir f |> Array.to_list
        |> List.map (Filename.concat f)
        |> List.append fs |> loop result
    | f :: fs -> loop (f :: result) fs
    | [] -> result
  in
  loop [] [ dir ]

(* Run the tests *)
let run_tests (folder : string) : unit =
  (* List the LLBC files.

     Remark: we do not deserialize the ULLBC files.
  *)
  let llbc_files =
    dir_contents folder
    |> List.filter (fun file -> Filename.check_suffix file ".llbc")
  in

  let assert_eq x y msg arg_to_string =
    if x <> y then
      raise
        (Failure
           ("Real: " ^ arg_to_string x ^ ", Expected: " ^ arg_to_string y
          ^ " for " ^ msg))
  in

  (* Deserialize LLBC *)
  let () =
    List.iter
      (fun file ->
        log#ldebug (lazy ("Deserializing LLBC file: " ^ file));
        (* Load the module *)
        let json = Yojson.Basic.from_file file in
        match LlbcOfJson.crate_of_json json with
        | Error s ->
            log#error "Error when deserializing file %s: %s\n" file s;
            exit 1
        | Ok m ->
            log#linfo (lazy ("Deserialized: " ^ file));
            (* Test discriminant/tag roundtrip *)
            let print_ctx = PrintUtils.of_crate m in
            let print_var_id_opt = function
              | Some v ->
                  "Some (" ^ PrintTypes.variant_id_to_pretty_string v ^ ")"
              | None -> "None"
            in
            let print_scalar_value_opt = function
              | Some v -> "Some " ^ PrintValues.scalar_value_to_string v
              | None -> "None"
            in
            let ptr_size = m.target_information.target_pointer_size in
            Types.TypeDeclId.Map.iter
              (fun _ (ty_decl : Types.type_decl) ->
                match ty_decl.Types.layout with
                | Some layout ->
                    if Option.is_some layout.discriminant_layout then
                      let name =
                        PrintTypes.name_to_string print_ctx
                          ty_decl.item_meta.name
                      in
                      Types.VariantId.iteri
                        (fun var_id _ ->
                          let variant_layout =
                            Types.VariantId.nth layout.variant_layouts var_id
                          in
                          let tag = variant_layout.tag in
                          if variant_layout.uninhabited then
                            assert_eq tag None name print_scalar_value_opt
                          else
                            match tag with
                            | None -> () (* Must be the untagged variant *)
                            | Some tag ->
                                let roundtrip_var_id =
                                  TypesUtils.get_variant_from_tag ptr_size
                                    ty_decl tag
                                in
                                assert_eq roundtrip_var_id (Some var_id)
                                  (name ^ " with tag: "
                                  ^ print_scalar_value_opt (Some tag))
                                  print_var_id_opt)
                        layout.variant_layouts
                    else ()
                | None -> ())
              m.type_decls;
            (* Test that pretty-printing doesn't crash *)
            let printed = PrintLlbcAst.Crate.crate_to_string m in
            log#ldebug (lazy ("\n" ^ printed ^ "\n")))
      llbc_files
  in

  (* Done *)
  ()
