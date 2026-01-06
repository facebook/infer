(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
open PythonSourceAst

module Normalize = struct
  let ann_assign_to_assign fields : Node.dict =
    (* remove annotation, simple, type_comment, change type to assign, target becomes targets = [target] *)
    let assign_fields =
      StringMap.fold
        (fun k field_node acc ->
          match k with
          | "annotation" | "simple" ->
              acc
          | k when String.equal k Node.type_field_name ->
              StringMap.add Node.type_field_name (Node.Str "Assign") acc
          | "target" ->
              StringMap.add "targets" (Node.List [field_node]) acc
          | _ ->
              StringMap.add k field_node acc )
        fields StringMap.empty
    in
    StringMap.add "type_comment" Node.Null assign_fields


  let rec apply (node : Node.t) : Node.t =
    match node with
    | Dict fields when Node.is_type fields "Import" || Node.is_type fields "ImportFrom" ->
        Null
    | Dict fields when Node.is_type fields "Compare" -> (
        let left_node = StringMap.find_opt "left" fields in
        let left_fields =
          match left_node with Some (Dict fields) -> fields | _ -> StringMap.empty
        in
        match StringMap.find_opt "attr" left_fields with
        | Some (Str "__class__") ->
            let fst_arg =
              match StringMap.find_opt Node.field_value left_fields with
              | Some arg ->
                  arg
              | _ ->
                  L.die InternalError "Could not find object where __class__ attribute is accessed"
            in
            let snd_arg =
              match StringMap.find_opt "comparators" fields with
              | Some (List [x]) ->
                  x
              | _ ->
                  L.die InternalError
                    "Could not find rhs type in comparison using __class__ attribute"
            in
            let ctx_node = Node.find_field_or_null Node.field_ctx left_fields in
            let lineno = Node.find_field_or_null Node.field_lineno fields in
            let end_lineno = Node.find_field_or_null Node.field_end_lineno fields in
            let func_node =
              Node.make_dict_node
                [ (Node.type_field_name, Str "Name")
                ; (Node.field_id, Str "isinstance")
                ; (Node.field_ctx, ctx_node)
                ; (Node.field_lineno, lineno)
                ; (Node.field_end_lineno, end_lineno) ]
            in
            Node.make_dict_node
              [ (Node.type_field_name, Str "Call")
              ; (Node.field_lineno, lineno)
              ; (Node.field_end_lineno, end_lineno)
              ; (Node.field_func, func_node)
              ; (Node.field_args, List [fst_arg; snd_arg])
              ; (Node.field_keywords, List []) ]
        | _ ->
            Dict (StringMap.map (fun v -> apply v) fields) )
    | Dict fields ->
        Dict (StringMap.map (fun v -> apply v) fields)
    | List l ->
        List
          (List.filter
             ~f:(fun n -> match n with Node.Null -> false | _ -> true)
             (List.map ~f:apply l) )
    | other ->
        other
end

(* ===== Diff Computation ===== *)
let get_builtin_name_from_type_name type_name =
  match type_name with
  | "Dict" | "FrozenSet" | "List" | "Set" | "Tuple" ->
      Some (String.lowercase type_name)
  | _ ->
      None


let rec annotations_equal (n1 : Node.t) (n2 : Node.t) : bool =
  let internal_fields_equal fields1 fields2 =
    StringMap.for_all
      (fun k _ ->
        StringMap.mem k fields1
        || String.equal k Node.type_field_name
        || Node.is_line_number_field k )
      fields2
    && StringMap.for_all
         (fun k v1 ->
           String.equal k Node.field_id
           || String.equal k Node.type_field_name
           || Node.is_line_number_field k
           || Option.exists (StringMap.find_opt k fields2) ~f:(fun v2 -> annotations_equal v1 v2) )
         fields1
  in
  match (n1, n2) with
  | Null, Dict _ ->
      (* Adding any type annotation is fine *)
      true
  | Dict fields1, Dict fields2 -> (
    (* More complex case: type annotations are preeexisting and change to something else
      A few cases to consider here:
      Dict --> dict, Set --> set etc
      Any --> object should also be allowed *)
    match (Node.get_type fields1, Node.get_type fields2) with
    | Str type1, Str type2 when not (String.equal type1 type2) ->
        false
    | Str "Name", Str "Name" ->
        IOption.exists2 (StringMap.find_opt Node.field_id fields1)
          (StringMap.find_opt Node.field_id fields2) ~f:(fun id1 id2 ->
            match (id1, id2) with
            | Node.Str id1, Node.Str id2 ->
                let builtin = Option.value (get_builtin_name_from_type_name id1) ~default:id1 in
                let ids_equal =
                  String.equal builtin id2 || (String.equal id1 "Any" && String.equal id2 "object")
                in
                ids_equal && internal_fields_equal fields1 fields2
            | _ ->
                false )
    | _ ->
        (* For all other Dict node types (Subscript, Tuple, Load, etc.) *)
        (* Recursively compare all fields, skipping type and line number fields *)
        internal_fields_equal fields1 fields2 )
  | List l1, List l2 ->
      (* Compare lists element by element *)
      Int.equal (List.length l1) (List.length l2) && List.for_all2_exn ~f:annotations_equal l1 l2
  | a, b ->
      Node.equal a b


let zip_and_build_diffs ~normalize_right_dict ~skip_annotations (n1 : Node.t) (n2 : Node.t) :
    Diff.t list =
  let rec zip ~left_line ~right_line acc (n1 : Node.t) (n2 : Node.t) : Diff.t list =
    match (n1, n2) with
    | a, b when Node.equal a b ->
        acc
    | Dict f1, Dict f2 ->
        let left_line = Node.get_line_number f1 in
        let right_line = Node.get_line_number f2 in
        let f2 = normalize_right_dict ~left:f1 ~right:f2 in
        let acc =
          StringMap.fold
            (fun k v1 acc ->
              match StringMap.find_opt k f2 with
              | Some v2 when Node.is_type_annotation_field k ->
                  if
                    (* special case for type annotations *)
                    skip_annotations v1 v2
                  then acc
                  else zip ~left_line ~right_line acc v1 v2
              | Some _ when Node.is_line_number_field k ->
                  acc
              | Some v2 ->
                  zip ~left_line ~right_line acc v1 v2
              | None ->
                  Diff.append_removed_line left_line acc )
            f1 acc
        in
        let missing_in_left =
          StringMap.fold
            (fun k _ acc ->
              if StringMap.mem k f1 then acc else Diff.append_added_line right_line acc )
            f2 acc
        in
        missing_in_left
    | List l1, List l2 ->
        let rec aux acc xs ys =
          match (xs, ys) with
          | x :: xt, y :: yt ->
              aux (zip ~left_line ~right_line acc x y) xt yt
          | x :: xt, [] ->
              aux (Diff.append_removed_line (Node.get_line_number_of_node x) acc) xt []
          | [], y :: yt ->
              aux (Diff.append_added_line (Node.get_line_number_of_node y) acc) [] yt
          | [], [] ->
              acc
        in
        aux acc l1 l2
    | Dict f1, _ ->
        Diff.append_removed_line (Node.get_line_number f1) acc
    | _, Dict f2 ->
        Diff.append_added_line (Node.get_line_number f2) acc
    | _ ->
        Diff.append_removed_line left_line (Diff.append_added_line right_line acc)
  in
  zip ~left_line:None ~right_line:None [] n1 n2


let make_diffs n1 n2 =
  zip_and_build_diffs n1 n2
    ~normalize_right_dict:(fun ~left ~right ->
      if Node.is_type left "Assign" && Node.is_type right "AnnAssign" then
        Normalize.ann_assign_to_assign right
      else right )
    ~skip_annotations:annotations_equal


let normalize = Normalize.apply

let ast_diff ~debug ~test_eqsat ?filename1 ?filename2 previous_content current_content =
  let parse = build_parser () in
  match (parse ?filename:filename1 previous_content, parse ?filename:filename2 current_content) with
  | Error error, _ | Ok _, Error error ->
      L.user_error "%a" PythonSourceAst.pp_error error ;
      []
  | Ok original_ast1, Ok original_ast2 ->
      let ast1 = Normalize.apply original_ast1 in
      let ast2 = Normalize.apply original_ast2 in
      let diffs =
        make_diffs ast1 ast2 |> Diff.gen_explicit_diffs ~previous_content ~current_content
      in
      if test_eqsat then
        PythonSourceAstDiff.check_equivalence ~debug:false ~expected:(List.is_empty diffs)
          original_ast1 original_ast2
        |> ignore ;
      if debug then (
        F.printf "AST1: %s\n" (Node.to_str ast1) ;
        F.printf "AST2: %s\n" (Node.to_str ast2) ;
        F.printf "SemDiff:\n" ;
        List.iter diffs ~f:(fun diff -> F.printf "%a\n" Diff.pp_explicit diff) ) ;
      diffs


let test_ast_diff ~debug ~test_eqsat src1 src2 = ast_diff ~debug ~test_eqsat src1 src2

let semdiff previous_file current_file =
  let debug = Config.debug_mode in
  let previous_src = In_channel.with_file previous_file ~f:In_channel.input_all in
  let current_src = In_channel.with_file current_file ~f:In_channel.input_all in
  let diffs =
    ast_diff ~debug ~test_eqsat:false ~filename1:previous_file ~filename2:current_file previous_src
      current_src
  in
  let out_path = ResultsDir.get_path SemDiff in
  Diff.write_json ~previous_file ~current_file ~out_path diffs
