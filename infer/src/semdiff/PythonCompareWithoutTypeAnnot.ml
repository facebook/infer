(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module StringSet = IString.Set
module IntSet = IInt.Set
open PythonSourceAst

module Normalize = struct
  let ann_assign_to_assign fields : Node.t =
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
    Dict (StringMap.add "type_comment" Node.Null assign_fields)


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
type diff = LineAdded of int | LineRemoved of int [@@deriving compare, equal]

module Diff = struct
  let append_removed_line_to_diff_list left_line acc =
    Option.value_map left_line ~default:acc ~f:(fun line -> LineRemoved line :: acc)


  let append_added_line_to_diff_list right_line acc =
    Option.value_map right_line ~default:acc ~f:(fun line -> LineAdded line :: acc)


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
    | a, b when Node.equal a b ->
        (* Type annotations are equal: no need to compare field by field *)
        true
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
    | _ ->
        false


  let rec get_diff ?(left_line : int option = None) ?(right_line : int option = None) (n1 : Node.t)
      (n2 : Node.t) : diff list =
    match (n1, n2) with
    | a, b when Node.equal a b ->
        []
    | Dict f1, Dict f2 ->
        let left_line = Node.get_line_number f1 in
        let right_line = Node.get_line_number f2 in
        if Node.is_type f1 "Assign" && Node.is_type f2 "AnnAssign" then
          get_diff ~left_line ~right_line n1 (Normalize.ann_assign_to_assign f2)
        else
          let diffs =
            StringMap.fold
              (fun k v1 acc ->
                match StringMap.find_opt k f2 with
                | Some v2 when Node.is_type_annotation_field k ->
                    if
                      (* special case for type annotations *)
                      annotations_equal v1 v2
                    then acc
                    else get_diff ~left_line ~right_line v1 v2 @ acc
                | Some _ when Node.is_line_number_field k ->
                    acc
                | Some v2 ->
                    get_diff ~left_line ~right_line v1 v2 @ acc
                | None ->
                    append_removed_line_to_diff_list left_line acc )
              f1 []
          in
          let missing_in_left =
            StringMap.fold
              (fun k _ acc ->
                if StringMap.mem k f1 then acc else append_added_line_to_diff_list right_line acc )
              f2 []
          in
          diffs @ missing_in_left
    | List l1, List l2 ->
        let rec aux acc xs ys =
          match (xs, ys) with
          | x :: xt, y :: yt ->
              aux (get_diff ~left_line ~right_line x y @ acc) xt yt
          | x :: xt, [] ->
              aux (append_removed_line_to_diff_list (Node.get_line_number_of_node x) acc) xt []
          | [], y :: yt ->
              aux (append_added_line_to_diff_list (Node.get_line_number_of_node y) acc) [] yt
          | [], [] ->
              acc
        in
        aux [] l1 l2
    | Dict f1, _ ->
        append_removed_line_to_diff_list (Node.get_line_number f1) []
    | _, Dict f2 ->
        append_added_line_to_diff_list (Node.get_line_number f2) []
    | _ ->
        append_removed_line_to_diff_list left_line [] @ append_added_line_to_diff_list right_line []


  let split_diffs diffs =
    List.fold
      ~f:(fun (lines_removed, lines_added) diff ->
        match diff with
        | LineRemoved line_number ->
            (IntSet.add line_number lines_removed, lines_added)
        | LineAdded line_number ->
            (lines_removed, IntSet.add line_number lines_added) )
      ~init:(IntSet.empty, IntSet.empty) diffs
end

let normalize = Normalize.apply

(* ===== Diff Formatting and Output ===== *)
module Output = struct
  let write_output previous_file current_file diffs =
    let out_path = ResultsDir.get_path SemDiff in
    let outcome = if List.is_empty diffs then "equal" else "different" in
    let json =
      `Assoc
        [ ("previous", `String previous_file)
        ; ("current", `String current_file)
        ; ("outcome", `String outcome)
        ; ("diff", `List (List.map ~f:(fun diff -> `String diff) diffs)) ]
    in
    Out_channel.with_file out_path ~f:(fun out_channel -> Yojson.Safe.to_channel out_channel json)


  let merge_changes removed added =
    let rec aux removed added acc =
      match (removed, added) with
      | [], [] ->
          List.rev acc
      | a :: as', [] ->
          aux as' [] ((Some a, None) :: acc)
      | [], r :: rs' ->
          aux [] rs' ((None, Some r) :: acc)
      | ((ia, _) as a) :: as', ((ir, _) as r) :: rs' ->
          if Int.equal ia ir then aux as' rs' ((Some a, Some r) :: acc)
          else if ia < ir then aux as' added ((Some a, None) :: acc)
          else aux removed rs' ((None, Some r) :: acc)
    in
    aux added removed []


  let show_diff file1 file2 lines_removed lines_added =
    let lines1 = String.split_on_chars ~on:['\n'] file1
    and lines2 = String.split_on_chars ~on:['\n'] file2 in
    let get_line_content lines n : string =
      if n - 1 < List.length lines then List.nth_exn lines (n - 1) else ""
    in
    let lines_to_string_set lines =
      List.fold_left ~f:(fun acc (_, s) -> StringSet.add s acc) ~init:StringSet.empty lines
    in
    (* Removes changes from left and right when the only difference is the line number to avoid noise in output *)
    let remove_common_changes list1 list2 =
      let set1 = lines_to_string_set list1 in
      let set2 = lines_to_string_set list2 in
      let common = StringSet.inter set1 set2 in
      let keep (_, s) = not (StringSet.mem s common) in
      (List.filter ~f:keep list1, List.filter ~f:keep list2)
    in
    let lines_removed =
      List.map
        ~f:(fun line_number -> (line_number, get_line_content lines1 line_number))
        (IntSet.to_list lines_removed)
    in
    let lines_added =
      List.map
        ~f:(fun line_number -> (line_number, get_line_content lines2 line_number))
        (IntSet.to_list lines_added)
    in
    let lines_removed, lines_added = remove_common_changes lines_removed lines_added in
    let diffs = merge_changes lines_added lines_removed in
    List.map
      ~f:(fun (line_removed, line_added) ->
        match (line_removed, line_added) with
        | Some (line_number, line_content), None ->
            Printf.sprintf "(Line %d) - %s" line_number line_content
        | Some (line_number, line_content_removed), Some (_, line_content_added) ->
            Printf.sprintf "(Line %d) - %s, + %s" line_number line_content_removed
              line_content_added
        | None, Some (line_number, line_content_added) ->
            Printf.sprintf "(Line %d) + %s" line_number line_content_added
        | None, None ->
            "" )
      diffs
end

let ast_diff ~debug ?filename1 ?filename2 src1 src2 =
  let parse = build_parser () in
  match (parse ?filename:filename1 src1, parse ?filename:filename2 src2) with
  | Error error, _ | Ok _, Error error ->
      L.user_error "%a" PythonSourceAst.pp_error error ;
      []
  | Ok ast1, Ok ast2 ->
      let ast1 = Normalize.apply ast1 in
      let ast2 = Normalize.apply ast2 in
      let diffs = Diff.get_diff ast1 ast2 in
      let lines_removed, lines_added = Diff.split_diffs diffs in
      let diffs = Output.show_diff src1 src2 lines_removed lines_added in
      if debug then (
        Printf.printf "AST1: %s\n" (Node.to_str ast1) ;
        Printf.printf "AST2: %s\n" (Node.to_str ast2) ;
        Printf.printf "SemDiff:\n%s\n" (String.concat ~sep:"\n" diffs) ) ;
      diffs


let test_ast_diff ~debug src1 src2 = ast_diff ~debug src1 src2

let semdiff previous_file current_file =
  let debug = Config.debug_mode in
  let previous_src = In_channel.with_file previous_file ~f:In_channel.input_all in
  let current_src = In_channel.with_file current_file ~f:In_channel.input_all in
  let diffs =
    ast_diff ~debug ~filename1:previous_file ~filename2:current_file previous_src current_src
  in
  Output.write_output previous_file current_file diffs
