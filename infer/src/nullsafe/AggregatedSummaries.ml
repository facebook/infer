(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module ClassInfo = struct
  type t =
    { class_name: JavaClassName.t
    ; summaries: NullsafeSummary.t list
    ; mutable nested_classes_info: t list
    ; nested_anonymous_classes: anonymous_class_to_summaries }

  and anonymous_class_to_summaries = NullsafeSummary.t list JavaClassName.Map.t

  let get_class_name {class_name} = class_name

  let get_summaries {summaries} = summaries

  let get_nested_anonymous_summaries {nested_anonymous_classes} = nested_anonymous_classes

  let get_nested_classes_info {nested_classes_info} = nested_classes_info

  (** Add a link between this class and a nested class info *)
  let add_nested_class_info ~nested t = t.nested_classes_info <- nested :: t.nested_classes_info

  let make_empty class_name =
    { class_name
    ; summaries= []
    ; nested_classes_info= []
    ; nested_anonymous_classes= JavaClassName.Map.empty }


  (** Add top level (not anonymous) summary belonging to this class *)
  let add_class_summary summary t = {t with summaries= summary :: t.summaries}

  (** Add summary for the nested anonymous class belonging to this class *)
  let add_anonymous_summary anonymous_class_name summary t =
    let updated_anonymous_classes =
      JavaClassName.Map.update anonymous_class_name
        (function None -> Some [summary] | Some summaries -> Some (summary :: summaries))
        t.nested_anonymous_classes
    in
    {t with nested_anonymous_classes= updated_anonymous_classes}


  let rec get_recursive_summaries t =
    let this_summaries = List.map t.summaries ~f:(fun summary -> (t.class_name, summary)) in
    let anonymous_summaries =
      JavaClassName.Map.bindings t.nested_anonymous_classes
      |> List.map ~f:(fun (class_name, summaries) ->
             List.map summaries ~f:(fun summary -> (class_name, summary)) )
      |> List.concat
    in
    let nested_summaries =
      List.map t.nested_classes_info ~f:get_recursive_summaries |> List.concat
    in
    this_summaries @ nested_summaries @ anonymous_summaries


  let rec pp fmt class_info =
    let pp_anonymous fmt (class_name, summaries) =
      F.fprintf fmt "Class name: %a, Summaries: %d" JavaClassName.pp class_name
        (List.length summaries)
    in
    let pp_list fmt items ~pp =
      if List.is_empty items then F.fprintf fmt "<empty>"
      else List.iter items ~f:(F.fprintf fmt "%a@\n" pp)
    in
    let pp_content fmt {summaries; nested_anonymous_classes; nested_classes_info} =
      F.fprintf fmt
        "Summaries: %d@\nNested anonymous classes:@\n  @[%a@]@\nNested classes:@\n  @[%a@]"
        (List.length summaries) (pp_list ~pp:pp_anonymous)
        (JavaClassName.Map.bindings nested_anonymous_classes)
        (pp_list ~pp) nested_classes_info
    in
    F.fprintf fmt "Class name: %a@\n  @[%a@]" JavaClassName.pp class_info.class_name pp_content
      class_info
end

(* If key (class_name) was not in the map yet, add it, otherwise modify the existing value.
   [update] is a function that modifies value.
 *)
let update_in_map class_name ~update t =
  JavaClassName.Map.update class_name
    (fun class_info ->
      let info_to_update = Option.value class_info ~default:(ClassInfo.make_empty class_name) in
      Some (update info_to_update) )
    t


(* Add (empty) class info for all outer classes for this class recursively,
   if it did not exist yet *)
let rec register_ancestors user_defined_class map =
  match JavaClassName.get_outer_class_name user_defined_class with
  | Some outer_class ->
      (* add to the map if not added and to the same for parent *)
      update_in_map outer_class ~update:ident map |> register_ancestors outer_class
  | None ->
      map


(* Register this class and all it ancestor classes, if not registered yet,
   and update list of summaries for this class.
*)
let register_classes_and_add_summary class_name summary map =
  match JavaClassName.get_user_defined_class_if_anonymous_inner class_name with
  | Some outer_class_name ->
      (* That was a nested anonymous class.
         We don't register it at top level, registed outer class instead. *)
      update_in_map outer_class_name
        ~update:(ClassInfo.add_anonymous_summary class_name summary)
        map
      |> register_ancestors outer_class_name
  | None ->
      (* This is not an anonymous class, register it as is *)
      update_in_map class_name ~update:(ClassInfo.add_class_summary summary) map
      |> register_ancestors class_name


(* the result is the map from all class names (including all ancestors) to corresponding info with summaries,
   without links to nested classes *)
let create_initial_map all_summaries =
  List.fold all_summaries ~init:JavaClassName.Map.empty ~f:(fun map (class_name, summary) ->
      register_classes_and_add_summary class_name summary map )


(* given a map containing all nested and parent class names (excluding anonymous classes),
   update [nested_classes_info] links for all outer classes
*)
let set_links map =
  JavaClassName.Map.iter
    (fun class_name class_info ->
      Option.iter (JavaClassName.get_outer_class_name class_name) ~f:(fun outer_class_name ->
          let outer_info = JavaClassName.Map.find outer_class_name map in
          ClassInfo.add_nested_class_info ~nested:class_info outer_info ) )
    map


let aggregate all_summaries =
  let class_name_to_info = create_initial_map all_summaries in
  set_links class_name_to_info ;
  (* Return only list of top level classes that are not nested *)
  JavaClassName.Map.bindings class_name_to_info
  |> List.filter_map ~f:(fun (class_name, class_info) ->
         if Option.is_none (JavaClassName.get_outer_class_name class_name) then
           (* This class is the outermost, leave it *)
           Some class_info
         else None )
