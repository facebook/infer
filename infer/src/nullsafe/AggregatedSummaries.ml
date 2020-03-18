(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module ClassInfo = struct
  type t = {summaries: Summary.t list; nested_anonymous_classes: anonymous_class_to_summaries}

  and anonymous_class_to_summaries = Summary.t list JavaClassName.Map.t

  let make_empty () = {summaries= []; nested_anonymous_classes= JavaClassName.Map.empty}

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


  let get_anonymous_summaries {nested_anonymous_classes} =
    JavaClassName.Map.fold
      (fun class_name summaries acc -> (class_name, summaries) :: acc)
      nested_anonymous_classes []


  let get_all_summaries t =
    let anonymous_summaries =
      get_anonymous_summaries t
      |> List.map ~f:(fun (_, summaries) -> summaries)
      |> List.fold ~init:[] ~f:( @ )
    in
    t.summaries @ anonymous_summaries
end

type t = ClassInfo.t JavaClassName.Map.t

let make_empty () = JavaClassName.Map.empty

(* If key (class_name) was not in the map yet, add it, otherwise modify the existing value.
   [update] is a function that modifies value.
 *)
let update_in_map class_name ~update t =
  JavaClassName.Map.update class_name
    (fun class_info ->
      let info_to_update = Option.value class_info ~default:(ClassInfo.make_empty ()) in
      Some (update info_to_update) )
    t


let register_summary java_class_name summary t =
  match JavaClassName.get_user_defined_class_if_anonymous_inner java_class_name with
  | Some outer_class_name ->
      (* That was a nested anonymous class.
         We don't register it at top level, registed outer class instead. *)
      update_in_map outer_class_name
        ~update:(ClassInfo.add_anonymous_summary java_class_name summary)
        t
  | None ->
      (* This is not an anonymous class, register it as is *)
      update_in_map java_class_name ~update:(ClassInfo.add_class_summary summary) t


let group_by_user_class t =
  JavaClassName.Map.fold (fun class_name class_info acc -> (class_name, class_info) :: acc) t []
