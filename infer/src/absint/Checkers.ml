(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module for user-defined checkers. *)

(** State that persists in the .specs files. *)
module ST = struct
  let report_error tenv proc_name proc_desc kind loc ?(field_name= None) ?(origin_loc= None)
      ?(exception_kind= fun k d -> Exceptions.Checkers (k, d)) ?(always_report= false) description =
    let lookup = Tenv.lookup tenv in
    let localized_description =
      Localise.custom_desc description [("always_report", string_of_bool always_report)]
    in
    let exn = exception_kind kind localized_description in
    let proc_attributes = Summary.pdesc_resolve_attributes proc_desc in
    (* Errors can be suppressed with annotations. An error of kind CHECKER_ERROR_NAME can be
       suppressed with the following annotations:
       - @android.annotation.SuppressLint("checker-error-name")
       - @some.PrefixErrorName
       where the kind matching is case - insensitive and ignores '-' and '_' characters. *)
    let suppressed =
      let annotation_matches (a: Annot.t) =
        let normalize str = Str.global_replace (Str.regexp "[_-]") "" (String.lowercase str) in
        let drop_prefix str = Str.replace_first (Str.regexp "^[A-Za-z]+_") "" str in
        let normalized_equal s1 s2 = String.equal (normalize s1) (normalize s2) in
        let is_parameter_suppressed =
          String.is_suffix a.class_name ~suffix:Annotations.suppress_lint
          && List.mem ~equal:normalized_equal a.parameters kind.IssueType.unique_id
        in
        let is_annotation_suppressed =
          String.is_suffix
            ~suffix:(normalize (drop_prefix kind.IssueType.unique_id))
            (normalize a.class_name)
        in
        is_parameter_suppressed || is_annotation_suppressed
      in
      let is_method_suppressed =
        Annotations.ma_has_annotation_with proc_attributes.ProcAttributes.method_annotation
          annotation_matches
      in
      let is_field_suppressed =
        match (field_name, PatternMatch.get_this_type proc_attributes) with
        | Some field_name, Some t -> (
          match Typ.Struct.get_field_type_and_annotation ~lookup field_name t with
          | Some (_, ia) ->
              Annotations.ia_has_annotation_with ia annotation_matches
          | None ->
              false )
        | _ ->
            false
      in
      let is_class_suppressed =
        match PatternMatch.get_this_type proc_attributes with
        | Some t -> (
          match PatternMatch.type_get_annotation tenv t with
          | Some ia ->
              Annotations.ia_has_annotation_with ia annotation_matches
          | None ->
              false )
        | None ->
            false
      in
      is_method_suppressed || is_field_suppressed || is_class_suppressed
    in
    let trace =
      let origin_elements =
        match origin_loc with
        | Some oloc ->
            [Errlog.make_trace_element 0 oloc "origin" []]
        | None ->
            []
      in
      origin_elements @ [Errlog.make_trace_element 0 loc description []]
    in
    if not suppressed then Reporting.log_error_deprecated proc_name ~loc ~ltr:trace exn
end
