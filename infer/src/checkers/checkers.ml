(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Module for user-defined checkers. *)

module L = Logging
module F = Format

let verbose = ref true

(** Convenience functions for checkers to print information *)
module PP = struct
  (** Print a range of lines of the source file in [loc], including [nbefore] lines before loc
      and [nafter] lines after [loc] *)
  let pp_loc_range linereader nbefore nafter fmt loc =
    let printline n =
      match Printer.LineReader.from_loc linereader { loc with Location.line = n } with
      | Some s -> F.fprintf fmt "%s%s@\n" (if Int.equal n loc.Location.line then "-->" else "   ") s
      | _ -> () in
    F.fprintf fmt "%a:%d@\n" SourceFile.pp loc.Location.file loc.Location.line;
    for n = loc.Location.line - nbefore to loc.Location.line + nafter do printline n done
end (* PP *)


(** State that persists in the .specs files. *)
module ST = struct

  let report_error tenv
      proc_name
      proc_desc
      kind
      loc
      ?(advice = None)
      ?(field_name = None)
      ?(origin_loc = None)
      ?(exception_kind = fun k d -> Exceptions.Checkers (k, d))
      ?(always_report = false)
      description =
    let lookup = Tenv.lookup tenv in
    let localized_description = Localise.custom_desc_with_advice
        description
        (Option.value ~default:"" advice)
        [("always_report", string_of_bool always_report)] in
    let exn = exception_kind (Localise.to_issue_id kind) localized_description in
    let proc_attributes = Specs.pdesc_resolve_attributes proc_desc in

    (* Errors can be suppressed with annotations. An error of kind CHECKER_ERROR_NAME can be
       suppressed with the following annotations:
       - @android.annotation.SuppressLint("checker-error-name")
       - @some.PrefixErrorName
       where the kind matching is case - insensitive and ignores '-' and '_' characters. *)
    let suppressed =
      let annotation_matches (a: Annot.t) =
        let normalize str =
          Str.global_replace (Str.regexp "[_-]") "" (String.lowercase str) in
        let drop_prefix str =
          Str.replace_first (Str.regexp "^[A-Za-z]+_") "" str in
        let normalized_equal s1 s2 =
          String.equal (normalize s1) (normalize s2) in

        let is_parameter_suppressed =
          String.is_suffix a.class_name ~suffix:Annotations.suppress_lint &&
          List.mem ~equal:normalized_equal a.parameters (Localise.to_issue_id kind) in
        let is_annotation_suppressed =
          String.is_suffix
            ~suffix:(normalize (drop_prefix (Localise.to_issue_id kind)))
            (normalize a.class_name) in

        is_parameter_suppressed || is_annotation_suppressed in

      let is_method_suppressed =
        Annotations.ma_has_annotation_with
          proc_attributes.ProcAttributes.method_annotation
          annotation_matches in

      let is_field_suppressed =
        match field_name, PatternMatch.get_this_type proc_attributes with
        | Some field_name, Some t -> begin
            match Typ.Struct.get_field_type_and_annotation ~lookup field_name t with
            | Some (_, ia) -> Annotations.ia_has_annotation_with ia annotation_matches
            | None -> false
          end
        | _ -> false in

      let is_class_suppressed =
        match PatternMatch.get_this_type proc_attributes with
        | Some t -> begin
            match (PatternMatch.type_get_annotation tenv t) with
            | Some ia -> Annotations.ia_has_annotation_with ia annotation_matches
            | None -> false
          end
        | None -> false in

      is_method_suppressed || is_field_suppressed || is_class_suppressed in

    let trace =
      let origin_elements =
        match origin_loc with
        | Some oloc -> [Errlog.make_trace_element 0 oloc "origin" []]
        | None -> [] in
      origin_elements @ [Errlog.make_trace_element 0 loc description []]
    in

    if not suppressed then
      begin
        if !verbose then
          begin
            L.stdout "%s: %a: %s@."
              (Localise.to_issue_id kind)
              SourceFile.pp loc.Location.file
              (Typ.Procname.to_string proc_name);
            L.stdout "%s@." description
          end;
        Reporting.log_error proc_name ~loc ~ltr:trace exn
      end
end
