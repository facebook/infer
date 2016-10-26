(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

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
      | Some s -> F.fprintf fmt "%s%s@\n" (if n = loc.Location.line then "-->" else "   ") s
      | _ -> () in
    F.fprintf fmt "%s:%d@\n" (DB.source_file_to_string loc.Location.file) loc.Location.line;
    for n = loc.Location.line - nbefore to loc.Location.line + nafter do printline n done
end (* PP *)


(** State that persists in the .specs files. *)
module ST = struct
  let add summary key value =
    proc_flags_add summary.Specs.attributes.ProcAttributes.proc_flags key value

  let pname_add proc_name key value =
    let summary = Specs.get_summary_unsafe "ST.pname_add" proc_name in
    add summary key value

  let files_open = ref Procname.Set.empty

  let pname_find proc_name key =
    if Procname.Set.mem proc_name !files_open then
      let summary = Specs.get_summary_unsafe "ST.pname_find" proc_name in
      proc_flags_find summary.Specs.attributes.ProcAttributes.proc_flags key
    else begin
      match Specs.get_summary proc_name with
      | None -> raise Not_found
      | Some summary ->
          begin
            files_open := Procname.Set.add proc_name !files_open;
            proc_flags_find summary.Specs.attributes.ProcAttributes.proc_flags key
          end
    end

  let store_summary tenv proc_name =
    Option.may
      (fun summary ->
         let summary' =
           { summary with
             Specs.timestamp = summary.Specs.timestamp + 1 } in
         try Specs.store_summary tenv proc_name summary' with Sys_error s -> L.err "%s@." s)
      (Specs.get_summary proc_name)

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
        (Option.default "" advice)
        [("always_report", string_of_bool always_report)] in
    let exn = exception_kind kind localized_description in
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
          string_equal (normalize s1) (normalize s2) in

        let is_parameter_suppressed =
          IList.mem string_equal a.class_name [Annotations.suppressLint] &&
          IList.mem normalized_equal kind a.parameters in
        let is_annotation_suppressed =
          string_is_suffix (normalize (drop_prefix kind)) (normalize a.class_name) in

        is_parameter_suppressed || is_annotation_suppressed in

      let is_method_suppressed =
        Annotations.ma_has_annotation_with
          proc_attributes.ProcAttributes.method_annotation
          annotation_matches in

      let is_field_suppressed =
        match field_name, PatternMatch.get_this_type proc_attributes with
        | Some field_name, Some t -> begin
            match StructTyp.get_field_type_and_annotation ~lookup field_name t with
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
      let make_trace_element loc description =
        [{
          Errlog.lt_level = 0;
          Errlog.lt_loc = loc;
          Errlog.lt_description = description;
          Errlog.lt_node_tags = []
        }] in
      let origin_elements =
        match origin_loc with
        | Some oloc -> make_trace_element oloc "origin"
        | None -> [] in
      origin_elements @ (make_trace_element loc description) in

    if not suppressed then
      begin
        if !verbose then
          begin
            let file = DB.source_file_to_string loc.Location.file in
            L.stdout "%s: %s: %s@."
              kind
              file
              (Procname.to_string proc_name);
            L.stdout "%s@." description
          end;
        Reporting.log_error proc_name ~loc ~ltr:trace exn
      end
end

let report_calls_and_accesses tenv callback node instr =
  let proc_desc = Cfg.Node.get_proc_desc node in
  let proc_name = Cfg.Procdesc.get_proc_name proc_desc in
  let callee = Procname.to_string proc_name in
  match PatternMatch.get_java_field_access_signature instr with
  | Some (bt, fn, ft) ->
      ST.report_error tenv
        proc_name
        proc_desc
        (callback ^ "_CALLBACK")
        (Cfg.Procdesc.get_loc proc_desc)
        (Format.sprintf "field access %s.%s:%s in %s@." bt fn ft callee)
  | None ->
      match PatternMatch.get_java_method_call_formal_signature instr with
      | Some (bt, fn, _, rt) ->
          ST.report_error tenv
            proc_name
            proc_desc
            (callback ^ "_CALLBACK")
            (Cfg.Procdesc.get_loc proc_desc)
            (Format.sprintf "method call %s.%s(%s):%s in %s@." bt fn "..." rt callee)
      | None -> ()

(** Report all field accesses and method calls of a procedure. *)
let callback_check_access { Callbacks.tenv; proc_desc } =
  Cfg.Procdesc.iter_instrs (report_calls_and_accesses tenv "PROC") proc_desc

(** Report all field accesses and method calls of a class. *)
let callback_check_cluster_access exe_env all_procs get_proc_desc _ =
  IList.iter (fun proc_name ->
      match get_proc_desc proc_name with
      | Some proc_desc ->
          let tenv = Exe_env.get_tenv exe_env proc_name in
          Cfg.Procdesc.iter_instrs (report_calls_and_accesses tenv "CLUSTER") proc_desc
      | _ ->
          ()
    ) all_procs

(** Looks for writeToParcel methods and checks whether read is in reverse *)
let callback_check_write_to_parcel_java
    pname_java ({ Callbacks.tenv; proc_desc; idenv; get_proc_desc } as args) =
  let verbose = ref false in

  let is_write_to_parcel this_expr this_type =
    let method_match () =
      Procname.java_get_method pname_java = "writeToParcel" in
    let expr_match () = Exp.is_this this_expr in
    let type_match () =
      let class_name =
        Typename.TN_csu (Csu.Class Csu.Java, Mangled.from_string "android.os.Parcelable") in
      match this_type with
      | Typ.Tptr (Tstruct name, _) | Tstruct name ->
          PatternMatch.is_immediate_subtype tenv name class_name
      | _ -> false in
    method_match () && expr_match () && type_match () in

  let is_parcel_constructor proc_name =
    Procname.is_constructor proc_name &&
    PatternMatch.has_formal_method_argument_type_names
      proc_desc pname_java ["android.os.Parcel"] in

  let parcel_constructors tenv typ =
    match typ with
    | Typ.Tptr (Tstruct name, _) -> (
        match Tenv.lookup tenv name with
        | Some { methods } -> IList.filter is_parcel_constructor methods
        | None -> []
      )
    | _ -> [] in

  let check r_desc w_desc =

    let is_serialization_node node =
      match Cfg.Node.get_callees node with
      | [] -> false
      | [Procname.Java pname_java] ->
          let class_name = Procname.java_get_class_name pname_java in
          let method_name = Procname.java_get_method pname_java in
          (try
             class_name = "android.os.Parcel" &&
             (String.sub method_name 0 5 = "write"
              ||
              String.sub method_name 0 4 = "read")
           with Invalid_argument _ -> false)
      | _ -> assert false in

    let is_inverse rc_ wc_ = match rc_, wc_ with
      | Procname.Java rc, Procname.Java wc ->
          let rn = Procname.java_get_method rc in
          let wn = Procname.java_get_method wc in
          let postfix_length = String.length wn - 5 in (* covers writeList <-> readArrayList etc. *)
          (try
             String.sub rn (String.length rn - postfix_length) postfix_length =
             String.sub wn 5 postfix_length
           with Invalid_argument _ -> false)
      | _ ->
          false in

    let node_to_call_desc node =
      match Cfg.Node.get_callees node with
      | [desc] -> desc
      | _ -> assert false in

    let r_call_descs =
      IList.map node_to_call_desc
        (IList.filter is_serialization_node
           (Cfg.Procdesc.get_sliced_slope r_desc is_serialization_node)) in
    let w_call_descs =
      IList.map node_to_call_desc
        (IList.filter is_serialization_node
           (Cfg.Procdesc.get_sliced_slope w_desc is_serialization_node)) in

    let rec check_match = function
      | rc:: rcs, wc:: wcs ->
          if not (is_inverse rc wc) then
            L.stdout "Serialization missmatch in %a for %a and %a@."
              Procname.pp args.Callbacks.proc_name
              Procname.pp rc
              Procname.pp wc
          else
            check_match (rcs, wcs)
      | rc:: _, [] ->
          L.stdout "Missing write in %a: for %a@."
            Procname.pp args.Callbacks.proc_name Procname.pp rc
      | _, wc:: _ ->
          L.stdout "Missing read in %a: for %a@."
            Procname.pp args.Callbacks.proc_name Procname.pp wc
      | _ ->
          () in

    check_match (r_call_descs, w_call_descs) in

  let do_instr _ instr = match instr with
    | Sil.Call (_, Exp.Const (Const.Cfun _), (_this_exp, this_type):: _, _, _) ->
        let this_exp = Idenv.expand_expr idenv _this_exp in
        if is_write_to_parcel this_exp this_type then begin
          if !verbose then
            L.stdout "Serialization check for %a@."
              Procname.pp args.Callbacks.proc_name;
          try
            match parcel_constructors tenv this_type with
            | x :: _ ->
                (match get_proc_desc x with
                 | Some x_proc_desc -> check x_proc_desc proc_desc
                 | None -> raise Not_found)
            | _ ->
                L.stdout "No parcel constructor found for %a@."
                  Procname.pp args.Callbacks.proc_name
          with Not_found ->
            if !verbose then L.stdout "Methods not available@."
        end
    | _ -> () in
  Cfg.Procdesc.iter_instrs do_instr proc_desc

(** Looks for writeToParcel methods and checks whether read is in reverse *)
let callback_check_write_to_parcel ({ Callbacks.proc_name } as args) =
  match proc_name with
  | Procname.Java pname_java ->
      callback_check_write_to_parcel_java pname_java args
  | _ ->
      ()

(** Monitor calls to Preconditions.checkNotNull and detect inconsistent uses. *)
let callback_monitor_nullcheck { Callbacks.proc_desc; idenv; proc_name } =
  let verbose = ref false in

  let class_formal_names = lazy (
    let formals = Cfg.Procdesc.get_formals proc_desc in
    let class_formals =
      let is_class_type (p, typ) =
        match typ with
        | Typ.Tptr _ when Mangled.to_string p = "this" ->
            false (* no need to null check 'this' *)
        | Typ.Tstruct _ -> true
        | Typ.Tptr (Typ.Tstruct _, _) -> true
        | _ -> false in
      IList.filter is_class_type formals in
    IList.map fst class_formals) in
  let equal_formal_param exp formal_name = match exp with
    | Exp.Lvar pvar ->
        let name = Pvar.get_name pvar in
        Mangled.equal name formal_name
    | _ -> false in

  let is_formal_param exp =
    IList.exists (equal_formal_param exp) (Lazy.force class_formal_names) in

  let is_nullcheck pn = match pn with
    | Procname.Java pn_java ->
        PatternMatch.java_proc_name_with_class_method
          pn_java "com.google.common.base.Preconditions" "checkNotNull"
    | _ ->
        false in

  let checks_to_formals = ref Exp.Set.empty in

  let handle_check_of_formal e =
    let repeated = Exp.Set.mem e !checks_to_formals in
    if repeated && !verbose then L.stdout "Repeated Null Check of Formal: %a@." Exp.pp e
    else begin
      checks_to_formals := Exp.Set.add e !checks_to_formals;
      if !verbose then L.stdout "Null Check of Formal: %a@." Exp.pp e
    end in

  let summary_checks_of_formals () =
    let formal_names = Lazy.force class_formal_names in
    let nchecks = Exp.Set.cardinal !checks_to_formals in
    let nformals = IList.length formal_names in
    if (nchecks > 0 && nchecks < nformals) then
      begin
        let was_not_found formal_name =
          not (Exp.Set.exists (fun exp -> equal_formal_param exp formal_name) !checks_to_formals) in
        let missing = IList.filter was_not_found formal_names in
        let loc = Cfg.Procdesc.get_loc proc_desc in
        let pp_file_loc fmt () =
          F.fprintf fmt "%s:%d" (DB.source_file_to_string loc.Location.file) loc.Location.line in
        L.stdout "Null Checks of Formal Parameters: ";
        L.stdout "%d out of %d parameters checked (missing checks on: %a)[%a]@."
          nchecks nformals (pp_seq Mangled.pp) missing pp_file_loc ();

        let linereader = Printer.LineReader.create () in
        L.stdout "%a@." (PP.pp_loc_range linereader 10 10) loc
      end in

  let do_instr _ instr = match instr with
    | Sil.Call (_, Exp.Const (Const.Cfun pn), (_arg1, _):: _, _, _) when is_nullcheck pn ->
        let arg1 = Idenv.expand_expr idenv _arg1 in
        if is_formal_param arg1 then handle_check_of_formal arg1;
        if !verbose then
          (match proc_name with
           | Procname.Java pname_java ->
               L.stdout "call in %s %s: %a with first arg: %a@."
                 (Procname.java_get_class_name pname_java)
                 (Procname.java_get_method pname_java)
                 (Sil.pp_instr pe_text) instr
                 Exp.pp arg1
           | _ ->
               ())
    | _ -> () in
  Cfg.Procdesc.iter_instrs do_instr proc_desc;
  summary_checks_of_formals ()

(** Test persistent state. *)
let callback_test_state { Callbacks.proc_name } =
  ST.pname_add proc_name "somekey" "somevalue"

(** Check the uses of VisibleForTesting *)
let callback_checkVisibleForTesting { Callbacks.proc_desc } =
  if Annotations.pdesc_has_annot proc_desc Annotations.visibleForTesting then
    begin
      let loc = Cfg.Procdesc.get_loc proc_desc in
      let linereader = Printer.LineReader.create () in
      L.stdout "%a@." (PP.pp_loc_range linereader 10 10) loc
    end

(** Check for readValue and readValueAs json deserialization *)
let callback_find_deserialization { Callbacks.proc_desc; get_proc_desc; idenv; proc_name } =
  let verbose = true in

  let ret_const_key = "return_const" in

  let reverse_find_instr f node =
    (* this is not really sound but for the moment a sufficient approximation *)
    let has_instr node =
      try ignore(IList.find f (Cfg.Node.get_instrs node)); true
      with Not_found -> false in
    let preds = Cfg.Node.get_generated_slope node (fun n -> Cfg.Node.get_sliced_preds n has_instr) in
    let instrs = IList.flatten (IList.map (fun n -> IList.rev (Cfg.Node.get_instrs n)) preds) in
    try
      Some (IList.find f instrs)
    with Not_found -> None in

  let get_return_const proc_name' =
    try
      ST.pname_find proc_name' ret_const_key
    with Not_found ->
    match get_proc_desc proc_name' with
      Some proc_desc' ->
        let is_return_instr = function
          | Sil.Store (Exp.Lvar p, _, _, _)
            when Pvar.equal p (Cfg.Procdesc.get_ret_var proc_desc') -> true
          | _ -> false in
        (match reverse_find_instr is_return_instr (Cfg.Procdesc.get_exit_node proc_desc') with
         | Some (Sil.Store (_, _, Exp.Const (Const.Cclass n), _)) -> Ident.name_to_string n
         | _ -> "<" ^ (Procname.to_string proc_name') ^ ">")
    | None -> "?" in

  let get_actual_arguments node instr = match instr with
    | Sil.Call (_, Exp.Const (Const.Cfun _), _:: args, _, _) ->
        (try
           let find_const exp =
             let expanded = Idenv.expand_expr idenv exp in
             match expanded with
             | Exp.Const (Const.Cclass n) -> Ident.name_to_string n
             | Exp.Lvar _ -> (
                 let is_call_instr set call = match set, call with
                   | Sil.Store (_, _, Exp.Var (i1), _), Sil.Call (Some (i2, _), _, _, _, _)
                     when Ident.equal i1 i2 -> true
                   | _ -> false in
                 let is_set_instr = function
                   | Sil.Store (e1, _, _, _) when Exp.equal expanded e1 -> true
                   | _ -> false in
                 match reverse_find_instr is_set_instr node with
                 (* Look for ivar := tmp *)
                 | Some s -> (
                     match reverse_find_instr (is_call_instr s) node with
                     (* Look for tmp := foo() *)
                     | Some (Sil.Call (_, Exp.Const (Const.Cfun pn), _, _, _)) ->
                         get_return_const pn
                     | _ -> "?")
                 | _ -> "?")
             | _ -> "?" in
           let arg_name (exp, _) = find_const exp in
           Some (IList.map arg_name args)
         with _ -> None)
    | _ -> None in

  let process_result instr result =
    if verbose then (
      let linereader = Printer.LineReader.create () in
      L.stdout "%a@." (PP.pp_loc_range linereader 2 2) (Sil.instr_get_loc instr);
    );
    match result with
    | str when (Str.string_match (Str.regexp "<\\(.*\\)>") str 0) -> (
        let missing_proc_name = Str.matched_group 1 str in
        L.stdout "Deserialization of %s requires 2nd phase: " str;
        L.stdout "missing: %s@." missing_proc_name)
    | "?" -> L.stdout "Unable to resolve deserialization\n\n@."
    | _ -> L.stdout "Deserialization of %s\n\n@." result in

  let do_instr node instr =
    match PatternMatch.get_java_method_call_formal_signature instr with
    | Some (_, "readValue", _, _) -> (
        match get_actual_arguments node instr with
        | Some [_; cl] -> process_result instr cl
        | _ -> process_result instr "?")
    | Some (_, "readValueAs", _, _) -> (
        match get_actual_arguments node instr with
        | Some [cl] -> process_result instr cl
        | _ -> process_result instr "?")
    | _ -> () in

  let store_return () =
    let ret_const = get_return_const proc_name in
    ST.pname_add proc_name ret_const_key ret_const in

  store_return ();
  Cfg.Procdesc.iter_instrs do_instr proc_desc

(** Check field accesses. *)
let callback_check_field_access { Callbacks.proc_desc } =
  let rec do_exp is_read = function
    | Exp.Var _ -> ()
    | Exp.UnOp (_, e, _) ->
        do_exp is_read e
    | Exp.BinOp (_, e1, e2) ->
        do_exp is_read e1;
        do_exp is_read e2
    | Exp.Exn _ -> ()
    | Exp.Closure _ -> ()
    | Exp.Const _ -> ()
    | Exp.Cast (_, e) ->
        do_exp is_read e
    | Exp.Lvar _ -> ()
    | Exp.Lfield (e, fn, _) ->
        if not (Ident.java_fieldname_is_outer_instance fn) then
          L.stdout "field %s %s@." (Ident.fieldname_to_string fn) (if is_read then "reading" else "writing");
        do_exp is_read e
    | Exp.Lindex (e1, e2) ->
        do_exp is_read e1;
        do_exp is_read e2
    | Exp.Sizeof _ -> () in
  let do_read_exp = do_exp true in
  let do_write_exp = do_exp false in
  let do_instr _ = function
    | Sil.Load (_, e, _, _) ->
        do_read_exp e
    | Sil.Store (e1, _, e2, _) ->
        do_write_exp e1;
        do_read_exp e2
    | Sil.Prune (e, _, _, _) ->
        do_read_exp e
    | Sil.Call (_, e, etl, _, _) ->
        do_read_exp e;
        IList.iter (fun (e, _) -> do_read_exp e) etl
    | Sil.Nullify _
    | Sil.Abstract _
    | Sil.Remove_temps _
    | Sil.Declare_locals _ ->
        () in
  Cfg.Procdesc.iter_instrs do_instr proc_desc

(** Print c method calls. *)
let callback_print_c_method_calls { Callbacks.tenv; proc_desc; proc_name } =
  let do_instr node = function
    | Sil.Call (_, Exp.Const (Const.Cfun pn), (e, _):: _, loc, _)
      when Procname.is_c_method pn ->
        let receiver = match Errdesc.exp_rv_dexp tenv node e with
          | Some de -> DecompiledExp.to_string de
          | None -> "?" in
        let description =
          Printf.sprintf "['%s' %s]" receiver (Procname.to_string pn) in
        ST.report_error tenv
          proc_name
          proc_desc
          "CHECKERS_PRINT_OBJC_METHOD_CALLS"
          loc
          description
    | Sil.Call (_, Exp.Const (Const.Cfun pn), _, loc, _) ->
        let description =
          Printf.sprintf "call to %s" (Procname.to_string pn) in
        ST.report_error tenv
          proc_name
          proc_desc
          "CHECKERS_PRINT_C_CALL"
          loc
          description
    | _ -> () in
  Cfg.Procdesc.iter_instrs do_instr proc_desc

(** Print access to globals. *)
let callback_print_access_to_globals { Callbacks.tenv; proc_desc; proc_name } =
  let do_pvar is_read pvar loc =
    let description =
      Printf.sprintf "%s of global %s"
        (if is_read then "read" else "write")
        (Pvar.to_string pvar) in
    ST.report_error tenv
      proc_name
      proc_desc
      "CHECKERS_ACCESS_GLOBAL"
      loc
      description in
  let rec get_global_var = function
    | Exp.Lvar pvar when Pvar.is_global pvar ->
        Some pvar
    | Exp.Lfield (e, _, _) ->
        get_global_var e
    | _ ->
        None in
  let do_instr _ = function
    | Sil.Load (_, e, _, loc) when get_global_var e <> None ->
        Option.may (fun pvar -> do_pvar true pvar loc) (get_global_var e)
    | Sil.Store (e, _, _, loc) when get_global_var e <> None ->
        Option.may (fun pvar -> do_pvar false pvar loc) (get_global_var e)
    | _ -> () in
  Cfg.Procdesc.iter_instrs do_instr proc_desc
