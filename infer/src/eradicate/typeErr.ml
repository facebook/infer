(*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module L = Logging
module P = Printf

(** Module for Type Error messages. *)


(** Describe the origin of values propagated by the checker. *)
module type InstrRefT =
sig
  type t
  type generator
  val create_generator : Cfg.Node.t -> generator
  val equal : t -> t -> bool
  val gen : generator -> t
  val get_node : t -> Cfg.Node.t
  val hash : t -> int
  val replace_node : t -> Cfg.Node.t -> t
end (* InstrRefT *)


(** Per-node instruction reference. *)
module InstrRef : InstrRefT =
struct
  type t = Cfg.Node.t * int
  type generator = Cfg.Node.t * int ref
  let equal (n1, i1) (n2, i2) =
    Cfg.Node.equal n1 n2 && i1 = i2
  let hash (n, i) = Hashtbl.hash (Cfg.Node.hash n, i)
  let get_node (n, _) = n
  let replace_node (_, i) n' = (n', i)
  let create_generator n = (n, ref 0)
  let gen instr_ref_gen =
    let (node, ir) = instr_ref_gen in
    incr ir;
    (node, !ir)
end (* InstrRef *)


type origin_descr =
  string *
  Location.t option *
  Annotations.annotated_signature option  (* callee signature *)

type parameter_not_nullable =
  Annotations.annotation *
  string * (* description *)
  int * (* parameter number *)
  Procname.t *
  Location.t * (* callee location *)
  origin_descr

(** Instance of an error *)
type err_instance =
  | Condition_redundant of (bool * (string option) * bool)
  | Inconsistent_subclass_return_annotation of Procname.t * Procname.t
  | Inconsistent_subclass_parameter_annotation of string * int * Procname.t * Procname.t
  | Field_not_initialized of Ident.fieldname * Procname.t
  | Field_not_mutable of Ident.fieldname * origin_descr
  | Field_annotation_inconsistent of Annotations.annotation * Ident.fieldname * origin_descr
  | Field_over_annotated of Ident.fieldname * Procname.t
  | Null_field_access of string option * Ident.fieldname * origin_descr * bool
  | Call_receiver_annotation_inconsistent
    of Annotations.annotation * string option * Procname.t * origin_descr
  | Parameter_annotation_inconsistent of parameter_not_nullable
  | Return_annotation_inconsistent of Annotations.annotation * Procname.t * origin_descr
  | Return_over_annotated of Procname.t

module H = Hashtbl.Make(struct
    type t = err_instance * InstrRef.t option
    let err_instance_equal x y = match x, y with
      | Condition_redundant (b1, so1, nn1), Condition_redundant (b2, so2, nn2) ->
          bool_equal b1 b2 &&
          (opt_equal string_equal) so1 so2 &&
          bool_equal nn1 nn2
      | Condition_redundant _, _
      | _, Condition_redundant _ -> false
      | Field_not_initialized (fn1, pn1), Field_not_initialized (fn2, pn2) ->
          Ident.fieldname_equal fn1 fn2 &&
          Procname.equal pn1 pn2
      | Field_not_initialized (_, _), _
      | _, Field_not_initialized (_, _) -> false
      | Field_not_mutable (fn1, _), Field_not_mutable (fn2, _) ->
          Ident.fieldname_equal fn1 fn2
      | Field_not_mutable _, _
      | _, Field_not_mutable _ -> false
      | Field_annotation_inconsistent (ann1, fn1, _),
        Field_annotation_inconsistent (ann2, fn2, _) ->
          ann1 = ann2 &&
          Ident.fieldname_equal fn1 fn2
      | Field_annotation_inconsistent _, _
      | _, Field_annotation_inconsistent _ -> false
      | Field_over_annotated (fn1, pn1), Field_over_annotated (fn2, pn2) ->
          Ident.fieldname_equal fn1 fn2 &&
          Procname.equal pn1 pn2
      | Field_over_annotated (_, _), _
      | _, Field_over_annotated (_, _) -> false
      | Null_field_access (so1, fn1, _, ii1), Null_field_access (so2, fn2, _, ii2) ->
          (opt_equal string_equal) so1 so2 &&
          Ident.fieldname_equal fn1 fn2 &&
          bool_equal ii1 ii2
      | Null_field_access _, _
      | _, Null_field_access _ -> false
      | Call_receiver_annotation_inconsistent (ann1, so1, pn1, _),
        Call_receiver_annotation_inconsistent (ann2, so2, pn2, _) ->
          ann1 = ann2 &&
          (opt_equal string_equal) so1 so2 &&
          Procname.equal pn1 pn2
      | Call_receiver_annotation_inconsistent _, _
      | _, Call_receiver_annotation_inconsistent _ -> false
      | Parameter_annotation_inconsistent (ann1, s1, n1, pn1, cl1, _),
        Parameter_annotation_inconsistent (ann2, s2, n2, pn2, cl2, _) ->
          ann1 = ann2 &&
          string_equal s1 s2 &&
          int_equal n1 n2 &&
          Procname.equal pn1 pn2 &&
          Location.equal cl1 cl2
      | Parameter_annotation_inconsistent _, _
      | _, Parameter_annotation_inconsistent _ -> false
      | Return_annotation_inconsistent (ann1, pn1, _),
        Return_annotation_inconsistent (ann2, pn2, _) ->
          ann1 = ann2 && Procname.equal pn1 pn2
      | Return_annotation_inconsistent _, _
      | _, Return_annotation_inconsistent _ -> false
      | Return_over_annotated pn1, Return_over_annotated pn2 ->
          Procname.equal pn1 pn2
      | Inconsistent_subclass_return_annotation (pn1, spn1),
        Inconsistent_subclass_return_annotation (pn2, spn2) ->
          if Procname.equal pn1 pn2 then true
          else Procname.equal spn1 spn2
      | Inconsistent_subclass_parameter_annotation (param_name_1, pos_1, pn_1, overriden_pn_1),
        Inconsistent_subclass_parameter_annotation (param_name_2, pos_2, pn_2, overriden_pn_2) ->
          string_equal param_name_1 param_name_2 &&
          int_equal pos_1 pos_2 &&
          Procname.equal pn_1 pn_2 &&
          Procname.equal overriden_pn_1 overriden_pn_2
      | Inconsistent_subclass_return_annotation _, _
      | _, Inconsistent_subclass_return_annotation _ -> false
      | Inconsistent_subclass_parameter_annotation _, _
      | _, Inconsistent_subclass_parameter_annotation _ -> false

    let equal (err_inst1, instr_ref_opt1) (err_inst2, instr_ref_opt2) =
      err_instance_equal err_inst1 err_inst2 &&
      opt_equal InstrRef.equal instr_ref_opt1 instr_ref_opt2

    let err_instance_hash x =
      let string_hash s = Hashtbl.hash s in
      let string_opt_hash so = Hashtbl.hash so in
      match x with
      | Condition_redundant (b, so, nn) ->
          Hashtbl.hash (1, b, string_opt_hash so, nn)
      | Field_not_initialized (fn, pn) ->
          Hashtbl.hash (2, string_hash ((Ident.fieldname_to_string fn) ^ (Procname.to_string pn)))
      | Field_not_mutable (fn, _) ->
          Hashtbl.hash (3, string_hash (Ident.fieldname_to_string fn))
      | Field_annotation_inconsistent (ann, fn, _) ->
          Hashtbl.hash (4, ann, string_hash (Ident.fieldname_to_string fn))
      | Field_over_annotated (fn, pn) ->
          Hashtbl.hash (5, string_hash ((Ident.fieldname_to_string fn) ^ (Procname.to_string pn)))
      | Null_field_access (so, fn, _, _) ->
          Hashtbl.hash (6, string_opt_hash so, string_hash (Ident.fieldname_to_string fn))
      | Call_receiver_annotation_inconsistent (ann, so, pn, _) ->
          Hashtbl.hash (7, ann, string_opt_hash so, Procname.hash_pname pn)
      | Parameter_annotation_inconsistent (ann, s, n, pn, _, _) ->
          Hashtbl.hash (8, ann, string_hash s, n, Procname.hash_pname pn)
      | Return_annotation_inconsistent (ann, pn, _) ->
          Hashtbl.hash (9, ann, Procname.hash_pname pn)
      | Return_over_annotated pn ->
          Hashtbl.hash (10, Procname.hash_pname pn)
      | Inconsistent_subclass_return_annotation (pn, opn) ->
          Hashtbl.hash (11, Procname.hash_pname pn, Procname.hash_pname opn)
      | Inconsistent_subclass_parameter_annotation (param_name, pos, pn, opn) ->
          let pn_hash = string_hash param_name in
          Hashtbl.hash (12, pn_hash, pos, Procname.hash_pname pn, Procname.hash_pname opn)

    let hash (err_inst, instr_ref_opt) =
      let x = match instr_ref_opt with
        | None -> None
        | Some instr_ref -> Some (InstrRef.hash instr_ref) in
      let y = err_instance_hash err_inst in
      Hashtbl.hash (x, y)
  end (* H *))

type err_state = {
  loc: Location.t; (** location of the error *)
  mutable always: bool; (** always fires on its associated node *)
}

let err_tbl : err_state H.t =
  H.create 1

(** Reset the error table. *)
let reset () = H.reset err_tbl

(** Get the forall status of an err_instance.
    The forall status indicates that the error should be printed only if it
    occurs on every path. *)
let get_forall = function
  | Condition_redundant _ -> true
  | Field_not_initialized _ -> false
  | Field_not_mutable _ -> false
  | Field_annotation_inconsistent _ -> false
  | Field_over_annotated _ -> false
  | Inconsistent_subclass_return_annotation _ -> false
  | Inconsistent_subclass_parameter_annotation _ -> false
  | Null_field_access _ -> false
  | Call_receiver_annotation_inconsistent _ -> false
  | Parameter_annotation_inconsistent _ -> false
  | Return_annotation_inconsistent _ -> false
  | Return_over_annotated _ -> false


(** Reset the always field of the forall erros in the node, so if they are not set again
    we know that they don't fire on every path. *)
let node_reset_forall node =
  let iter (err_instance, instr_ref_opt) err_state =
    match instr_ref_opt, get_forall err_instance with
    | Some instr_ref, is_forall ->
        let node' = InstrRef.get_node instr_ref in
        if is_forall && Cfg.Node.equal node node' then err_state.always <- false
    | None, _ -> () in
  H.iter iter err_tbl

(** Add an error to the error table and return whether it should be printed now. *)
let add_err find_canonical_duplicate err_instance instr_ref_opt loc =
  let is_forall = get_forall err_instance in
  if H.mem err_tbl (err_instance, instr_ref_opt)
  then false (* don't print now *)
  else begin
    let instr_ref_opt_deduplicate =
      match is_forall, instr_ref_opt with
      | true, Some instr_ref -> (* use canonical duplicate for forall checks *)
          let node = InstrRef.get_node instr_ref in
          let canonical_node = find_canonical_duplicate node in
          let instr_ref' = InstrRef.replace_node instr_ref canonical_node in
          Some instr_ref'
      | _ -> instr_ref_opt in
    H.add err_tbl (err_instance, instr_ref_opt_deduplicate) { loc = loc; always = true };
    not is_forall (* print now if it's not a forall check *)
  end

module Strict = struct
  let method_get_strict signature =
    let (ia, _) = signature.Annotations.ret in
    Annotations.ia_get_strict ia

  let this_type_get_strict tenv signature =
    match signature.Annotations.params with
    | (p, _, this_type):: _ when Mangled.to_string p = "this" ->
        begin
          match PatternMatch.type_get_annotation tenv this_type with
          | Some ia -> Annotations.ia_get_strict ia
          | None -> None
        end
    | _ -> None

  let signature_get_strict tenv signature =
    match method_get_strict signature with
    | None -> this_type_get_strict tenv signature
    | Some x -> Some x

  let origin_descr_get_strict tenv origin_descr = match origin_descr with
    | _, _, Some signature ->
        signature_get_strict tenv signature
    | _, _, None ->
        None

  let report_on_method_arguments = false

  (* Return (Some parameters) if there is a method call on a @Nullable object,*)
  (* where the origin of @Nullable in the analysis is the return value of a Strict method*)
  (* with parameters. A method is Strict if it or its class are annotated @Strict. *)
  let err_instance_get_strict tenv err_instance : Annot.t option =
    match err_instance with
    | Call_receiver_annotation_inconsistent (Annotations.Nullable, _, _, origin_descr)
    | Null_field_access (_, _, origin_descr, _) ->
        origin_descr_get_strict tenv origin_descr
    | Parameter_annotation_inconsistent (Annotations.Nullable, _, _, _, _, origin_descr)
      when report_on_method_arguments ->
        origin_descr_get_strict tenv origin_descr
    | _ -> None
end (* Strict *)

type st_report_error =
  Procname.t ->
  Cfg.Procdesc.t ->
  string ->
  Location.t ->
  ?advice: string option ->
  ?field_name: Ident.fieldname option ->
  ?origin_loc: Location.t option ->
  ?exception_kind: (string -> Localise.error_desc -> exn) ->
  ?always_report: bool ->
  string ->
  unit

(** Report an error right now. *)
let report_error_now tenv
    (st_report_error : st_report_error) node err_instance loc pname : unit =
  let demo_mode = true in
  let do_print_base ew_string kind_s s =
    let mname = match pname with
      | Procname.Java pname_java ->
          Procname.java_get_method pname_java
      | _ ->
          Procname.to_simplified_string pname in
    L.stdout "%s %s in %s %s@." ew_string kind_s mname s in
  let do_print ew_string kind_s s =
    L.stdout "%s:%d " (DB.source_file_to_string loc.Location.file) loc.Location.line;
    do_print_base ew_string kind_s s in
  let do_print_demo ew_string kind_s s = (* demo mode print for Eclipse and ocaml plugin *)
    let process_path s = filename_to_relative (Sys.getcwd ()) s in
    L.stdout
      "File %s, line %d, characters 0-10:\n"
      (process_path (DB.source_file_to_string loc.Location.file))
      loc.Location.line;
    do_print_base ew_string kind_s s in

  let is_err, kind_s, description, advice, field_name, origin_loc = match err_instance with
    | Condition_redundant (b, s_opt, nonnull) ->
        let name =
          if nonnull
          then "ERADICATE_CONDITION_REDUNDANT_NONNULL"
          else "ERADICATE_CONDITION_REDUNDANT" in
        false,
        name,
        P.sprintf
          "The condition %s is always %b according to the existing annotations."
          (string_value_or_empty_string s_opt)
          b,
        Some "Consider adding a `@Nullable` annotation or removing the redundant check.",
        None,
        None
    | Field_not_initialized (fn, pn) ->
        let constructor_name =
          if Procname.is_constructor pn
          then "the constructor"
          else
            match pn with
            | Procname.Java pn_java ->
                Procname.java_get_method pn_java
            | _ ->
                Procname.to_simplified_string pn in
        true,
        "ERADICATE_FIELD_NOT_INITIALIZED",
        P.sprintf
          "Field `%s` is not initialized in %s and is not declared `@Nullable`"
          (Ident.fieldname_to_simplified_string fn)
          constructor_name,
        None,
        Some fn,
        None
    | Field_not_mutable (fn, (origin_description, origin_loc, _)) ->
        true,
        "ERADICATE_FIELD_NOT_MUTABLE",
        P.sprintf
          "Field `%s` is modified but is not declared `@Mutable`. %s"
          (Ident.fieldname_to_simplified_string fn)
          origin_description,
        None,
        None,
        origin_loc
    | Field_annotation_inconsistent (ann, fn, (origin_description, origin_loc, _)) ->
        let kind_s, description = match ann with
          | Annotations.Nullable ->
              "ERADICATE_FIELD_NOT_NULLABLE",
              P.sprintf
                "Field `%s` can be null but is not declared `@Nullable`. %s"
                (Ident.fieldname_to_simplified_string fn)
                origin_description
          | Annotations.Present ->
              "ERADICATE_FIELD_VALUE_ABSENT",
              P.sprintf
                "Field `%s` is assigned a possibly absent value but is declared `@Present`. %s"
                (Ident.fieldname_to_simplified_string fn)
                origin_description in
        true,
        kind_s,
        description,
        None,
        None,
        origin_loc
    | Field_over_annotated (fn, pn) ->
        let constructor_name =
          if Procname.is_constructor pn
          then "the constructor"
          else
            match pn with
            | Procname.Java pn_java ->
                Procname.java_get_method pn_java
            | _ ->
                Procname.to_simplified_string pn in
        true,
        "ERADICATE_FIELD_OVER_ANNOTATED",
        P.sprintf
          "Field `%s` is always initialized in %s but is declared `@Nullable`"
          (Ident.fieldname_to_simplified_string fn)
          constructor_name,
        None,
        Some fn,
        None
    | Null_field_access (s_opt, fn, (origin_description, origin_loc, _), indexed) ->
        let at_index = if indexed then "element at index" else "field" in
        true,
        "ERADICATE_NULL_FIELD_ACCESS",
        P.sprintf
          "Object `%s` could be null when accessing %s `%s`. %s"
          (string_value_or_empty_string s_opt)
          at_index
          (Ident.fieldname_to_simplified_string fn)
          origin_description,
        None,
        None,
        origin_loc
    | Call_receiver_annotation_inconsistent (ann, s_opt, pn, (origin_description, origin_loc, _)) ->
        let kind_s, description = match ann with
          | Annotations.Nullable ->
              "ERADICATE_NULL_METHOD_CALL",
              P.sprintf
                "The value of `%s` in the call to `%s` could be null. %s"
                (string_value_or_empty_string s_opt)
                (Procname.to_simplified_string pn)
                origin_description
          | Annotations.Present ->
              "ERADICATE_VALUE_NOT_PRESENT",
              P.sprintf
                "The value of `%s` in the call to `%s` is not @Present. %s"
                (string_value_or_empty_string s_opt)
                (Procname.to_simplified_string pn)
                origin_description in
        true,
        kind_s,
        description,
        None,
        None,
        origin_loc
    | Parameter_annotation_inconsistent (ann, s, n, pn, _, (origin_desc, origin_loc, _)) ->
        let kind_s, description = match ann with
          | Annotations.Nullable ->
              "ERADICATE_PARAMETER_NOT_NULLABLE",
              P.sprintf
                "`%s` needs a non-null value in parameter %d but argument `%s` can be null. %s"
                (Procname.to_simplified_string pn)
                n
                s
                origin_desc
          | Annotations.Present ->
              "ERADICATE_PARAMETER_VALUE_ABSENT",
              P.sprintf
                "`%s` needs a present value in parameter %d but argument `%s` can be absent. %s"
                (Procname.to_simplified_string pn)
                n
                s
                origin_desc in
        true,
        kind_s,
        description,
        None,
        None,
        origin_loc
    | Return_annotation_inconsistent (ann, pn, (origin_description, origin_loc, _)) ->
        let kind_s, description = match ann with
          | Annotations.Nullable ->
              "ERADICATE_RETURN_NOT_NULLABLE",
              P.sprintf
                "Method `%s` may return null but it is not annotated with `@Nullable`. %s"
                (Procname.to_simplified_string pn)
                origin_description
          | Annotations.Present ->
              "ERADICATE_RETURN_VALUE_NOT_PRESENT",
              P.sprintf
                "Method `%s` may return an absent value but it is annotated with `@Present`. %s"
                (Procname.to_simplified_string pn)
                origin_description in
        true,
        kind_s,
        description,
        None,
        None,
        origin_loc
    | Return_over_annotated pn ->
        false,
        "ERADICATE_RETURN_OVER_ANNOTATED",
        P.sprintf
          "Method `%s` is annotated with `@Nullable` but never returns null."
          (Procname.to_simplified_string pn),
        None,
        None,
        None
    | Inconsistent_subclass_return_annotation (pn, opn) ->
        false,
        "ERADICATE_INCONSISTENT_SUBCLASS_RETURN_ANNOTATION",
        P.sprintf
          "Method `%s` is annotated with `@Nullable` but overrides unannotated method `%s`."
          (Procname.to_simplified_string ~withclass: true pn)
          (Procname.to_simplified_string ~withclass: true opn),
        None,
        None,
        None
    | Inconsistent_subclass_parameter_annotation (param_name, pos, pn, opn) ->
        let translate_position = function
          | 1 -> "First"
          | 2 -> "Second"
          | 3 -> "Third"
          | n -> (string_of_int n)^"th" in
        false,
        "ERADICATE_INCONSISTENT_SUBCLASS_PARAMETER_ANNOTATION",
        P.sprintf
          "%s parameter `%s` of method `%s` is not `@Nullable` but is declared `@Nullable`\
           in the parent class method `%s`."
          (translate_position pos) param_name
          (Procname.to_simplified_string ~withclass: true pn)
          (Procname.to_simplified_string ~withclass: true opn),
        None,
        None,
        None in
  let ew_string = if is_err then "Error" else "Warning" in
  (if demo_mode then do_print_demo else do_print) ew_string kind_s description;
  let always_report = Strict.err_instance_get_strict tenv err_instance <> None in
  st_report_error
    pname
    (Cfg.Node.get_proc_desc node)
    kind_s
    loc
    ~advice
    ~field_name
    ~origin_loc
    ~exception_kind: (fun k d -> Exceptions.Eradicate (k, d))
    ~always_report
    description


(** Report an error unless is has been reported already, or unless it's a forall error
    since it requires waiting until the end of the analysis and be printed by flush. *)
let report_error tenv (st_report_error : st_report_error) find_canonical_duplicate node
    err_instance instr_ref_opt loc pname_java =
  let should_report_now =
    add_err find_canonical_duplicate err_instance instr_ref_opt loc in
  if should_report_now then
    report_error_now tenv st_report_error node err_instance loc pname_java

(** Report the forall checks at the end of the analysis and reset the error table *)
let report_forall_checks_and_reset tenv st_report_error proc_name =
  let iter (err_instance, instr_ref_opt) err_state =
    match instr_ref_opt, get_forall err_instance with
    | Some instr_ref, is_forall ->
        let node = InstrRef.get_node instr_ref in
        State.set_node node;
        if is_forall && err_state.always
        then report_error_now tenv st_report_error node err_instance err_state.loc proc_name
    | None, _ -> () in
  H.iter iter err_tbl;
  reset ()
