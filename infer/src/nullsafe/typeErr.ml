(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Hashtbl = Caml.Hashtbl
module MF = MarkupFormatter
module P = Printf
module F = Format

(** Module for Type Error messages. *)

(** Describe the origin of values propagated by the checker. *)
module type InstrRefT = sig
  type t [@@deriving compare]

  val equal : t -> t -> bool

  type generator

  val create_generator : Procdesc.Node.t -> generator

  val gen : generator -> t

  val get_node : t -> Procdesc.Node.t

  val hash : t -> int

  val replace_node : t -> Procdesc.Node.t -> t
end

(* InstrRefT *)

(** Per-node instruction reference. *)
module InstrRef : InstrRefT = struct
  type t = Procdesc.Node.t * int [@@deriving compare]

  let equal = [%compare.equal: t]

  type generator = Procdesc.Node.t * int ref

  let hash (n, i) = Hashtbl.hash (Procdesc.Node.hash n, i)

  let get_node (n, _) = n

  let replace_node (_, i) n' = (n', i)

  let create_generator n = (n, ref 0)

  let gen instr_ref_gen =
    let node, ir = instr_ref_gen in
    incr ir ;
    (node, !ir)
end

(* InstrRef *)

(** Instance of an error *)
type err_instance =
  | Condition_redundant of
      { loc: Location.t
      ; is_always_true: bool
      ; condition_descr: string option
      ; nonnull_origin: TypeOrigin.t }
  | Inconsistent_subclass of
      { loc: Location.t
      ; inheritance_violation: InheritanceRule.violation
      ; violation_type: InheritanceRule.ReportableViolation.violation_type
      ; base_proc_name: Procname.Java.t
      ; overridden_proc_name: Procname.Java.t }
  | Field_not_initialized of {loc: Location.t; field_name: Fieldname.t}
  | Over_annotation of
      { loc: Location.t
      ; over_annotated_violation: OverAnnotatedRule.violation
      ; violation_type: OverAnnotatedRule.violation_type }
  | Nullable_dereference of
      { dereference_violation: DereferenceRule.violation
      ; dereference_location: Location.t
      ; dereference_type: DereferenceRule.ReportableViolation.dereference_type
      ; nullable_object_descr: string option }
  | Bad_assignment of
      { assignment_violation: AssignmentRule.violation
      ; assignment_location: Location.t
      ; assignment_type: AssignmentRule.ReportableViolation.assignment_type }
[@@deriving compare]

(** Returns whether we can be certain that an [err_instance] is related to synthetic code,
    autogen/codegen).

    NOTE: This implementation is very naive and handles only simplest cases (which is actually
    enough for our use-case at the moment). The problem here is that in general:

    - any expression that has synthetic code inside can be considered "tainted",
    - any expression with a "tainted" sub-expression is also "tainted",
    - with "tainted" expressions all bets are off and error reporting should be suppressed.

    We could handle this properly by introducing [Nullability.Synthetic] but this is quite involved
    (and hopefully won't be needed). *)
let is_synthetic_err = function
  | Bad_assignment {assignment_type; _} -> (
      AssignmentRule.ReportableViolation.(
        match assignment_type with
        | PassingParamToFunction {procname; _} ->
            (* NOTE: this currently doesn't cover the case when the actual
               argument involves syntethic code *)
            Procname.Java.is_autogen_method procname
        | AssigningToField fieldname ->
            Fieldname.is_java_synthetic fieldname
        | _ ->
            false) )
  | _ ->
      false


let pp_err_instance fmt err_instance =
  match err_instance with
  | Condition_redundant _ ->
      F.pp_print_string fmt "Condition_redundant"
  | Inconsistent_subclass _ ->
      F.pp_print_string fmt "Inconsistent_subclass"
  | Field_not_initialized _ ->
      F.pp_print_string fmt "Field_not_initialized"
  | Over_annotation _ ->
      F.pp_print_string fmt "Over_annotation"
  | Nullable_dereference _ ->
      F.pp_print_string fmt "Nullable_dereference"
  | Bad_assignment _ ->
      F.fprintf fmt "Bad_assignment"


module H = Hashtbl.Make (struct
  type t = err_instance * InstrRef.t option [@@deriving compare]

  let equal = [%compare.equal: t]

  let hash = Hashtbl.hash
end
(* H *))

type err_state = {mutable always: bool  (** always fires on its associated node *)}

let err_tbl : err_state H.t = H.create 1

(** Reset the error table. *)
let reset () = H.reset err_tbl

(** Get the forall status of an err_instance. The forall status indicates that the error should be
    printed only if it occurs on every path. *)
let get_forall = function
  | Condition_redundant _ ->
      true
  | Field_not_initialized _ ->
      false
  | Over_annotation _ ->
      false
  | Bad_assignment _ ->
      false
  | Inconsistent_subclass _ ->
      false
  | Nullable_dereference _ ->
      false


(** Reset the always field of the forall erros in the node, so if they are not set again we know
    that they don't fire on every path. *)
let node_reset_forall node =
  let iter (err_instance, instr_ref_opt) err_state =
    match (instr_ref_opt, get_forall err_instance) with
    | Some instr_ref, is_forall ->
        let node' = InstrRef.get_node instr_ref in
        if is_forall && Procdesc.Node.equal node node' then err_state.always <- false
    | None, _ ->
        ()
  in
  H.iter iter err_tbl


(** Add an error to the error table and return whether it should be printed now. *)
let add_err find_canonical_duplicate err_instance instr_ref_opt =
  let is_forall = get_forall err_instance in
  if H.mem err_tbl (err_instance, instr_ref_opt) then false (* don't print now *)
  else
    let instr_ref_opt_deduplicate =
      match (is_forall, instr_ref_opt) with
      | true, Some instr_ref ->
          (* use canonical duplicate for forall checks *)
          let node = InstrRef.get_node instr_ref in
          let canonical_node = find_canonical_duplicate node in
          let instr_ref' = InstrRef.replace_node instr_ref canonical_node in
          Some instr_ref'
      | _ ->
          instr_ref_opt
    in
    Logging.debug Analysis Medium "Registering an issue: %a@\n" pp_err_instance err_instance ;
    H.add err_tbl (err_instance, instr_ref_opt_deduplicate) {always= true} ;
    not is_forall


(* If an error is related to a particular field, we support suppressing the
   error via a supress annotation placed near the field declaration *)
let get_field_name_for_error_suppressing = function
  | Over_annotation {violation_type= OverAnnotatedRule.FieldOverAnnoted field_name} ->
      Some field_name
  | Field_not_initialized {field_name} ->
      Some field_name
  | Condition_redundant _
  | Over_annotation {violation_type= OverAnnotatedRule.ReturnOverAnnotated _}
  (* In case of assignment to a field, it should be fixed case by case for each assignment *)
  | Bad_assignment _
  | Nullable_dereference _
  | Inconsistent_subclass _ ->
      None


(* The condition is redundant because a non-nullable object was (implicitly or explicitly) compared with null.
   Describes what exactly made nullsafe believe this is indeed a non-nullable.
 *)
let get_nonnull_explanation_for_condition_redudant (nonnull_origin : TypeOrigin.t) =
  match nonnull_origin with
  | MethodCall {pname} ->
      Format.asprintf ": %a is not annotated as `@Nullable`" MF.pp_monospaced
        (Procname.Java.to_simplified_string ~withclass:true pname)
  | NullConst _ ->
      Logging.die Logging.InternalError
        "Unexpected origin NullConst: this is for nullable types, should not lead to condition \
         redundant"
  | ArrayLengthResult ->
      Logging.die Logging.InternalError
        "Unexpected origin ArrayLengthAccess: the result is integer, should not be compared with \
         null"
  (* TODO: this could be specified more precisely *)
  | NonnullConst _
  | Field _
  | CallToGetKnownToContainsKey
  | CurrMethodParameter _
  | This
  | New
  | ArrayAccess
  | InferredNonnull _
  | OptimisticFallback ->
      " according to the existing annotations"


(** If error is reportable to the user, return its information. Otherwise return [None]. *)
let make_nullsafe_issue_if_reportable_lazy ~nullsafe_mode err_instance =
  let open IOption.Let_syntax in
  if is_synthetic_err err_instance then None
  else
    match err_instance with
    | Condition_redundant {is_always_true; loc; condition_descr; nonnull_origin} ->
        Some
          ( lazy
            (NullsafeIssue.make
               ~description:
                 (P.sprintf "The condition %s might be always %b%s."
                    (Option.value condition_descr ~default:"")
                    is_always_true
                    (get_nonnull_explanation_for_condition_redudant nonnull_origin))
               ~issue_type:IssueType.eradicate_condition_redundant
               ~loc
                 (* Condition redundant is a very non-precise issue. Depending on the origin of what is compared with null,
                      this can have a lot of reasons to be actually nullable.
                      Until it is made non-precise, it is recommended to not turn this warning on.
                      But even when it is on, this should not be more than advice.
                 *) ~severity:IssueType.Advice ~field_name:None) )
    | Over_annotation {over_annotated_violation; loc; violation_type} ->
        Some
          ( lazy
            (let issue_type, field_name =
               match violation_type with
               | OverAnnotatedRule.FieldOverAnnoted field_name ->
                   (IssueType.eradicate_field_over_annotated, Some field_name)
               | OverAnnotatedRule.ReturnOverAnnotated _ ->
                   (IssueType.eradicate_return_over_annotated, None)
             in
             let description =
               OverAnnotatedRule.violation_description over_annotated_violation violation_type
             in
             NullsafeIssue.make ~description ~issue_type ~loc ~field_name
               ~severity:
                 (* Very non-precise issue. Should be actually turned off unless for experimental purposes. *)
                 IssueType.Advice ) )
    | Field_not_initialized {field_name; loc} ->
        Some
          ( lazy
            (NullsafeIssue.make
               ~description:
                 (Format.asprintf
                    "Field %a is declared non-nullable, so it should be initialized in the \
                     constructor or in an `@Initializer` method"
                    MF.pp_monospaced
                    (Fieldname.get_field_name field_name))
               ~issue_type:IssueType.eradicate_field_not_initialized ~loc
               ~severity:(NullsafeMode.severity nullsafe_mode)
               ~field_name:(Some field_name)) )
    | Bad_assignment {assignment_location; assignment_type; assignment_violation} ->
        (* If violation is reportable, create tuple, otherwise None *)
        let+ reportable_violation =
          AssignmentRule.ReportableViolation.from nullsafe_mode assignment_violation
        in
        lazy
          (AssignmentRule.ReportableViolation.make_nullsafe_issue ~assignment_location
             assignment_type reportable_violation)
    | Nullable_dereference
        {dereference_violation; dereference_location; nullable_object_descr; dereference_type} ->
        (* If violation is reportable, create tuple, otherwise None *)
        let+ reportable_violation =
          DereferenceRule.ReportableViolation.from nullsafe_mode dereference_violation
        in
        lazy
          (DereferenceRule.ReportableViolation.make_nullsafe_issue reportable_violation
             ~dereference_location dereference_type ~nullable_object_descr)
    | Inconsistent_subclass
        {inheritance_violation; violation_type; loc; base_proc_name; overridden_proc_name} ->
        (* If violation is reportable, create tuple, otherwise None *)
        let+ reportable_violation =
          InheritanceRule.ReportableViolation.from nullsafe_mode inheritance_violation
        in
        lazy
          (InheritanceRule.ReportableViolation.make_nullsafe_issue ~nullsafe_mode
             reportable_violation violation_type ~loc ~base_proc_name ~overridden_proc_name)


(** If error is reportable to the user, return description, severity etc. Otherwise return None. *)
let make_nullsafe_issue_if_reportable ~nullsafe_mode err_instance =
  make_nullsafe_issue_if_reportable_lazy ~nullsafe_mode err_instance |> Option.map ~f:Lazy.force


let is_reportable ~nullsafe_mode err_instance =
  (* Note: we don't fetch the whole info because the the class-level analysis breaks some
     assumptions of this function, and also for optimization purposes (saving some string
     manipulations). *)
  make_nullsafe_issue_if_reportable_lazy ~nullsafe_mode err_instance |> Option.is_some


let report_now_if_reportable analysis_data err_instance ~nullsafe_mode =
  make_nullsafe_issue_if_reportable ~nullsafe_mode err_instance
  |> Option.iter ~f:(fun nullsafe_issue ->
         Logging.debug Analysis Medium "About to report: %s"
           (NullsafeIssue.get_description nullsafe_issue) ;
         let field_name = get_field_name_for_error_suppressing err_instance in
         EradicateReporting.report_error analysis_data Eradicate nullsafe_issue ~field_name )


(** Register issue (unless exactly the same issue was already registered). If needed, report this
    error immediately. *)
let register_error analysis_data find_canonical_duplicate err_instance ~nullsafe_mode instr_ref_opt
    =
  let should_report_now = add_err find_canonical_duplicate err_instance instr_ref_opt in
  if should_report_now then report_now_if_reportable analysis_data err_instance ~nullsafe_mode


let report_forall_issues_and_reset analysis_data ~nullsafe_mode =
  let iter (err_instance, instr_ref_opt) err_state =
    match (instr_ref_opt, get_forall err_instance) with
    | Some instr_ref, is_forall ->
        let node = InstrRef.get_node instr_ref in
        AnalysisState.set_node node ;
        if is_forall && err_state.always then
          report_now_if_reportable analysis_data err_instance ~nullsafe_mode
    | None, _ ->
        ()
  in
  H.iter iter err_tbl ;
  reset ()


let get_errors () =
  H.fold
    (fun (err_instance, instr_ref_opt) _err_state acc -> (err_instance, instr_ref_opt) :: acc)
    err_tbl []
