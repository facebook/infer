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
    incr ir ; (node, !ir)
end

(* InstrRef *)

type origin_descr = string * Location.t option * AnnotatedSignature.t option

(* callee signature *)
(* ignore origin descr *)
let compare_origin_descr _ _ = 0

(** Instance of an error *)
type err_instance =
  | Condition_redundant of (bool * string option)
  | Inconsistent_subclass of
      { inheritance_violation: InheritanceRule.violation
      ; violation_type: InheritanceRule.violation_type
      ; base_proc_name: Typ.Procname.t
      ; overridden_proc_name: Typ.Procname.t }
  | Field_not_initialized of Typ.Fieldname.t * Typ.Procname.t
  | Over_annotation of
      { over_annotated_violation: OverAnnotatedRule.violation
      ; violation_type: OverAnnotatedRule.violation_type }
  | Nullable_dereference of
      { dereference_violation: DereferenceRule.violation
      ; dereference_type: DereferenceRule.dereference_type
      ; nullable_object_descr: string option
      ; origin_descr: origin_descr }
  | Bad_assignment of
      { assignment_violation: AssignmentRule.violation
      ; assignment_type: AssignmentRule.assignment_type
      ; rhs_origin_descr: origin_descr }
[@@deriving compare]

module H = Hashtbl.Make (struct
  type t = err_instance * InstrRef.t option [@@deriving compare]

  let equal = [%compare.equal: t]

  let hash = Hashtbl.hash
end
(* H *))

type err_state =
  { loc: Location.t  (** location of the error *)
  ; mutable always: bool  (** always fires on its associated node *) }

let err_tbl : err_state H.t = H.create 1

(** Reset the error table. *)
let reset () = H.reset err_tbl

(** Get the forall status of an err_instance.
    The forall status indicates that the error should be printed only if it
    occurs on every path. *)
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


(** Reset the always field of the forall erros in the node, so if they are not set again
    we know that they don't fire on every path. *)
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
let add_err find_canonical_duplicate err_instance instr_ref_opt loc =
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
    H.add err_tbl (err_instance, instr_ref_opt_deduplicate) {loc; always= true} ;
    not is_forall


(* print now if it's not a forall check *)

module Severity = struct
  let get_severity ia =
    if Annotations.ia_ends_with ia Annotations.generated_graphql then Some Exceptions.Error
    else None


  let this_type_get_severity tenv (signature : AnnotatedSignature.t) =
    match signature.params with
    | AnnotatedSignature.{mangled; param_annotated_type} :: _ when Mangled.is_this mangled ->
        (* TODO(T54088319) get rid of direct access to annotation *)
        Option.bind ~f:get_severity (PatternMatch.type_get_annotation tenv param_annotated_type.typ)
    | _ ->
        None


  let origin_descr_get_severity tenv origin_descr =
    match origin_descr with
    | _, _, Some signature ->
        this_type_get_severity tenv signature
    | _, _, None ->
        None


  let err_instance_get_severity tenv err_instance : Exceptions.severity option =
    match err_instance with
    | Nullable_dereference {origin_descr} ->
        origin_descr_get_severity tenv origin_descr
    | _ ->
        None
end

(* Severity *)

type st_report_error =
     Typ.Procname.t
  -> Procdesc.t
  -> IssueType.t
  -> Location.t
  -> ?field_name:Typ.Fieldname.t option
  -> ?exception_kind:(IssueType.t -> Localise.error_desc -> exn)
  -> ?severity:Exceptions.severity
  -> string
  -> unit

let get_infer_issue_type = function
  | Condition_redundant _ ->
      IssueType.eradicate_condition_redundant
  | Over_annotation {violation_type= OverAnnotatedRule.FieldOverAnnoted _} ->
      IssueType.eradicate_field_over_annotated
  | Over_annotation {violation_type= OverAnnotatedRule.ReturnOverAnnotated _} ->
      IssueType.eradicate_return_over_annotated
  | Field_not_initialized _ ->
      IssueType.eradicate_field_not_initialized
  | Bad_assignment {assignment_type= PassingParamToFunction _} ->
      IssueType.eradicate_parameter_not_nullable
  | Bad_assignment {assignment_type= AssigningToField _} ->
      IssueType.eradicate_field_not_nullable
  | Bad_assignment {assignment_type= ReturningFromFunction _} ->
      IssueType.eradicate_return_not_nullable
  | Nullable_dereference _ ->
      IssueType.eradicate_nullable_dereference
  | Inconsistent_subclass {violation_type= InconsistentReturn} ->
      IssueType.eradicate_inconsistent_subclass_return_annotation
  | Inconsistent_subclass {violation_type= InconsistentParam _} ->
      IssueType.eradicate_inconsistent_subclass_parameter_annotation


(* If an error is related to a particular field, we support suppressing the
   error via a supress annotation placed near the field declaration *)
let get_field_name_for_error_suppressing = function
  | Over_annotation {violation_type= OverAnnotatedRule.FieldOverAnnoted field_name} ->
      Some field_name
  | Field_not_initialized (field_name, _) ->
      Some field_name
  | Condition_redundant _
  | Over_annotation {violation_type= OverAnnotatedRule.ReturnOverAnnotated _}
  (* In case of assignment to a field, it should be fixed case by case for each assignment *)
  | Bad_assignment _
  | Nullable_dereference _
  | Inconsistent_subclass _ ->
      None


let get_error_description err_instance =
  let nullable_annotation = "@Nullable" in
  match err_instance with
  | Condition_redundant (is_always_true, s_opt) ->
      P.sprintf "The condition %s is always %b according to the existing annotations."
        (Option.value s_opt ~default:"") is_always_true
  | Over_annotation {over_annotated_violation; violation_type} ->
      OverAnnotatedRule.violation_description over_annotated_violation violation_type
  | Field_not_initialized (field_name, proc_name) ->
      let constructor_name =
        if Typ.Procname.is_constructor proc_name then "the constructor"
        else
          match proc_name with
          | Typ.Procname.Java pn_java ->
              MF.monospaced_to_string (Typ.Procname.Java.get_method pn_java)
          | _ ->
              MF.monospaced_to_string (Typ.Procname.to_simplified_string proc_name)
      in
      Format.asprintf "Field %a is not initialized in %s and is not declared %a" MF.pp_monospaced
        (Typ.Fieldname.to_simplified_string field_name)
        constructor_name MF.pp_monospaced nullable_annotation
  | Bad_assignment {rhs_origin_descr= rhs_origin_descr, _, _; assignment_type; assignment_violation}
    ->
      AssignmentRule.violation_description assignment_violation assignment_type ~rhs_origin_descr
  | Nullable_dereference
      { dereference_violation
      ; nullable_object_descr
      ; dereference_type
      ; origin_descr= origin_descr, _, _ } ->
      DereferenceRule.violation_description dereference_violation dereference_type
        ~nullable_object_descr ~origin_descr
  | Inconsistent_subclass
      {inheritance_violation; violation_type; base_proc_name; overridden_proc_name} ->
      InheritanceRule.violation_description inheritance_violation violation_type ~base_proc_name
        ~overridden_proc_name


(** Report an error right now. *)
let report_error_now tenv (st_report_error : st_report_error) err_instance loc pdesc : unit =
  let pname = Procdesc.get_proc_name pdesc in
  let infer_issue_type = get_infer_issue_type err_instance in
  let field_name = get_field_name_for_error_suppressing err_instance in
  let err_description = get_error_description err_instance in
  let severity = Severity.err_instance_get_severity tenv err_instance in
  st_report_error pname pdesc infer_issue_type loc ~field_name
    ~exception_kind:(fun k d -> Exceptions.Eradicate (k, d))
    ?severity err_description


(** Report an error unless is has been reported already, or unless it's a forall error
    since it requires waiting until the end of the analysis and be printed by flush. *)
let report_error tenv (st_report_error : st_report_error) find_canonical_duplicate err_instance
    instr_ref_opt loc pdesc =
  let should_report_now = add_err find_canonical_duplicate err_instance instr_ref_opt loc in
  if should_report_now then report_error_now tenv st_report_error err_instance loc pdesc


(** Report the forall checks at the end of the analysis and reset the error table *)
let report_forall_checks_and_reset tenv st_report_error proc_desc =
  let iter (err_instance, instr_ref_opt) err_state =
    match (instr_ref_opt, get_forall err_instance) with
    | Some instr_ref, is_forall ->
        let node = InstrRef.get_node instr_ref in
        State.set_node node ;
        if is_forall && err_state.always then
          report_error_now tenv st_report_error err_instance err_state.loc proc_desc
    | None, _ ->
        ()
  in
  H.iter iter err_tbl ; reset ()
