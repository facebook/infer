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
      { base_proc_name: Typ.Procname.t
      ; overridden_proc_name: Typ.Procname.t
      ; inconsistent_subclass_type: inconsistent_subclass_type }
  | Field_not_initialized of Typ.Fieldname.t * Typ.Procname.t
  | Over_annotation of over_annotation_type
  | Nullable_dereference of
      { nullable_object_descr: string option
      ; dereference_type: dereference_type
      ; origin_descr: origin_descr }
  | Bad_assignment of {rhs_origin_descr: origin_descr; assignment_type: assignment_type}
[@@deriving compare]

and inconsistent_subclass_type =
  | InconsistentParam of {param_description: string; param_position: int}
  | InconsistentReturn

and over_annotation_type =
  | FieldOverAnnotedAsNullable of Typ.Fieldname.t
  | ReturnOverAnnotatedAsNullable of Typ.Procname.t
      (** Return value of a method can be made non-nullable *)

and assignment_type =
  | PassingParamToAFunction of
      { param_description: string
      ; param_position: int
      ; function_procname: Typ.Procname.t }
  | AssigningToAField of Typ.Fieldname.t
  | ReturningFromAFunction of Typ.Procname.t

and dereference_type =
  | MethodCall of Typ.Procname.t
  | AccessToField of Typ.Fieldname.t
  | AccessByIndex of {index_desc: string}
  | ArrayLengthAccess

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
        Option.bind ~f:get_severity
          (PatternMatch.type_get_annotation tenv param_annotated_type.typ)
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
  | Over_annotation (FieldOverAnnotedAsNullable _) ->
      IssueType.eradicate_field_over_annotated
  | Over_annotation (ReturnOverAnnotatedAsNullable _) ->
      IssueType.eradicate_return_over_annotated
  | Field_not_initialized _ ->
      IssueType.eradicate_field_not_initialized
  | Bad_assignment {assignment_type= PassingParamToAFunction _} ->
      IssueType.eradicate_parameter_not_nullable
  | Bad_assignment {assignment_type= AssigningToAField _} ->
      IssueType.eradicate_field_not_nullable
  | Bad_assignment {assignment_type= ReturningFromAFunction _} ->
      IssueType.eradicate_return_not_nullable
  | Nullable_dereference _ ->
      IssueType.eradicate_nullable_dereference
  | Inconsistent_subclass {inconsistent_subclass_type= InconsistentReturn} ->
      IssueType.eradicate_inconsistent_subclass_return_annotation
  | Inconsistent_subclass {inconsistent_subclass_type= InconsistentParam _} ->
      IssueType.eradicate_inconsistent_subclass_parameter_annotation


(* If an error is related to a particular field, we support suppressing the
   error via a supress annotation placed near the field declaration *)
let get_field_name_for_error_suppressing = function
  | Over_annotation (FieldOverAnnotedAsNullable field_name) ->
      Some field_name
  | Field_not_initialized (field_name, _) ->
      Some field_name
  | Condition_redundant _
  | Over_annotation (ReturnOverAnnotatedAsNullable _)
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
  | Over_annotation (FieldOverAnnotedAsNullable field_name) ->
      Format.asprintf "Field %a is always initialized in the constructor but is declared %a"
        MF.pp_monospaced
        (Typ.Fieldname.to_simplified_string field_name)
        MF.pp_monospaced nullable_annotation
  | Over_annotation (ReturnOverAnnotatedAsNullable proc_name) ->
      Format.asprintf "Method %a is annotated with %a but never returns null." MF.pp_monospaced
        (Typ.Procname.to_simplified_string proc_name)
        MF.pp_monospaced nullable_annotation
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
  | Bad_assignment {rhs_origin_descr= origin_descr_string, _, _; assignment_type} -> (
    match assignment_type with
    | PassingParamToAFunction {param_description; param_position; function_procname} ->
        Format.asprintf "%a needs a non-null value in parameter %d but argument %a can be null. %s"
          MF.pp_monospaced
          (Typ.Procname.to_simplified_string ~withclass:true function_procname)
          param_position MF.pp_monospaced param_description origin_descr_string
    | AssigningToAField field_name ->
        Format.asprintf "Field %a can be null but is not declared %a. %s" MF.pp_monospaced
          (Typ.Fieldname.to_simplified_string field_name)
          MF.pp_monospaced nullable_annotation origin_descr_string
    | ReturningFromAFunction function_proc_name ->
        Format.asprintf "Method %a may return null but it is not annotated with %a. %s"
          MF.pp_monospaced
          (Typ.Procname.to_simplified_string function_proc_name)
          MF.pp_monospaced nullable_annotation origin_descr_string )
  | Nullable_dereference {nullable_object_descr; dereference_type; origin_descr= origin_str, _, _}
    ->
      let nullable_object_descr =
        match dereference_type with
        | MethodCall _ | AccessToField _ -> (
          match nullable_object_descr with
          | None ->
              "Object"
          (* Just describe an object itself *)
          | Some descr ->
              MF.monospaced_to_string descr )
        | ArrayLengthAccess | AccessByIndex _ -> (
          (* In Java, those operations can be applied only to arrays *)
          match nullable_object_descr with
          | None ->
              "Array"
          | Some descr ->
              Format.sprintf "Array %s" (MF.monospaced_to_string descr) )
      in
      let action_descr =
        match dereference_type with
        | MethodCall method_name ->
            Format.sprintf "calling %s"
              (MF.monospaced_to_string (Typ.Procname.to_simplified_string method_name))
        | AccessToField field_name ->
            Format.sprintf "accessing field %s"
              (MF.monospaced_to_string (Typ.Fieldname.to_simplified_string field_name))
        | AccessByIndex {index_desc} ->
            Format.sprintf "accessing at index %s" (MF.monospaced_to_string index_desc)
        | ArrayLengthAccess ->
            "accessing its length"
      in
      Format.sprintf "%s is nullable and is not locally checked for null when %s. %s"
        nullable_object_descr action_descr origin_str
  | Inconsistent_subclass {base_proc_name; overridden_proc_name; inconsistent_subclass_type} -> (
      let base_method_descr = Typ.Procname.to_simplified_string ~withclass:true base_proc_name in
      let overridden_method_descr =
        Typ.Procname.to_simplified_string ~withclass:true overridden_proc_name
      in
      match inconsistent_subclass_type with
      | InconsistentReturn ->
          Format.asprintf "Method %a is annotated with %a but overrides unannotated method %a."
            MF.pp_monospaced overridden_method_descr MF.pp_monospaced nullable_annotation
            MF.pp_monospaced base_method_descr
      | InconsistentParam {param_description; param_position} ->
          let translate_position = function
            | 1 ->
                "First"
            | 2 ->
                "Second"
            | 3 ->
                "Third"
            | n ->
                string_of_int n ^ "th"
          in
          Format.asprintf
            "%s parameter %a of method %a is not %a but is declared %ain the parent class method \
             %a."
            (translate_position param_position)
            MF.pp_monospaced param_description MF.pp_monospaced overridden_method_descr
            MF.pp_monospaced nullable_annotation MF.pp_monospaced nullable_annotation
            MF.pp_monospaced base_method_descr )


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
