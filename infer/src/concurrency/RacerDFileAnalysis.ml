(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module AccessExpression = HilExp.AccessExpression
module F = Format
module MF = MarkupFormatter

type report_kind =
  | GuardedByViolation
  | WriteWriteRace of RacerDDomain.AccessSnapshot.t option
      (** one of conflicting access, if there are any *)
  | ReadWriteRace of RacerDDomain.AccessSnapshot.t  (** one of several conflicting accesses *)
  | UnannotatedInterface

(** Explain why we are reporting this access, in Java *)
let get_reporting_explanation_java ~nullsafe report_kind tenv pname thread =
  let open RacerDModels in
  (* best explanation is always that the current class or method is annotated thread-safe. try for
     that first. *)
  let annotation_explanation_opt =
    if is_thread_safe_method pname tenv then
      Some
        (F.asprintf
           "@\n Reporting because current method is annotated %a or overrides an annotated method."
           MF.pp_monospaced "@ThreadSafe")
    else
      match FbThreadSafety.get_fbthreadsafe_class_annot pname tenv with
      | Some (qual, annot) ->
          Some (FbThreadSafety.message_fbthreadsafe_class qual annot)
      | None -> (
        match get_current_class_and_threadsafe_superclasses tenv pname with
        | Some (current_class, (thread_safe_class :: _ as thread_safe_annotated_classes)) ->
            Some
              ( if List.mem ~equal:Typ.Name.equal thread_safe_annotated_classes current_class then
                F.asprintf "@\n Reporting because the current class is annotated %a"
                  MF.pp_monospaced "@ThreadSafe"
              else
                F.asprintf "@\n Reporting because a superclass %a is annotated %a"
                  (MF.wrap_monospaced Typ.Name.pp) thread_safe_class MF.pp_monospaced "@ThreadSafe"
              )
        | _ ->
            None )
  in
  let issue_type, explanation, should_add_nullsafe_trailer =
    match (report_kind, annotation_explanation_opt) with
    | GuardedByViolation, _ ->
        ( IssueType.(if nullsafe then guardedby_violation_nullsafe else guardedby_violation)
        , F.asprintf "@\n Reporting because field is annotated %a" MF.pp_monospaced "@GuardedBy"
        , nullsafe )
    | UnannotatedInterface, Some threadsafe_explanation ->
        (IssueType.interface_not_thread_safe, F.asprintf "%s." threadsafe_explanation, false)
    | UnannotatedInterface, None ->
        Logging.die InternalError
          "Reporting non-threadsafe interface call, but can't find a @ThreadSafe annotation"
    | _, Some threadsafe_explanation when RacerDDomain.ThreadsDomain.is_any thread ->
        ( IssueType.(if nullsafe then thread_safety_violation_nullsafe else thread_safety_violation)
        , F.asprintf
            "%s, so we assume that this method can run in parallel with other non-private methods \
             in the class (including itself)."
            threadsafe_explanation
        , nullsafe )
    | _, Some threadsafe_explanation ->
        ( IssueType.(if nullsafe then thread_safety_violation_nullsafe else thread_safety_violation)
        , F.asprintf
            "%s. Although this access is not known to run on a background thread, it may happen in \
             parallel with another access that does."
            threadsafe_explanation
        , nullsafe )
    | _, None ->
        (* failed to explain based on @ThreadSafe annotation; have to justify using background thread *)
        if RacerDDomain.ThreadsDomain.is_any thread then
          ( IssueType.(
              if nullsafe then thread_safety_violation_nullsafe else thread_safety_violation)
          , F.asprintf "@\n Reporting because this access may occur on a background thread."
          , nullsafe )
        else
          ( IssueType.(
              if nullsafe then thread_safety_violation_nullsafe else thread_safety_violation)
          , F.asprintf
              "@\n\
              \ Reporting because another access to the same memory occurs on a background thread, \
               although this access may not."
          , nullsafe )
  in
  let explanation =
    if should_add_nullsafe_trailer then
      F.sprintf "%s@\n Data races in `@Nullsafe` classes may still cause NPEs." explanation
    else explanation
  in
  (issue_type, explanation)


(** Explain why we are reporting this access, in C++ *)
let get_reporting_explanation_cpp = (IssueType.lock_consistency_violation, "")

(** Explain why we are reporting this access *)
let get_reporting_explanation ~nullsafe report_kind tenv pname thread =
  if Procname.is_java pname then
    get_reporting_explanation_java ~nullsafe report_kind tenv pname thread
  else get_reporting_explanation_cpp


let describe_exp = MF.wrap_monospaced RacerDDomain.pp_exp

let describe_pname = MF.wrap_monospaced (Procname.pp_simplified_string ~withclass:true)

let pp_access fmt (t : RacerDDomain.AccessSnapshot.t) =
  match t.elem.access with
  | Read {exp} | Write {exp} ->
      describe_exp fmt exp
  | ContainerRead {exp; pname} | ContainerWrite {exp; pname} ->
      F.fprintf fmt "container %a via call to %a" describe_exp exp describe_pname pname
  | InterfaceCall _ as access ->
      RacerDDomain.Access.pp fmt access


let make_trace ~report_kind original_exp =
  let open RacerDDomain in
  let loc_trace_of_path path = AccessSnapshot.make_loc_trace path in
  let original_trace = loc_trace_of_path original_exp in
  let get_end_loc trace = Option.map (List.last trace) ~f:(function {Errlog.lt_loc} -> lt_loc) in
  let original_end = get_end_loc original_trace in
  let make_with_conflicts conflict_sink original_trace ~label1 ~label2 =
    (* create a trace for one of the conflicts and append it to the trace for the original sink *)
    let conflict_trace = loc_trace_of_path conflict_sink in
    let conflict_end = get_end_loc conflict_trace in
    ( Errlog.concat_traces [(label1, original_trace); (label2, conflict_trace)]
    , original_end
    , conflict_end )
  in
  match report_kind with
  | ReadWriteRace conflict ->
      make_with_conflicts conflict original_trace ~label1:"<Read trace>" ~label2:"<Write trace>"
  | WriteWriteRace (Some conflict) ->
      make_with_conflicts conflict original_trace ~label1:"<Write on unknown thread>"
        ~label2:"<Write on background thread>"
  | GuardedByViolation | WriteWriteRace None | UnannotatedInterface ->
      (original_trace, original_end, None)


let log_issue current_pname ~issue_log ~loc ~ltr ~access issue_type error_message =
  Reporting.log_issue_external current_pname ~issue_log ~loc ~ltr ~access issue_type error_message


type reported_access =
  { threads: RacerDDomain.ThreadsDomain.t
  ; snapshot: RacerDDomain.AccessSnapshot.t
  ; tenv: Tenv.t
  ; procname: Procname.t }

let report_thread_safety_violation ~make_description ~report_kind ~nullsafe
    ({threads; snapshot; tenv; procname= pname} : reported_access) issue_log =
  let open RacerDDomain in
  let final_pname = List.last snapshot.trace |> Option.value_map ~default:pname ~f:CallSite.pname in
  let final_sink_site = CallSite.make final_pname snapshot.loc in
  let initial_sink_site = CallSite.make pname (AccessSnapshot.get_loc snapshot) in
  let loc = CallSite.loc initial_sink_site in
  let ltr, original_end, conflict_end = make_trace ~report_kind snapshot in
  (* what the potential bug is *)
  let description = make_description pname final_sink_site initial_sink_site snapshot in
  (* why we are reporting it *)
  let issue_type, explanation =
    get_reporting_explanation ~nullsafe report_kind tenv pname threads
  in
  let error_message = F.sprintf "%s%s" description explanation in
  let end_locs = Option.to_list original_end @ Option.to_list conflict_end in
  let access = IssueAuxData.encode end_locs in
  log_issue pname ~issue_log ~loc ~ltr ~access RacerD issue_type error_message


let report_unannotated_interface_violation reported_pname reported_access issue_log =
  match reported_pname with
  | Procname.Java java_pname ->
      let class_name = Procname.Java.get_class_name java_pname in
      let make_description _ _ _ _ =
        F.asprintf
          "Unprotected call to method %a of un-annotated interface %a. Consider annotating the \
           interface with %a or adding a lock."
          describe_pname reported_pname MF.pp_monospaced class_name MF.pp_monospaced "@ThreadSafe"
      in
      report_thread_safety_violation ~nullsafe:false ~make_description
        ~report_kind:UnannotatedInterface reported_access issue_log
  | _ ->
      (* skip reporting on C++ *)
      issue_log


let make_unprotected_write_description pname final_sink_site initial_sink_site final_sink =
  Format.asprintf "Unprotected write. Non-private method %a%s %s %a outside of synchronization."
    describe_pname pname
    (if CallSite.equal final_sink_site initial_sink_site then "" else " indirectly")
    ( if RacerDDomain.AccessSnapshot.is_container_write final_sink then "mutates"
    else "writes to field" )
    pp_access final_sink


let make_guardedby_violation_description pname final_sink_site initial_sink_site final_sink =
  Format.asprintf
    "GuardedBy violation. Non-private method %a%s accesses %a outside of synchronization."
    describe_pname pname
    (if CallSite.equal final_sink_site initial_sink_site then "" else " indirectly")
    pp_access final_sink


let make_read_write_race_description ~read_is_sync (conflict : reported_access) pname
    final_sink_site initial_sink_site final_sink =
  let pp_conflict fmt {procname} =
    F.pp_print_string fmt (Procname.to_simplified_string ~withclass:true procname)
  in
  let conflicts_description =
    Format.asprintf "Potentially races with%s write in method %a"
      (if read_is_sync then " unsynchronized" else "")
      (MF.wrap_monospaced pp_conflict) conflict
  in
  Format.asprintf "Read/Write race. Non-private method %a%s reads%s from %a. %s." describe_pname
    pname
    (if CallSite.equal final_sink_site initial_sink_site then "" else " indirectly")
    (if read_is_sync then " with synchronization" else " without synchronization")
    pp_access final_sink conflicts_description


module ReportedSet : sig
  (** Type for deduplicating and storing reports. *)
  type t

  val reset : t -> t
  (** Reset recorded writes and reads, while maintaining the same [IssueLog.t]. *)

  val empty_of_issue_log : IssueLog.t -> t
  (** Create a set of reports containing the given [IssueLog.t] but otherwise having no records of
      previous reports. *)

  val to_issue_log : t -> IssueLog.t
  (** Recover deduplicated [IssueLog.t] from [t]. *)

  val deduplicate : f:(reported_access -> IssueLog.t -> IssueLog.t) -> reported_access -> t -> t
  (** Deduplicate [f]. *)
end = struct
  type reported_set =
    { sites: CallSite.Set.t
    ; writes: Procname.Set.t
    ; reads: Procname.Set.t
    ; unannotated_calls: Procname.Set.t }

  let empty_reported_set =
    { sites= CallSite.Set.empty
    ; reads= Procname.Set.empty
    ; writes= Procname.Set.empty
    ; unannotated_calls= Procname.Set.empty }


  type t = reported_set * IssueLog.t

  let empty_of_issue_log issue_log = (empty_reported_set, issue_log)

  let to_issue_log = snd

  let reset (reported_set, issue_log) =
    ({reported_set with writes= Procname.Set.empty; reads= Procname.Set.empty}, issue_log)


  let is_duplicate {snapshot; procname} (reported_set, _) =
    let call_site = CallSite.make procname (RacerDDomain.AccessSnapshot.get_loc snapshot) in
    CallSite.Set.mem call_site reported_set.sites
    ||
    match snapshot.elem.access with
    | Write _ | ContainerWrite _ ->
        Procname.Set.mem procname reported_set.writes
    | Read _ | ContainerRead _ ->
        Procname.Set.mem procname reported_set.reads
    | InterfaceCall _ ->
        Procname.Set.mem procname reported_set.unannotated_calls


  let update {snapshot; procname} (reported_set, issue_log) =
    let call_site = CallSite.make procname (RacerDDomain.AccessSnapshot.get_loc snapshot) in
    let sites = CallSite.Set.add call_site reported_set.sites in
    let reported_set = {reported_set with sites} in
    let reported_set =
      match snapshot.elem.access with
      | Write _ | ContainerWrite _ ->
          {reported_set with writes= Procname.Set.add procname reported_set.writes}
      | Read _ | ContainerRead _ ->
          {reported_set with reads= Procname.Set.add procname reported_set.reads}
      | InterfaceCall _ ->
          { reported_set with
            unannotated_calls= Procname.Set.add procname reported_set.unannotated_calls }
    in
    (reported_set, issue_log)


  let deduplicate ~f reported_access ((reported_set, issue_log) as acc) =
    if Config.deduplicate && is_duplicate reported_access acc then acc
    else update reported_access (reported_set, f reported_access issue_log)
end

(** Map containing reported accesses, which groups them in lists, by abstract location. The
    equivalence relation used for grouping them is equality of access paths. This is slightly
    complicated because local variables contain the pname of the function declaring them. Here we
    want a purely name-based comparison, and in particular that [this == this] regardless the method
    declaring it. Hence the redefined comparison functions. *)
module ReportMap : sig
  type t

  val empty : t

  val add : reported_access -> t -> t

  val fold : (reported_access list -> 'a -> 'a) -> t -> 'a -> 'a
end = struct
  module PathModuloThis : Caml.Map.OrderedType with type t = AccessPath.t = struct
    type t = AccessPath.t

    type var_ = Var.t

    let compare_var_ = Var.compare_modulo_this

    let compare = [%compare: (var_ * Typ.t) * AccessPath.access list]
  end

  module Key = struct
    type t = Location of PathModuloThis.t | Container of PathModuloThis.t | Call of Procname.t
    [@@deriving compare]

    let of_access (access : RacerDDomain.Access.t) =
      match access with
      | Read {exp} | Write {exp} ->
          Location (AccessExpression.to_access_path exp)
      | ContainerRead {exp} | ContainerWrite {exp} ->
          Container (AccessExpression.to_access_path exp)
      | InterfaceCall {pname} ->
          Call pname
  end

  module M = Caml.Map.Make (Key)

  type t = reported_access list M.t

  let empty = M.empty

  let add (rep : reported_access) map =
    let access = rep.snapshot.elem.access in
    let k = Key.of_access access in
    M.update k (function None -> Some [rep] | Some reps -> Some (rep :: reps)) map


  let fold f map a =
    let f _ v acc = f v acc in
    M.fold f map a
end

let should_report_on_proc tenv procdesc =
  let proc_name = Procdesc.get_proc_name procdesc in
  match proc_name with
  | Java java_pname ->
      (* return true if procedure is at an abstraction boundary or reporting has been explicitly
         requested via @ThreadSafe in java *)
      RacerDModels.is_thread_safe_method proc_name tenv
      || (not (PredSymb.equal_access (Procdesc.get_access procdesc) Private))
         && (not (Procname.Java.is_autogen_method java_pname))
         && not (Annotations.pdesc_return_annot_ends_with procdesc Annotations.visibleForTesting)
  | ObjC_Cpp objc_cpp when Procname.ObjC_Cpp.is_cpp_lambda objc_cpp ->
      (* do not report on lambdas; they are essentially private though do not appear as such *)
      false
  | ObjC_Cpp {kind= CPPMethod _ | CPPConstructor _ | CPPDestructor _} ->
      not (PredSymb.equal_access (Procdesc.get_access procdesc) Private)
  | ObjC_Cpp {kind= ObjCClassMethod | ObjCInstanceMethod | ObjCInternalMethod; class_name} ->
      Tenv.lookup tenv class_name
      |> Option.exists ~f:(fun {Struct.exported_objc_methods} ->
             List.mem ~equal:Procname.equal exported_objc_methods proc_name )
  | _ ->
      false


let should_report_guardedby_violation classname ({snapshot; tenv; procname} : reported_access) =
  let is_uitthread param =
    match String.lowercase param with
    | "ui thread" | "ui-thread" | "ui_thread" | "uithread" ->
        true
    | _ ->
        false
  in
  let field_is_annotated_guardedby field_name (f, _, a) =
    Fieldname.equal f field_name
    && List.exists a ~f:(fun ((annot : Annot.t), _) ->
           Annotations.annot_ends_with annot Annotations.guarded_by
           &&
           match annot.parameters with
           | [param] ->
               not (Annot.has_matching_str_value ~pred:is_uitthread param.value)
           | _ ->
               false )
  in
  (not snapshot.elem.lock)
  && RacerDDomain.AccessSnapshot.is_write snapshot
  && Procname.is_java procname
  &&
  (* restrict check to access paths of length one *)
  match
    RacerDDomain.Access.get_access_exp snapshot.elem.access
    |> AccessExpression.to_accesses
    |> fun (base, accesses) -> (base, List.filter accesses ~f:HilExp.Access.is_field_or_array_access)
  with
  | AccessExpression.Base (_, base_type), [HilExp.Access.FieldAccess field_name] -> (
    match base_type.desc with
    | Tstruct base_name | Tptr ({desc= Tstruct base_name}, _) ->
        (* is the base class a subclass of the one containing the GuardedBy annotation? *)
        PatternMatch.is_subtype tenv base_name classname
        && Tenv.lookup tenv base_name
           |> Option.exists ~f:(fun ({fields; statics} : Struct.t) ->
                  let f fld = field_is_annotated_guardedby field_name fld in
                  List.exists fields ~f || List.exists statics ~f )
    | _ ->
        false )
  | _ ->
      false


let should_report_race_in_nullsafe_class ({snapshot; tenv} : reported_access) =
  match snapshot.elem.access with
  | Read {exp= (FieldOffset (Dereference (Base _), _) | FieldOffset (Base _, _)) as exp}
  | Write {exp= (FieldOffset (Dereference (Base _), _) | FieldOffset (Base _, _)) as exp} ->
      AccessExpression.get_typ exp tenv
      |> Option.exists ~f:(fun typ -> Typ.is_java_type typ && not (Typ.is_java_primitive_type typ))
  | _ ->
      (* allow normal reporting for all other cases *)
      false


(** Report accesses that may race with each other.

    Principles for race reporting.

    Two accesses are excluded if they are both protected by the same lock or are known to be on the
    same thread. Otherwise they are in conflict. We want to report conflicting accesses one of which
    is a write.

    To cut down on duplication noise we don't always report at both sites (line numbers) involved in
    a race.

    \-- If a protected access races with an unprotected one, we don't report the protected but we do
    report the unprotected one (and we point to the protected from the unprotected one). This way
    the report is at the line number in a race-pair where the programmer should take action.

    \-- Similarly, if a threaded and unthreaded (not known to be threaded) access race, we report at
    the unthreaded site.

    Also, we avoid reporting multiple races at the same line (which can happen a lot in an
    interprocedural scenario) or multiple accesses to the same field in a single method, expecting
    that the programmer already gets signal from one report. To report all the races with separate
    warnings leads to a lot of noise. But note, we never suppress all the potential issues in a
    class: if we don't report any races, it means we didn't find any.

    The above is tempered at the moment by abstractions of "same lock" and "same thread": we are
    currently not distinguishing different locks, and are treating "known to be confined to a
    thread" as if "known to be confined to UI thread". *)
let report_unsafe_accesses ~issue_log file_tenv classname (aggregated_access_map : ReportMap.t) =
  let open RacerDDomain in
  let open RacerDModels in
  let class_is_annotated_nullsafe =
    Tenv.lookup file_tenv classname
    |> Option.exists ~f:(fun tstruct ->
           Annotations.(struct_typ_has_annot tstruct (fun annot -> ia_ends_with annot nullsafe)) )
  in
  let report_thread_safety_violation ~acc ~make_description ~report_kind ~nullsafe reported_access =
    ReportedSet.deduplicate
      ~f:(report_thread_safety_violation ~make_description ~report_kind ~nullsafe)
      reported_access acc
  in
  let report_unannotated_interface_violation ~acc reported_pname reported_access =
    ReportedSet.deduplicate reported_access acc
      ~f:(report_unannotated_interface_violation reported_pname)
  in
  let report_unsafe_access accesses acc
      ({snapshot; threads; tenv; procname= pname} as reported_access) =
    let nullsafe =
      class_is_annotated_nullsafe && should_report_race_in_nullsafe_class reported_access
    in
    match snapshot.elem.access with
    | InterfaceCall {pname= reported_pname}
      when AccessSnapshot.is_unprotected snapshot
           && ThreadsDomain.is_any threads && is_marked_thread_safe pname tenv ->
        (* un-annotated interface call + no lock in method marked thread-safe. warn *)
        report_unannotated_interface_violation ~acc reported_pname reported_access
    | InterfaceCall _ ->
        acc
    | (Write _ | ContainerWrite _) when Procname.is_java pname ->
        let conflict =
          if ThreadsDomain.is_any threads then
            (* unprotected write in method that may run in parallel with itself. warn *)
            None
          else
            (* unprotected write, but not on a method that may run in parallel with itself
               (i.e., not a self race). find accesses on a background thread this access might
               conflict with and report them *)
            List.find_map accesses ~f:(fun {snapshot= other_snapshot; threads= other_threads} ->
                if AccessSnapshot.is_write other_snapshot && ThreadsDomain.is_any other_threads then
                  Some other_snapshot
                else None )
        in
        if
          AccessSnapshot.is_unprotected snapshot
          && (Option.is_some conflict || ThreadsDomain.is_any threads)
        then
          report_thread_safety_violation ~acc ~make_description:make_unprotected_write_description
            ~report_kind:(WriteWriteRace conflict) ~nullsafe reported_access
        else acc
    | Write _ | ContainerWrite _ ->
        (* Do not report unprotected writes for ObjC_Cpp *)
        acc
    | (Read _ | ContainerRead _) when AccessSnapshot.is_unprotected snapshot ->
        (* unprotected read. report all writes as conflicts for java. for c++ filter out
           unprotected writes *)
        let is_conflict {snapshot; threads= other_threads} =
          AccessSnapshot.is_write snapshot
          &&
          if Procname.is_java pname then
            ThreadsDomain.is_any threads || ThreadsDomain.is_any other_threads
          else not (AccessSnapshot.is_unprotected snapshot)
        in
        List.find ~f:is_conflict accesses
        |> Option.value_map ~default:acc ~f:(fun conflict ->
               let make_description =
                 make_read_write_race_description ~read_is_sync:false conflict
               in
               let report_kind = ReadWriteRace conflict.snapshot in
               report_thread_safety_violation ~acc ~make_description ~report_kind ~nullsafe
                 reported_access )
    | (Read _ | ContainerRead _) when Procname.is_java pname ->
        (* protected read. report unprotected writes and opposite protected writes as conflicts *)
        let can_conflict (snapshot1 : AccessSnapshot.t) (snapshot2 : AccessSnapshot.t) =
          if snapshot1.elem.lock && snapshot2.elem.lock then false
          else ThreadsDomain.can_conflict snapshot1.elem.thread snapshot2.elem.thread
        in
        let is_conflict {snapshot= other_snapshot; threads= other_threads} =
          if AccessSnapshot.is_unprotected other_snapshot then
            AccessSnapshot.is_write other_snapshot && ThreadsDomain.is_any other_threads
          else AccessSnapshot.is_write other_snapshot && can_conflict snapshot other_snapshot
        in
        List.find accesses ~f:is_conflict
        |> Option.value_map ~default:acc ~f:(fun conflict ->
               (* protected read with conflicting unprotected write(s). warn. *)
               let make_description =
                 make_read_write_race_description ~read_is_sync:true conflict
               in
               let report_kind = ReadWriteRace conflict.snapshot in
               report_thread_safety_violation ~acc ~make_description ~report_kind ~nullsafe
                 reported_access )
    | Read _ | ContainerRead _ ->
        (* Do not report protected reads for ObjC_Cpp *)
        acc
  in
  let report_accesses_on_location reportable_accesses init =
    (* Don't report on location if all accesses are on non-concurrent contexts *)
    if
      List.for_all reportable_accesses ~f:(fun ({threads} : reported_access) ->
          ThreadsDomain.is_any threads |> not )
    then init
    else List.fold reportable_accesses ~init ~f:(report_unsafe_access reportable_accesses)
  in
  let report_guardedby_violations_on_location grouped_accesses init =
    if Config.racerd_guardedby then
      List.fold grouped_accesses ~init ~f:(fun acc r ->
          if should_report_guardedby_violation classname r then
            let nullsafe = class_is_annotated_nullsafe && should_report_race_in_nullsafe_class r in
            report_thread_safety_violation ~acc ~report_kind:GuardedByViolation
              ~make_description:make_guardedby_violation_description ~nullsafe r
          else acc )
    else init
  in
  let report grouped_accesses acc =
    (* reset the reported reads and writes for each memory location *)
    ReportedSet.reset acc
    |> report_guardedby_violations_on_location grouped_accesses
    |> report_accesses_on_location grouped_accesses
  in
  ReportedSet.empty_of_issue_log issue_log
  |> ReportMap.fold report aggregated_access_map
  |> ReportedSet.to_issue_log


(* create a map from [abstraction of a memory loc] -> accesses that
   may touch that memory loc. the abstraction of a location is an access
   path like x.f.g whose concretization is the set of memory cells
   that x.f.g may point to during execution *)
let make_results_table exe_env summaries =
  let open RacerDDomain in
  let aggregate_post tenv procname acc {threads; accesses} =
    AccessDomain.fold
      (fun snapshot acc -> ReportMap.add {threads; snapshot; tenv; procname} acc)
      accesses acc
  in
  List.fold summaries ~init:ReportMap.empty ~f:(fun acc (proc_desc, summary) ->
      let procname = Procdesc.get_proc_name proc_desc in
      let tenv = Exe_env.get_proc_tenv exe_env procname in
      aggregate_post tenv procname acc summary )


let class_has_concurrent_method class_summaries =
  let open RacerDDomain in
  let method_has_concurrent_context (_, summary) =
    match (summary.threads : ThreadsDomain.t) with NoThread -> false | _ -> true
  in
  List.exists class_summaries ~f:method_has_concurrent_context


let should_report_on_class (classname : Typ.Name.t) class_summaries =
  match classname with
  | JavaClass _ | CSharpClass _ ->
      true
  | CppClass _ | ObjcClass _ | ObjcProtocol _ | CStruct _ ->
      class_has_concurrent_method class_summaries
  | CUnion _ ->
      false


let filter_reportable_classes class_map = Typ.Name.Map.filter should_report_on_class class_map

(** aggregate all of the procedures in the file env by their declaring class. this lets us analyze
    each class individually *)
let aggregate_by_class {InterproceduralAnalysis.procedures; file_exe_env; analyze_file_dependency} =
  List.fold procedures ~init:Typ.Name.Map.empty ~f:(fun acc procname ->
      Procname.get_class_type_name procname
      |> Option.bind ~f:(fun classname ->
             analyze_file_dependency procname
             |> Option.filter ~f:(fun (pdesc, _) ->
                    let tenv = Exe_env.get_proc_tenv file_exe_env procname in
                    should_report_on_proc tenv pdesc )
             |> Option.map ~f:(fun summary_proc_desc ->
                    Typ.Name.Map.update classname
                      (function
                        | None ->
                            Some [summary_proc_desc]
                        | Some summaries ->
                            Some (summary_proc_desc :: summaries) )
                      acc ) )
      |> Option.value ~default:acc )
  |> filter_reportable_classes


(** Gathers results by analyzing all the methods in a file, then post-processes the results to check
    an (approximation of) thread safety *)
let analyze ({InterproceduralAnalysis.file_exe_env; source_file} as file_t) =
  let class_map = aggregate_by_class file_t in
  Typ.Name.Map.fold
    (fun classname methods issue_log ->
      let file_tenv = Exe_env.get_sourcefile_tenv file_exe_env source_file in
      make_results_table file_exe_env methods
      |> report_unsafe_accesses ~issue_log file_tenv classname )
    class_map IssueLog.empty
