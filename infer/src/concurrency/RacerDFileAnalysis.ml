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


type reported_access =
  { threads: RacerDDomain.ThreadsDomain.t
  ; snapshot: RacerDDomain.AccessSnapshot.t
  ; tenv: Tenv.t
  ; procname: Procname.t }

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

module PathModuloThis : Caml.Map.OrderedType with type t = AccessPath.t = struct
  type t = AccessPath.t

  type var_ = Var.t

  let compare_var_ = Var.compare_modulo_this

  let compare = [%compare: (var_ * Typ.t) * AccessPath.access list]
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

  val filter_container_accesses : (PathModuloThis.t -> bool) -> t -> t
end = struct
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


  let filter_container_accesses f map =
    M.filter (fun k _ -> match k with Container p -> f p | _ -> true) map


  let fold f map a =
    let f _ v acc = f v acc in
    M.fold f map a
end

let should_report_guardedby_violation classname ({snapshot; tenv; procname} : reported_access) =
  let is_uitthread param =
    match String.lowercase param with
    | "ui thread" | "ui-thread" | "ui_thread" | "uithread" ->
        true
    | _ ->
        false
  in
  let field_is_annotated_guardedby field_name {Struct.name= f; annot= a} =
    Fieldname.equal f field_name
    && List.exists a ~f:(fun (annot : Annot.t) ->
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
    |> fun (base, accesses) -> (base, List.filter accesses ~f:MemoryAccess.is_field_or_array_access)
  with
  | AccessExpression.Base (_, base_type), [MemoryAccess.FieldAccess field_name] -> (
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


type report_kind =
  | GuardedByViolation
  | WriteWriteRace of RacerDDomain.AccessSnapshot.t option
      (** one of conflicting access, if there are any *)
  | ReadWriteRace of RacerDDomain.AccessSnapshot.t  (** one of several conflicting accesses *)
  | UnannotatedInterface

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


(** Explain why we are reporting this access, in Java *)
let get_reporting_explanation_java report_kind tenv pname thread =
  (* best explanation is always that the current class or method is annotated thread-safe. try for
     that first. *)
  let annotation_explanation_opt =
    if RacerDModels.is_thread_safe_method pname tenv then
      Some
        (F.asprintf
           "@\n Reporting because current method is annotated %a or overrides an annotated method."
           MF.pp_monospaced "@ThreadSafe" )
    else
      match RacerDModels.get_litho_explanation tenv pname with
      | Some _ as expl_opt ->
          expl_opt
      | None -> (
        match RacerDModels.get_current_class_and_threadsafe_superclasses tenv pname with
        | Some (current_class, (thread_safe_class :: _ as thread_safe_annotated_classes)) ->
            Some
              ( if List.mem ~equal:Typ.Name.equal thread_safe_annotated_classes current_class then
                  F.asprintf "@\n Reporting because the current class is annotated %a"
                    MF.pp_monospaced "@ThreadSafe"
                else
                  F.asprintf "@\n Reporting because a superclass %a is annotated %a"
                    (MF.wrap_monospaced Typ.Name.pp) thread_safe_class MF.pp_monospaced
                    "@ThreadSafe" )
        | _ ->
            None )
  in
  let issue_type, explanation =
    match (report_kind, annotation_explanation_opt) with
    | GuardedByViolation, _ ->
        ( IssueType.guardedby_violation
        , F.asprintf "@\n Reporting because field is annotated %a" MF.pp_monospaced "@GuardedBy" )
    | UnannotatedInterface, Some threadsafe_explanation ->
        (IssueType.interface_not_thread_safe, F.asprintf "%s." threadsafe_explanation)
    | UnannotatedInterface, None ->
        Logging.die InternalError
          "Reporting non-threadsafe interface call, but can't find a @ThreadSafe annotation"
    | _, Some threadsafe_explanation when RacerDDomain.ThreadsDomain.is_any thread ->
        ( IssueType.thread_safety_violation
        , F.asprintf
            "%s, so we assume that this method can run in parallel with other non-private methods \
             in the class (including itself)."
            threadsafe_explanation )
    | _, Some threadsafe_explanation ->
        ( IssueType.thread_safety_violation
        , F.asprintf
            "%s. Although this access is not known to run on a background thread, it may happen in \
             parallel with another access that does."
            threadsafe_explanation )
    | _, None ->
        (* failed to explain based on @ThreadSafe annotation; have to justify using background thread *)
        if RacerDDomain.ThreadsDomain.is_any thread then
          ( IssueType.thread_safety_violation
          , F.asprintf "@\n Reporting because this access may occur on a background thread." )
        else
          ( IssueType.thread_safety_violation
          , F.asprintf
              "@\n\
              \ Reporting because another access to the same memory occurs on a background thread, \
               although this access may not." )
  in
  (issue_type, explanation)


(** Explain why we are reporting this access, in C++ *)
let get_reporting_explanation_cpp = (IssueType.lock_consistency_violation, "")

(** Explain why we are reporting this access *)
let get_reporting_explanation report_kind tenv pname thread =
  if Procname.is_java pname || Procname.is_csharp pname then
    get_reporting_explanation_java report_kind tenv pname thread
  else get_reporting_explanation_cpp


let log_issue current_pname ~issue_log ~loc ~ltr ~access issue_type error_message =
  Reporting.log_issue_external current_pname ~issue_log ~loc ~ltr ~access issue_type error_message


let report_thread_safety_violation ~make_description ~report_kind
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
  let issue_type, explanation = get_reporting_explanation report_kind tenv pname threads in
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
      report_thread_safety_violation ~make_description ~report_kind:UnannotatedInterface
        reported_access issue_log
  | _ ->
      (* skip reporting on C++ *)
      issue_log


let report_thread_safety_violation ~acc ~make_description ~report_kind reported_access =
  ReportedSet.deduplicate
    ~f:(report_thread_safety_violation ~make_description ~report_kind)
    reported_access acc


let report_unannotated_interface_violation ~acc reported_pname reported_access =
  ReportedSet.deduplicate reported_access acc
    ~f:(report_unannotated_interface_violation reported_pname)


let make_read_write_race_description ~read_is_sync (conflict : reported_access) pname
    final_sink_site initial_sink_site final_sink =
  let pp_conflict fmt {procname} =
    F.pp_print_string fmt (Procname.to_simplified_string ~withclass:true procname)
  in
  Format.asprintf
    "Read/Write race. Non-private method %a%s reads%s from %a, which races with the%s write in \
     method %a."
    describe_pname pname
    (if CallSite.equal final_sink_site initial_sink_site then "" else " indirectly")
    (if read_is_sync then " with synchronization" else " without synchronization")
    pp_access final_sink
    (if read_is_sync then " unsynchronized" else "")
    (MF.wrap_monospaced pp_conflict) conflict


let make_guardedby_violation_description pname final_sink_site initial_sink_site final_sink =
  Format.asprintf
    "GuardedBy violation. Non-private method %a%s accesses %a outside of synchronization."
    describe_pname pname
    (if CallSite.equal final_sink_site initial_sink_site then "" else " indirectly")
    pp_access final_sink


let make_unprotected_write_description pname final_sink_site initial_sink_site final_sink =
  Format.asprintf "Unprotected write. Non-private method %a%s %s %a outside of synchronization."
    describe_pname pname
    (if CallSite.equal final_sink_site initial_sink_site then "" else " indirectly")
    ( if RacerDDomain.AccessSnapshot.is_container_write final_sink then "mutates"
      else "writes to field" )
    pp_access final_sink


let report_on_write_java_csharp accesses acc (reported_access : reported_access) =
  let open RacerDDomain in
  let conflict =
    if ThreadsDomain.is_any reported_access.threads then
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
    AccessSnapshot.is_unprotected reported_access.snapshot
    && (Option.is_some conflict || ThreadsDomain.is_any reported_access.threads)
  then
    report_thread_safety_violation ~acc ~make_description:make_unprotected_write_description
      ~report_kind:(WriteWriteRace conflict) reported_access
  else acc


(** unprotected read. report all writes as conflicts for java/csharp. *)
let report_on_unprotected_read_java_csharp accesses acc (reported_access : reported_access) =
  let open RacerDDomain in
  let is_conflict {snapshot; threads= other_threads} =
    AccessSnapshot.is_write snapshot
    && (ThreadsDomain.is_any reported_access.threads || ThreadsDomain.is_any other_threads)
  in
  List.find ~f:is_conflict accesses
  |> Option.value_map ~default:acc ~f:(fun conflict ->
         let make_description = make_read_write_race_description ~read_is_sync:false conflict in
         let report_kind = ReadWriteRace conflict.snapshot in
         report_thread_safety_violation ~acc ~make_description ~report_kind reported_access )


(** protected read. report unprotected writes and opposite protected writes as conflicts *)
let report_on_protected_read_java_csharp accesses acc (reported_access : reported_access) =
  let open RacerDDomain in
  let can_conflict (snapshot1 : AccessSnapshot.t) (snapshot2 : AccessSnapshot.t) =
    if snapshot1.elem.lock && snapshot2.elem.lock then false
    else ThreadsDomain.can_conflict snapshot1.elem.thread snapshot2.elem.thread
  in
  let is_conflict {snapshot= other_snapshot; threads= other_threads} =
    if AccessSnapshot.is_unprotected other_snapshot then
      AccessSnapshot.is_write other_snapshot && ThreadsDomain.is_any other_threads
    else
      AccessSnapshot.is_write other_snapshot && can_conflict reported_access.snapshot other_snapshot
  in
  List.find accesses ~f:is_conflict
  |> Option.value_map ~default:acc ~f:(fun conflict ->
         (* protected read with conflicting unprotected write(s). warn. *)
         let make_description = make_read_write_race_description ~read_is_sync:true conflict in
         let report_kind = ReadWriteRace conflict.snapshot in
         report_thread_safety_violation ~acc ~make_description ~report_kind reported_access )


(** main reporting hook for Java & C# *)
let report_unsafe_access_java_csharp accesses acc
    ({snapshot; threads; tenv; procname= pname} as reported_access) =
  let open RacerDDomain in
  let open RacerDModels in
  match snapshot.elem.access with
  | InterfaceCall {pname= reported_pname}
    when AccessSnapshot.is_unprotected snapshot
         && ThreadsDomain.is_any threads && is_marked_thread_safe pname tenv ->
      (* un-annotated interface call + no lock in method marked thread-safe. warn *)
      report_unannotated_interface_violation ~acc reported_pname reported_access
  | InterfaceCall _ ->
      acc
  | Write _ | ContainerWrite _ ->
      report_on_write_java_csharp accesses acc reported_access
  | (Read _ | ContainerRead _) when AccessSnapshot.is_unprotected snapshot ->
      report_on_unprotected_read_java_csharp accesses acc reported_access
  | Read _ | ContainerRead _ ->
      report_on_protected_read_java_csharp accesses acc reported_access


(** main reporting hook for C langs *)
let report_unsafe_access_objc_cpp accesses acc ({snapshot} as reported_access) =
  let open RacerDDomain in
  match snapshot.elem.access with
  | InterfaceCall _ | Write _ | ContainerWrite _ ->
      (* Do not report unprotected writes for ObjC_Cpp *)
      acc
  | (Read _ | ContainerRead _) when AccessSnapshot.is_unprotected snapshot ->
      (* unprotected read. for c++ filter out unprotected writes *)
      let is_conflict {snapshot} =
        AccessSnapshot.is_write snapshot && not (AccessSnapshot.is_unprotected snapshot)
      in
      List.find ~f:is_conflict accesses
      |> Option.value_map ~default:acc ~f:(fun conflict ->
             let make_description = make_read_write_race_description ~read_is_sync:false conflict in
             let report_kind = ReadWriteRace conflict.snapshot in
             report_thread_safety_violation ~acc ~make_description ~report_kind reported_access )
  | Read _ | ContainerRead _ ->
      (* Do not report protected reads for ObjC_Cpp *)
      acc


(** report hook dispatching to language specific functions *)
let report_unsafe_access accesses acc ({procname} as reported_access) =
  match (procname : Procname.t) with
  | Java _ | CSharp _ ->
      report_unsafe_access_java_csharp accesses acc reported_access
  | ObjC_Cpp _ ->
      report_unsafe_access_objc_cpp accesses acc reported_access
  | _ ->
      acc


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
let report_unsafe_accesses ~issue_log classname aggregated_access_map =
  let open RacerDDomain in
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
            report_thread_safety_violation ~acc ~report_kind:GuardedByViolation
              ~make_description:make_guardedby_violation_description r
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


let should_report_on_proc file_exe_env proc_name =
  Attributes.load proc_name
  |> Option.exists ~f:(fun attrs ->
         let tenv = Exe_env.get_proc_tenv file_exe_env proc_name in
         let is_not_private = not ProcAttributes.(equal_access (get_access attrs) Private) in
         match (proc_name : Procname.t) with
         | CSharp _ ->
             is_not_private
         | Java java_pname ->
             (* return true if procedure is at an abstraction boundary or reporting has been explicitly
                requested via @ThreadSafe in java *)
             RacerDModels.is_thread_safe_method proc_name tenv
             || is_not_private
                && (not (Procname.Java.is_class_initializer java_pname))
                && (not (Procname.Java.is_autogen_method java_pname))
                && not Annotations.(attrs_return_annot_ends_with attrs visibleForTesting)
         | ObjC_Cpp _ when Procname.is_cpp_lambda proc_name ->
             (* do not report on lambdas; they are essentially private though do not appear as such *)
             false
         | ObjC_Cpp {kind= CPPMethod _ | CPPConstructor _ | CPPDestructor _} ->
             is_not_private
         | ObjC_Cpp {kind= ObjCClassMethod | ObjCInstanceMethod; class_name} ->
             Tenv.lookup tenv class_name
             |> Option.exists ~f:(fun {Struct.exported_objc_methods} ->
                    List.mem ~equal:Procname.equal exported_objc_methods proc_name )
         | _ ->
             false )


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
  List.fold summaries ~init:ReportMap.empty ~f:(fun acc (procname, summary) ->
      if should_report_on_proc exe_env procname then
        let tenv = Exe_env.get_proc_tenv exe_env procname in
        aggregate_post tenv procname acc summary
      else acc )


let class_has_concurrent_method class_summaries =
  let open RacerDDomain in
  let method_has_concurrent_context (_, summary) =
    match (summary.threads : ThreadsDomain.t) with NoThread -> false | _ -> true
  in
  List.exists class_summaries ~f:method_has_concurrent_context


let should_report_on_class (classname : Typ.Name.t) class_summaries =
  (not (RacerDModels.class_is_ignored_by_racerd classname))
  &&
  match classname with
  | JavaClass _ ->
      (* don't do top-level reports on classes generated when compiling Kotlin coroutines *)
      not (RacerDModels.is_kotlin_coroutine_generated classname)
  | CSharpClass _ ->
      true
  | CppClass _ | ObjcClass _ | ObjcProtocol _ | CStruct _ ->
      class_has_concurrent_method class_summaries
  | CUnion _ | ErlangType _ | HackClass _ | PythonClass _ | ObjcBlock _ | CFunction _ ->
      false


(** aggregate all of the procedures in the file env by their declaring class. this lets us analyze
    each class individually *)
let aggregate_by_class {InterproceduralAnalysis.procedures; analyze_file_dependency} =
  List.fold procedures ~init:Typ.Name.Map.empty ~f:(fun acc procname ->
      Procname.get_class_type_name procname
      |> Option.bind ~f:(fun classname ->
             analyze_file_dependency procname |> AnalysisResult.to_option
             |> Option.map ~f:(fun summary ->
                    Typ.Name.Map.update classname
                      (fun summaries_opt ->
                        Some ((procname, summary) :: Option.value ~default:[] summaries_opt) )
                      acc ) )
      |> Option.value ~default:acc )
  |> Typ.Name.Map.filter should_report_on_class


let get_synchronized_container_fields_of analyze tenv classname =
  let open RacerDDomain in
  let last_field_of_access_expression acc_exp =
    let _, path = HilExp.AccessExpression.to_access_path acc_exp in
    List.last path
    |> Option.bind ~f:(function AccessPath.FieldAccess fieldname -> Some fieldname | _ -> None)
  in
  let add_synchronized_container_field acc_exp attr acc =
    match attr with
    | Attribute.Synchronized ->
        last_field_of_access_expression acc_exp
        |> Option.fold ~init:acc ~f:(fun acc fieldname -> Fieldname.Set.add fieldname acc)
    | _ ->
        acc
  in
  Tenv.lookup tenv classname
  |> Option.value_map ~default:[] ~f:(fun (tstruct : Struct.t) -> tstruct.methods)
  |> List.rev_filter ~f:(function
       | Procname.Java j ->
           Procname.Java.(is_class_initializer j || is_constructor j)
       | _ ->
           false )
  |> List.rev_filter_map ~f:(fun proc_name -> analyze proc_name |> AnalysisResult.to_option)
  |> List.rev_map ~f:(fun (summary : summary) -> summary.attributes)
  |> List.fold ~init:Fieldname.Set.empty ~f:(fun acc attributes ->
         AttributeMapDomain.fold add_synchronized_container_field attributes acc )


let get_synchronized_container_fields_of, clear_sync_container_cache =
  let cache = Typ.Name.Hash.create 5 in
  ( (fun analyze tenv classname ->
      match Typ.Name.Hash.find_opt cache classname with
      | Some fieldset ->
          fieldset
      | None ->
          let fieldset = get_synchronized_container_fields_of analyze tenv classname in
          Typ.Name.Hash.add cache classname fieldset ;
          fieldset )
  , fun () -> Typ.Name.Hash.clear cache )


let should_keep_container_access analyze tenv ((_base, path) : PathModuloThis.t) =
  let should_keep_access_ending_at_field fieldname =
    let classname = Fieldname.get_class_name fieldname in
    let sync_container_fields = get_synchronized_container_fields_of analyze tenv classname in
    not (Fieldname.Set.mem fieldname sync_container_fields)
  in
  let rec should_keep_container_access_inner rev_path =
    match rev_path with
    | [] ->
        true
    | AccessPath.ArrayAccess _ :: rest ->
        should_keep_container_access_inner rest
    | AccessPath.FieldAccess fieldname :: _ ->
        should_keep_access_ending_at_field fieldname
  in
  List.rev path |> should_keep_container_access_inner


(** Gathers results by analyzing all the methods in a file, then post-processes the results to check
    an (approximation of) thread safety *)
let analyze ({InterproceduralAnalysis.file_exe_env; analyze_file_dependency} as file_t) =
  let synchronized_container_filter = function
    | Typ.JavaClass _ ->
        let tenv = Exe_env.load_java_global_tenv file_exe_env in
        ReportMap.filter_container_accesses
          (should_keep_container_access analyze_file_dependency tenv)
    | _ ->
        Fn.id
  in
  let class_map = aggregate_by_class file_t in
  let result =
    Typ.Name.Map.fold
      (fun classname methods issue_log ->
        make_results_table file_exe_env methods
        |> synchronized_container_filter classname
        |> report_unsafe_accesses ~issue_log classname )
      class_map IssueLog.empty
  in
  clear_sync_container_cache () ;
  result
