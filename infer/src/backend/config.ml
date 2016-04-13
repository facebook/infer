(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module F = Format;;

(* Initialization *)

F.set_margin 100

let set_minor_heap_size nMb = (* increase the minor heap size to speed up gc *)
  let ctrl = Gc.get () in
  let oneMb = 1048576 in
  let new_size = max ctrl.Gc.minor_heap_size (nMb * oneMb)
  in Gc.set { ctrl with Gc.minor_heap_size = new_size };;

set_minor_heap_size 1

let from_env_variable var_name =
  try
    let _ = Sys.getenv var_name in true
  with Not_found -> false

let get_env_variable var_name =
  try
    let v = Sys.getenv var_name in
    if v = "" then None else Some v
  with Not_found -> None

let attributes_dir_name = "attributes"
let captured_dir_name = "captured"
let sources_dir_name = "sources"
let specs_dir_name = "specs"
let perf_stats_prefix = "perf_stats"
let backend_stats_dir_name = "backend_stats"
let frontend_stats_dir_name = "frontend_stats"
let reporting_stats_dir_name = "reporting_stats"

let default_in_zip_results_dir = "infer"

let default_buck_out = "buck-out"

let proc_stats_filename = "proc_stats.json"

let global_tenv_filename = "global.tenv"

(** Name of the infer configuration file *)
let inferconfig_file = ".inferconfig"

let inferconfig_home : string option ref = ref None

let suppress_warnings_annotations : string option ref = ref None

(** List of paths to the directories containing specs for library functions. *)
let specs_library = ref []

(** path to lib/specs to retrieve the default models *)
let models_dir =
  let bin_dir = Filename.dirname Sys.executable_name in
  let lib_dir = Filename.concat (Filename.concat bin_dir Filename.parent_dir_name) "lib" in
  let lib_specs_dir = Filename.concat lib_dir specs_dir_name in
  lib_specs_dir

let string_crc_hex32 s =
  Digest.to_hex (Digest.string s)

module JarCache =
struct
  let infer_cache : string option ref = ref None

  let mkdir s =
    try
      Unix.mkdir s 0o700;
      true
    with Unix.Unix_error _ -> false

  let extract_specs dest_dir jarfile =
    let zip_channel = Zip.open_in jarfile in
    let entries = Zip.entries zip_channel in
    let extract_entry entry =
      let dest_file = Filename.concat dest_dir (Filename.basename entry.Zip.filename) in
      if Filename.check_suffix entry.Zip.filename ".specs"
      then Zip.copy_entry_to_file zip_channel entry dest_file in
    List.iter extract_entry entries;
    Zip.close_in zip_channel

  let handle_jar jarfile =
    match !infer_cache with
    | Some cache_dir ->
        let basename = Filename.basename jarfile in
        let key = basename ^ string_crc_hex32 jarfile in
        let key_dir = Filename.concat cache_dir key in

        if (mkdir key_dir)
        then extract_specs key_dir jarfile;

        specs_library := !specs_library @ [key_dir]
    | None -> ()
end

type zip_library = {
  zip_filename: string;
  zip_channel: Zip.in_file;
  models: bool;
}

let zip_filename zip_library =
  zip_library.zip_filename

let zip_channel zip_library =
  zip_library.zip_channel

(** list of the zip files to search for specs files *)
let zip_libraries : zip_library list ref = ref []

let add_zip_library zip_filename =
  if !JarCache.infer_cache != None
  then
    JarCache.handle_jar zip_filename
  else
    (* The order matters, the jar files should be added following the order *)
    (* specs files should be searched in them *)
    zip_libraries := !zip_libraries @ [{ zip_filename = zip_filename; zip_channel = Zip.open_in zip_filename; models = false }]

let add_models zip_filename =
  zip_libraries := !zip_libraries @ [{ zip_filename = zip_filename; zip_channel = Zip.open_in zip_filename; models = true }]

let project_root : string option ref = ref None

(** FLAGS AND GLOBAL VARIABLES *)

(** Flag for abstracting fields of structs
    0 = no
    1 = forget some fields during matching (and so lseg abstraction) *)
let abs_struct = ref 1

(** Flag for abstracting numerical values
    0 = no abstraction.
    1 = evaluate all expressions abstractly.
    2 = 1 + abstract constant integer values during join.
*)
let abs_val_default = 2
let abs_val =
  ref abs_val_default

let reset_abs_val () =
  abs_val := abs_val_default

(** if true, completely ignore the possibility that errors can be caused by unknown procedures
 * during the symbolic execution phase *)
let angelic_execution = ref true

(** Flag for forgetting memory leak
    false = no
    true = forget leaked memory cells during abstraction
*)
let allowleak = ref false

(** Flag for ignoring arrays and pointer arithmetic.
    0 = treats both features soundly.
    1 = assumes that the size of every array is infinite.
    2 = assumes that all heap dereferences via array indexing and pointer arithmetic are correct.
*)
let array_level = ref 0

(** Check whether to report Analysis_stops message in user mode *)
let analysis_stops = ref false

type os_type = Unix | Win32 | Cygwin

let os_type = match Sys.os_type with
  | "Win32" -> Win32
  | "Cygwin" -> Cygwin
  | _ -> Unix

(** default path of the project results directory *)
let default_results_dir =
  Filename.concat (Sys.getcwd ()) "infer-out"

(** If true shows internal exceptions*)
let developer_mode = ref false

(** flag: dotty output filename **)
let dotty_output = ref "icfg.dot"

(** command line option to activate the eradicate checker. *)
let eradicate = ref false

(** should the checkers be run? *)
let checkers_enabled () = not !eradicate

(** flag for reactive mode:
    the analysis starts from the files captured since the "infer" command started *)
let reactive_mode = ref false

(** Continue the capture for reactive mode:
    If a procedure was changed beforehand, keep the changed marking. *)
let continue_capture = ref false

(** Merge the captured results directories specified in the dependency file *)
let merge = ref false

(** Flag for footprint discovery mode *)
let footprint = ref true

(** Set in the middle of forcing delayed prints *)
let forcing_delayed_prints = ref false

(** If true, treat calls to no-arg getters as idempotent w.r.t non-nullness *)
let idempotent_getters = ref true

(** if true, changes to code are checked at the procedure level; if false, at the file level *)
let incremental_procs = ref true

(** if active, join  x+j and x+k for constants j and k *)
let join_plus = ref true

(** Flag to tune the final information-loss check used by the join
    0 = use the most aggressive join for preconditions
    1 = use the least aggressive join for preconditions
*)
let join_cond = ref 1

(** Flag for turning on the transformation that
    null is assigned to a program variable when it becomes dead.
 **)
let liveness = ref true

(** if true, give static procs a long name filename::procname *)
let long_static_proc_names = ref false

(** Number of lines of code in original file *)
let nLOC = ref 0

(** Maximum level of recursion during the analysis, after which a timeout is generated *)
let max_recursion = ref 5

(** Flag to tune the level of applying the meet operator for
    preconditions during the footprint analysis.
    0 = do not use the meet.
    1 = use the meet to generate new preconditions.
*)
let meet_level = ref 1

(** Monitor the size of the props, and print every time the current max is exceeded *)
let monitor_prop_size = ref false

(** Flag for using the nonempty lseg only **)
let nelseg = ref false

(** Flag to activate nonstop mode: the analysis continues after in encounters errors *)
let nonstop = ref false

(** if true, skip the re-execution phase *)
let only_footprint = ref false

(** if true, user simple pretty printing *)
let pp_simple = ref true

(** flag: if true print full type info *)
let print_types = ref false

(** if true, acrtivate color printing by diff'ing w.r.t. previous prop *)
let print_using_diff = ref true

(** path of the project results directory *)
let results_dir = ref default_results_dir

(** Flag to tune the level of abstracting the postconditions of specs discovered
    by the footprint analysis.
    0 = nothing special.
    1 = filter out redundant posts implied by other posts. *)
let spec_abs_level = ref 1

(** Flag for test mode *)
let test = ref true

(** Flag set to enable detailed tracing informatin during error explanation *)
let trace_error = ref false

(** Flag set to enable detailed tracing information during re-arrangement *)
let trace_rearrange = ref false

(** Flag set to enable detailed tracing information during join *)
let trace_join = ref false

(** Flag set to enable detailed tracing information during array abstraction *)
let trace_absarray = ref false

(** Consider the size of types during analysis, e.g. cannot use an int pointer to write to a char *)
let type_size = ref false

(** if true, compact summaries before saving *)
let save_compact_summaries = ref true

(** If true, save the execution time in summaries.
    This makes the analysis nondeterministic. *)
let save_time_in_summaries = ref false

(** flag: if true enables printing proposition compatible for the SMT project *)
let smt_output = ref false

(** flag: if true performs taint analysis *)
let taint_analysis = ref true

(** Flag for turning on the optimization based on locality
    0 = no
    1 = based on reachability
*)
let undo_join = ref true

(** visit mode for the worklist:
    0 depth - fist visit
    1 bias towards exit node
    2 least visited first *)
let worklist_mode = ref 0

(** flag: if true write dot files in db dir*)
let write_dotty = ref false

(** flag: if true write html files in db dir *)
let write_html = ref false

let subtype_multirange = ref true

let optimistic_cast = ref false

(** if true, filter out errors in low likelyhood buckets, and only show then in developer mode *)
let filter_buckets = ref false

(** if true, show buckets in textual description of errors *)
let show_buckets = ref false

(** if true, show memory leak buckets in textual description of errors *)
let show_ml_buckets = ref false

(** if true, print cfg nodes in the dot file that are not defined in that file *)
let dotty_cfg_libs = ref true

(** if true, it deals with messages (method calls) in objective-c using the objective-c
    typical semantics. That is: if the receiver is nil then the method is nop and it returns 0.
    When the flag is false we deal with messages as standard method / function calls *)
let objc_method_call_semantics = ref true

(** if true, generate preconditions for runtime exceptions in Java and report errors for the public
    methods having preconditions to throw runtime exceptions *)
let report_runtime_exceptions = ref false

(** if true, sanity-check inferred preconditions against Nullable annotations and report
    inconsistencies *)
let report_nullable_inconsistency = ref true

(** true if the current objective-c source file is compiled with automatic reference counting (ARC) *)
let arc_mode = ref false

let objc_memory_model_on = ref false

let report_custom_error = from_env_variable "INFER_REPORT_CUSTOM_ERROR"
let default_failure_name = "ASSERTION_FAILURE"

let analyze_models = from_env_variable "INFER_ANALYZE_MODELS"

(** initial time of the analysis, i.e. when this module is loaded, gotten from Unix.time *)
let initial_analysis_time = Unix.time ()

let symops_timeout, seconds_timeout =
  let default_symops_timeout = 333 in
  let default_seconds_timeout = 10.0 in
  let long_symops_timeout = 1000 in
  let long_seconds_timeout = 30.0 in
  if analyze_models then
    (* use longer timeouts when analyzing models *)
    long_symops_timeout, long_seconds_timeout
  else
    default_symops_timeout, default_seconds_timeout

(** number of symops to multiply by the number of iterations, after which there is a timeout *)
let symops_per_iteration = ref symops_timeout

(** number of seconds to multiply by the number of iterations, after which there is a timeout *)
let seconds_per_iteration = ref seconds_timeout

(** Set the timeout values in seconds and symops, computed as a multiple of the integer parameter *)
let iterations = ref 1

(** experimental: dynamic dispatch for interface calls only in Java. off by default because of the
    cost *)
let sound_dynamic_dispatch = from_env_variable "INFER_SOUND_DYNAMIC_DISPATCH"

(** experimental: handle dynamic dispatch by following the JVM semantics and creating
    during the symbolic excution procedure descriptions using the types information
    found in the abstract state *)
let lazy_dynamic_dispatch = from_env_variable "INFER_LAZY_DYNAMIC_DISPATCH"

module Experiment = struct

  (** if true, a precondition with e.g. index 3 in an array does not require the caller to have index 3 too
      this mimics what happens with direct access to the array without a procedure call, where the index is simply materialized if not there *)
  let allow_missing_index_in_proc_call = ref true

  (** if true, a procedure call succeeds even when there is a bound error
      this mimics what happens with a direct array access where an error is produced and the analysis continues *)
  let bound_error_allowed_in_procedure_call = ref true

end

let source_file_extentions = [".java"; ".m"; ".mm"; ".c"; ".cc"; ".cpp"; ".h"]

let anonymous_block_prefix = "__objc_anonymous_block_"

let anonymous_block_num_sep = "______"

let property_attributes = "property_attributes"

let ivar_attributes = "ivar_attributes"

let unsafe_unret = "<\"Unsafe_unretained\">"

let weak = "<\"Weak\">"

let assign = "<\"Assign\">"

let start_filename = ".start"

(** Programming language. *)
type language = C_CPP | Java

(** current language *)
let curr_language = ref C_CPP

let string_of_language = function
  | Java -> "Java"
  | C_CPP -> "C_CPP"

let show_progress_bar = ref true

let nsnotification_center_checker_backend = ref false
