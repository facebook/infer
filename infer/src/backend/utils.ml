(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** General utility functions and definition with global scope *)

module F = Format

(** List police: don't use the list module to avoid non-tail recursive
    functions and builtin equality. Use IList instead. *)
module List = struct end

(** initial time of the analysis, i.e. when this module is loaded, gotten from Unix.time *)
let initial_analysis_time = Unix.time ()

(** precise time of day at the start of the analysis *)
let initial_timeofday = Unix.gettimeofday ()

(** {2 Generic Utility Functions} *)

(** Compare police: generic compare disabled. *)
let compare = ()

let string_equal (s1: string) (s2: string) = s1 = s2

let string_compare (s1: string) (s2: string) = Pervasives.compare s1 s2

let float_compare (f1: float) (f2: float) = Pervasives.compare f1 f2

let bool_compare (b1: bool) (b2: bool) = Pervasives.compare b1 b2

let bool_equal (b1: bool) (b2: bool) = b1 = b2

(** Extend and equality function to an option type. *)
let opt_equal cmp x1 x2 = match x1, x2 with
  | None, None -> true
  | Some _, None -> false
  | None, Some _ -> false
  | Some y1, Some y2 -> cmp y1 y2

(** Efficient comparison for integers *)
let int_compare (i: int) (j: int) = i - j

let int_equal (i: int) (j: int) = i = j

(** Generic comparison of pairs given a compare function for each element of the pair. *)
let pair_compare compare compare' (x1, y1) (x2, y2) =
  let n = compare x1 x2 in
  if n <> 0 then n else compare' y1 y2

(** Generic comparison of pairs given a compare function for each element of the triple *)
let triple_compare compare compare' compare'' (x1, y1, z1) (x2, y2, z2) =
  let n = compare x1 x2 in
  if n <> 0 then n else let n = compare' y1 y2 in
    if n <> 0 then n else compare'' z1 z2

(** {2 Useful Modules} *)

(** Set of integers *)
module IntSet =
  Set.Make(struct
    type t = int
    let compare = int_compare
  end)

(** Set of strings *)
module StringSet = Set.Make(String)

(** Pretty print a set of strings *)
let pp_stringset fmt ss =
  StringSet.iter (fun s -> F.fprintf fmt "%s " s) ss

(** Maps from integers *)
module IntMap = Map.Make (struct
    type t = int
    let compare = int_compare
  end)

(** Maps from strings *)
module StringMap = Map.Make (struct
    type t = string
    let compare (s1: string) (s2: string) = Pervasives.compare s1 s2 end)

(** {2 Printing} *)

(** Kind of simple printing: default or with full types *)
type pp_simple_kind = PP_SIM_DEFAULT | PP_SIM_WITH_TYP

(** Kind of printing *)
type printkind = PP_TEXT | PP_LATEX | PP_HTML

(** Colors supported in printing *)
type color = Black | Blue | Green | Orange | Red

(** map subexpressions (as Obj.t element compared by physical equality) to colors *)
type colormap = Obj.t -> color

(** Print environment threaded through all the printing functions *)
type printenv = {
  pe_opt : pp_simple_kind; (** Current option for simple printing *)
  pe_kind : printkind; (** Current kind of printing *)
  pe_cmap_norm : colormap; (** Current colormap for the normal part *)
  pe_cmap_foot : colormap; (** Current colormap for the footprint part *)
  pe_color : color; (** Current color *)
  pe_obj_sub : (Obj.t -> Obj.t) option (** generic object substitution *)
}

(** Create a colormap of a given color *)
let colormap_from_color color (_: Obj.t) = color

(** standard colormap: black *)
let colormap_black (_: Obj.t) = Black

(** red colormap *)
let colormap_red (_: Obj.t) = Red

(** Default text print environment *)
let pe_text =
  { pe_opt = PP_SIM_DEFAULT;
    pe_kind = PP_TEXT;
    pe_cmap_norm = colormap_black;
    pe_cmap_foot = colormap_black;
    pe_color = Black;
    pe_obj_sub = None }

(** Default html print environment *)
let pe_html color =
  { pe_text with
    pe_kind = PP_HTML;
    pe_cmap_norm = colormap_from_color color;
    pe_cmap_foot = colormap_from_color color;
    pe_color = color }

(** Default latex print environment *)
let pe_latex color =
  { pe_opt = PP_SIM_DEFAULT;
    pe_kind = PP_LATEX;
    pe_cmap_norm = colormap_from_color color;
    pe_cmap_foot = colormap_from_color color;
    pe_color = color;
    pe_obj_sub = None }

(** Extend the normal colormap for the given object with the given color *)
let pe_extend_colormap pe (x: Obj.t) (c: color) =
  let colormap (y: Obj.t) =
    if x == y then c
    else pe.pe_cmap_norm y in
  { pe with pe_cmap_norm = colormap }

(** Set the object substitution, which is supposed to preserve the type.
    Currently only used for a map from (identifier) expressions to the program var containing them *)
let pe_set_obj_sub pe (sub: 'a -> 'a) =
  let new_obj_sub x =
    let x' = Obj.repr (sub (Obj.obj x)) in
    match pe.pe_obj_sub with
    | None -> x'
    | Some sub' -> sub' x' in
  { pe with pe_obj_sub = Some (new_obj_sub) }

(** Reset the object substitution, so that no substitution takes place *)
let pe_reset_obj_sub pe =
  { pe with pe_obj_sub = None }

(** string representation of colors *)
let color_string = function
  | Black -> "color_black"
  | Blue -> "color_blue"
  | Green -> "color_green"
  | Orange -> "color_orange"
  | Red -> "color_red"

(** Pretty print a space-separated sequence *)
let rec pp_seq pp f = function
  | [] -> ()
  | [x] -> F.fprintf f "%a" pp x
  | x:: l -> F.fprintf f "%a %a" pp x (pp_seq pp) l

(** Print a comma-separated sequence *)
let rec pp_comma_seq pp f = function
  | [] -> ()
  | [x] -> F.fprintf f "%a" pp x
  | x:: l -> F.fprintf f "%a,%a" pp x (pp_comma_seq pp) l

(** Print a ;-separated sequence. *)
let rec _pp_semicolon_seq oneline pe pp f =
  let pp_sep fmt () =
    if oneline then F.fprintf fmt " " else F.fprintf fmt "@\n" in
  function
  | [] -> ()
  | [x] -> F.fprintf f "%a" pp x
  | x:: l ->
      (match pe.pe_kind with
       | PP_TEXT | PP_HTML ->
           F.fprintf f "%a ; %a%a" pp x pp_sep () (_pp_semicolon_seq oneline pe pp) l
       | PP_LATEX ->
           F.fprintf f "%a ;\\\\%a %a" pp x pp_sep () (_pp_semicolon_seq oneline pe pp) l)

(** Print a ;-separated sequence with newlines. *)
let pp_semicolon_seq pe = _pp_semicolon_seq false pe

(** Print a ;-separated sequence on one line. *)
let pp_semicolon_seq_oneline pe = _pp_semicolon_seq true pe

(** Print an or-separated sequence. *)
let pp_or_seq pe pp f = function
  | [] -> ()
  | [x] -> F.fprintf f "%a" pp x
  | x:: l ->
      (match pe.pe_kind with
       | PP_TEXT ->
           F.fprintf f "%a || %a" pp x (pp_semicolon_seq pe pp) l
       | PP_HTML ->
           F.fprintf f "%a &or; %a" pp x (pp_semicolon_seq pe pp) l
       | PP_LATEX ->
           F.fprintf f "%a \\vee %a" pp x (pp_semicolon_seq pe pp) l)

(** Produce a string from a 1-argument pretty printer function *)
let pp_to_string pp x =
  let buf = Buffer.create 1 in
  let fmt = Format.formatter_of_buffer buf in
  Format.fprintf fmt "%a@?" pp x;
  Buffer.contents buf

(** Print the current time and date in a format similar to the "date" command *)
let pp_current_time f () =
  let tm = Unix.localtime (Unix.time ()) in
  F.fprintf f "%02d/%02d/%4d %02d:%02d" tm.Unix.tm_mday tm.Unix.tm_mon (tm.Unix.tm_year + 1900) tm.Unix.tm_hour tm.Unix.tm_min

(** Print the time in seconds elapsed since the beginning of the execution of the current command. *)
let pp_elapsed_time fmt () =
  let elapsed = Unix.gettimeofday () -. initial_timeofday in
  Format.fprintf fmt "%f" elapsed

(** Type of location in ml source: __POS__ *)
type ml_loc = string * int * int * int

(** Convert a ml location to a string *)
let ml_loc_to_string (file, lnum, cnum, enum) =
  Printf.sprintf "%s:%d:%d-%d:" file lnum cnum enum

(** Pretty print a location of ml source *)
let pp_ml_loc fmt ml_loc =
  F.fprintf fmt "%s" (ml_loc_to_string ml_loc)

let pp_ml_loc_opt fmt ml_loc_opt =
  if !Config.developer_mode then match ml_loc_opt with
    | None -> ()
    | Some ml_loc -> F.fprintf fmt "(%a)" pp_ml_loc ml_loc

(** {2 SymOp and Failures: units of symbolic execution} *)

type failure_kind =
  | FKtimeout (* max time exceeded *)
  | FKsymops_timeout of int (* max symop's exceeded *)
  | FKrecursion_timeout of int (* max recursion level exceeded *)
  | FKcrash of string (* uncaught exception or failed assertion *)

(** failure that prevented analysis from finishing *)
exception Analysis_failure_exe of failure_kind

let exn_not_failure = function
  | Analysis_failure_exe _ -> false
  | _ -> true

let pp_failure_kind fmt = function
  | FKtimeout -> F.fprintf fmt "TIMEOUT"
  | FKsymops_timeout symops -> F.fprintf fmt "SYMOPS TIMEOUT (%d)" symops
  | FKrecursion_timeout level -> F.fprintf fmt "RECURSION TIMEOUT(%d)" level
  | FKcrash msg -> F.fprintf fmt "CRASH (%s)" msg

let symops_timeout, seconds_timeout =
  (* default timeout and long timeout are the same for now, but this will change shortly *)
  let default_symops_timeout = 333 in
  let default_seconds_timeout = 10.0 in
  let long_symops_timeout = 1000 in
  let long_seconds_timeout = 30.0 in
  if Config.analyze_models then
    (* use longer timeouts when analyzing models *)
    long_symops_timeout, long_seconds_timeout
  else
    default_symops_timeout, default_seconds_timeout

(** number of symops to multiply by the number of iterations, after which there is a timeout *)
let symops_per_iteration = ref symops_timeout

(** number of seconds to multiply by the number of iterations, after which there is a timeout *)
let seconds_per_iteration = ref seconds_timeout

(** Timeout in seconds for each function *)
let timeout_seconds = ref !seconds_per_iteration

(** Timeout in SymOps *)
let timeout_symops = ref !symops_per_iteration

(** Set the timeout values in seconds and symops, computed as a multiple of the integer parameter *)
let set_iterations n =
  timeout_symops := !symops_per_iteration * n;
  timeout_seconds := !seconds_per_iteration *. (float_of_int n)

let get_timeout_seconds () = !timeout_seconds

(** Count the number of symbolic operations *)
module SymOp = struct

  (** Internal state of the module *)
  type t =
    {
      (** Only throw timeout exception when alarm is active *)
      mutable alarm_active : bool;

      (** last wallclock set by an alarm, if any *)
      mutable last_wallclock : float option;

      (** Number of symop's *)
      mutable symop_count : int;

      (** Counter for the total number of symop's.
          The new state created when save_state is called shares this counter
          if keep_symop_total is true. Otherwise, a new counter is created. *)
      symop_total : int ref;
    }

  let initial () : t =
    {
      alarm_active = false;
      last_wallclock = None;
      symop_count = 0;
      symop_total = ref 0;
    }

  (** Global State *)
  let gs : t ref = ref (initial ())

  (** Restore the old state. *)
  let restore_state state =
    gs := state

  (** Return the old state, and revert the current state to the initial one.
      If keep_symop_total is true, share the total counter. *)
  let save_state ~keep_symop_total =
    let old_state = !gs in
    let new_state =
      let st = initial () in
      if keep_symop_total
      then
        { st with symop_total = old_state.symop_total }
      else
        st in
    gs := new_state;
    old_state

  (** handler for the wallclock timeout *)
  let wallclock_timeout_handler = ref None

  (** set the handler for the wallclock timeout *)
  let set_wallclock_timeout_handler handler =
    wallclock_timeout_handler := Some handler

  (** Set the wallclock alarm checked at every pay() *)
  let set_wallclock_alarm nsecs =
    !gs.last_wallclock <- Some (Unix.gettimeofday () +. nsecs)

  (** Unset the wallclock alarm checked at every pay() *)
  let unset_wallclock_alarm () =
    !gs.last_wallclock <- None

  (** if the wallclock alarm has expired, raise a timeout exception *)
  let check_wallclock_alarm () =
    match !gs.last_wallclock, !wallclock_timeout_handler with
    | Some alarm_time, Some handler when Unix.gettimeofday () >= alarm_time ->
        unset_wallclock_alarm ();
        handler ()
    | _ -> ()

  (** Return the time remaining before the wallclock alarm expires *)
  let get_remaining_wallclock_time () =
    match !gs.last_wallclock with
    | Some alarm_time ->
        max 0.0 (alarm_time -. Unix.gettimeofday ())
    | None ->
        0.0

  (** Return the total number of symop's since the beginning *)
  let get_total () =
    !(!gs.symop_total)

  (** Reset the total number of symop's *)
  let reset_total () =
    !gs.symop_total := 0

  (** Count one symop *)
  let pay () =
    !gs.symop_count <- !gs.symop_count + 1;
    !gs.symop_total := !(!gs.symop_total) + 1;
    if !gs.symop_count > !timeout_symops &&
       !gs.alarm_active
    then raise (Analysis_failure_exe (FKsymops_timeout !gs.symop_count));
    check_wallclock_alarm ()

  (** Reset the counter *)
  let reset_count () =
    !gs.symop_count <- 0

  (** Reset the counter and activate the alarm *)
  let set_alarm () =
    reset_count ();
    !gs.alarm_active <- true

  (** De-activate the alarm *)
  let unset_alarm () =
    !gs.alarm_active <- false
end

(** Check if the lhs is a substring of the rhs. *)
let string_is_prefix s1 s2 =
  String.length s1 <= String.length s2 &&
  String.sub s2 0 (String.length s1) = s1

(** Check if the lhs is a postfix of the rhs. *)
let string_is_suffix s1 s2 =
  let l1 = String.length s1 in
  let l2 = String.length s2 in
  l1 <= l2 &&
  String.sub s2 (l2 - l1) l1 = s1

(** Check if the lhs is contained in the rhs. *)
let string_contains s1 s2 =
  let rexp = Str.regexp_string s1 in
  try
    ignore (Str.search_forward rexp s2 0);
    true
  with Not_found -> false

(** Split a string across the given character, if given. (e.g. split first.second with '.').*)
let string_split_character s c =
  try
    let index = String.rindex s c in
    let lhs = String.sub s 0 index in
    let rhs = String.sub s (index + 1) ((String.length s) - (1 + index)) in
    (Some lhs, rhs)
  with Not_found -> (None, s)

let string_value_or_empty_string
    (string_option: string option): string =
  match string_option with
  | Some s -> s
  | None -> ""

(** read a source file and return a list of lines, or None in case of error *)
let read_file fname =
  let res = ref [] in
  let cin_ref = ref None in
  let cleanup () =
    match !cin_ref with
    | None -> ()
    | Some cin -> close_in cin in
  try
    let cin = open_in fname in
    cin_ref := Some cin;
    while true do
      let line = input_line cin in
      res := line :: !res
    done;
    assert false
  with
  | End_of_file ->
      cleanup ();
      Some (IList.rev !res)
  | Sys_error _ ->
      cleanup ();
      None

(** copy a source file, return the number of lines, or None in case of error *)
let copy_file fname_from fname_to =
  let res = ref 0 in
  let cin_ref = ref None in
  let cout_ref = ref None in
  let cleanup () =
    begin match !cin_ref with
      | None -> ()
      | Some cin -> close_in cin
    end;
    begin match !cout_ref with
      | None -> ()
      | Some cout -> close_out cout
    end in
  try
    let cin = open_in fname_from in
    cin_ref := Some cin;
    let cout = open_out fname_to in
    cout_ref := Some cout;
    while true do
      let line = input_line cin in
      output_string cout line;
      output_char cout '\n';
      incr res
    done;
    assert false
  with
  | End_of_file ->
      cleanup ();
      Some !res
  | Sys_error _ ->
      cleanup();
      None

module FileLOC = (** count lines of code of files and keep processed results in a cache *)
struct
  let include_loc_hash = Hashtbl.create 1

  let reset () = Hashtbl.clear include_loc_hash

  let file_get_loc fname =
    try Hashtbl.find include_loc_hash fname with Not_found ->
      let loc = match read_file fname with
        | None -> 0
        | Some l -> IList.length l in
      Hashtbl.add include_loc_hash fname loc;
      loc
end

(** type for files used for printing *)
type outfile =
  { fname : string; (** name of the file *)
    out_c : out_channel; (** output channel *)
    fmt : F.formatter (** formatter for printing *) }

(** create an outfile for the command line *)
let create_outfile fname =
  try
    let out_c = open_out fname in
    let fmt = F.formatter_of_out_channel out_c in
    Some { fname = fname; out_c = out_c; fmt = fmt }
  with Sys_error _ ->
    F.fprintf F.err_formatter "error: cannot create file %s@." fname;
    None

(** operate on an outfile reference if it is not None *)
let do_outf outf_ref f =
  match !outf_ref with
  | None -> ()
  | Some outf ->
      f outf

(** close an outfile *)
let close_outf outf =
  close_out outf.out_c

(** convert a filename to absolute path and normalize by removing occurrences of "." and ".." *)
module FileNormalize = struct
  let rec fname_to_list_rev fname =
    if fname = "" then [] else
      let base = Filename.basename fname in
      let dir = Filename.dirname fname in
      let does_not_split = (* make sure it terminates whatever the implementation of Filename *)
        fname = base || String.length dir >= String.length fname in
      if does_not_split then [fname]
      else base :: fname_to_list_rev dir

  (* split a file name into a list of strings representing it as a path *)
  let fname_to_list fname =
    IList.rev (fname_to_list_rev fname)

  (* concatenate a list of strings representing a path into a filename *)
  let rec list_to_fname base path = match path with
    | [] -> base
    | x :: path' -> list_to_fname (Filename.concat base x) path'

  (* normalize a path where done_l is a reversed path from the root already normalized *)
  (* and todo_l is the path still to normalize *)
  let rec normalize done_l todo_l = match done_l, todo_l with
    | _, y :: tl when y = Filename.current_dir_name -> (* path/. --> path *)
        normalize done_l tl
    | [_], y :: tl when y = Filename.parent_dir_name -> (* /.. --> / *)
        normalize done_l tl
    | _ :: dl, y :: tl when y = Filename.parent_dir_name -> (* path/x/.. --> path *)
        normalize dl tl
    | _, y :: tl -> normalize (y :: done_l) tl
    | _, [] -> IList.rev done_l

  (* check if the filename contains "." or ".." *)
  let fname_contains_current_parent fname =
    let l = fname_to_list fname in
    IList.exists (fun x -> x = Filename.current_dir_name || x = Filename.parent_dir_name) l

  (* convert a filename to absolute path, if necessary, and normalize "." and ".." *)
  let fname_to_absolute_normalize fname =
    let is_relative = Filename.is_relative fname in
    let must_normalize = fname_contains_current_parent fname in
    let simple_case () =
      if is_relative then Filename.concat (Unix.getcwd ()) fname
      else fname in
    if must_normalize then begin
      let done_l, todo_l =
        if is_relative then
          fname_to_list_rev (Unix.getcwd ()), fname_to_list fname
        else match fname_to_list fname with
          | [] -> [fname], [] (* should not happen *)
          | root :: l -> [root], l in
      let normal_l = normalize done_l todo_l in
      match normal_l with
      | base :: l -> list_to_fname base l
      | [] -> (* should not happen *) simple_case ()
    end
    else simple_case ()

  (*
  let test () =
  let test_fname fname =
  let fname' = fname_to_absolute_normalize fname in
  Format.fprintf Format.std_formatter "fname %s --> %s@." fname fname' in
  let tests = [".";
  "..";
  "aaa.c";
  "/";
  "/..";
  "../test.c";
  "src/../././test.c"] in
  List.map test_fname tests
  *)
end

(** Convert a filename to an absolute one if it is relative, and normalize "." and ".." *)
let filename_to_absolute fname =
  FileNormalize.fname_to_absolute_normalize fname

(** Convert an absolute filename to one relative to the current directory. *)
let filename_to_relative root fname =
  let string_strict_subtract s1 s2 =
    let n1, n2 = String.length s1, String.length s2 in
    if n1 < n2 && String.sub s2 0 n1 = s1 then
      String.sub s2 (n1 + 1) (n2 - (n1 + 1))
    else s2 in
  let norm_root = (* norm_root is root without any trailing / *)
    Filename.concat (Filename.dirname root) (Filename.basename root) in
  let remainder = (* remove the path prefix to root including trailing / *)
    string_strict_subtract norm_root fname in
  remainder


(* Type of command-line arguments before processing *)
type arg_list = (string * Arg.spec * string option * string) list

let arg_desc_filter options_to_keep =
  IList.filter (function (option_name, _, _, _) -> IList.mem string_equal option_name options_to_keep)

(* Given a filename with a list of paths, convert it into a list of string iff they are absolute *)
let read_specs_dir_list_file fname =
  let validate_path path =
    if Filename.is_relative path then
      failwith ("Failing because path " ^ path ^ " is not absolute") in
  match read_file fname with
  | Some pathlist ->
      IList.iter validate_path pathlist;
      pathlist
  | None -> failwith ("cannot read file " ^ fname)

let base_arg_desc =
  [
    "-results_dir",
    Arg.String (fun s -> Config.results_dir := s),
    Some "dir",
    "set the project results directory (default dir=" ^ Config.default_results_dir ^ ")";

    "-coverage",
    Arg.Unit (fun () -> Config.worklist_mode:= 2),
    None,
    "analysis mode to maximize coverage (can take longer)";

    "-lib",
    Arg.String (fun s -> Config.specs_library := filename_to_absolute s :: !Config.specs_library),
    Some "dir",
    "add dir to the list of directories to be searched for spec files";

    "-specs-dir-list-file",
    Arg.String (fun s -> Config.specs_library := (read_specs_dir_list_file s) @ !Config.specs_library),
    Some "file",
    "add the newline-separated directories listed in <file> to the list of directories to \
     be searched for spec files";

    "-models",
    Arg.String (fun s -> Config.add_models (filename_to_absolute s)),
    Some "zip file",
    "add a zip file containing the models";

    "-ziplib",
    Arg.String (fun s -> Config.add_zip_library (filename_to_absolute s)),
    Some "zip file",
    "add a zip file containing library spec files";

    "-project_root",
    Arg.String (fun s -> Config.project_root := Some (filename_to_absolute s)),
    Some "dir",
    "root directory of the project";

    "-infer_cache",
    Arg.String (fun s -> Config.JarCache.infer_cache := Some (filename_to_absolute s)),
    Some "dir",
    "Select a directory to contain the infer cache";

    "-inferconfig_home",
    Arg.String (fun s -> Config.inferconfig_home := Some s),
    Some "dir",
    "Path to the .inferconfig file";
  ]

let reserved_arg_desc =
  [
    "-absstruct",
    Arg.Set_int Config.abs_struct,
    Some "n",
    "abstraction level for fields of structs (default n = 1)"
    ;
    "-absval",
    Arg.Set_int Config.abs_val,
    Some "n",
    "abstraction level for expressions (default n = 2)";
    "-arraylevel",
    Arg.Set_int Config.array_level,
    Some "n",
    "the level of treating the array indexing and pointer arithmetic (default n = 0)"
    ;
    "-developer_mode",
    Arg.Set Config.developer_mode,
    None,
    "reserved"
    ;
    "-dotty",
    Arg.Set Config.write_dotty,
    None,
    "produce dotty files in the results directory";
    "-exit_node_bias",
    Arg.Unit (fun () -> Config.worklist_mode:= 1),
    None,
    "nodes nearest the exit node are analyzed first";
    "-html",
    Arg.Set Config.write_html,
    None,
    "produce hmtl output in the results directory"
    ;
    "-join_cond",
    Arg.Set_int Config.join_cond,
    Some "n",
    "set the strength of the final information-loss check used by the join (default n=1)"
    ;
    "-leak",
    Arg.Set Config.allowleak,
    None,
    "forget leaks during abstraction"
    ;
    "-monitor_prop_size",
    Arg.Set Config.monitor_prop_size,
    None,
    "monitor size of props"
    ;
    "-nelseg",
    Arg.Set Config.nelseg,
    None,
    "use only nonempty lsegs"
    ;
    "-noliveness",
    Arg.Clear Config.liveness,
    None,
    "turn the dead program variable elimination off"
    ;
    "-noprintdiff",
    Arg.Clear Config.print_using_diff,
    None,
    "turn off highlighting diff w.r.t. previous prop in printing"
    ;
    "-notest",
    Arg.Clear Config.test,
    None,
    "turn test mode off"
    ;
    "-only_footprint",
    Arg.Set Config.only_footprint,
    None,
    "skip the re-execution phase"
    ;
    "-print_types",
    Arg.Set Config.print_types,
    None,
    "print types in symbolic heaps"
    ;
    "-set_pp_margin",
    Arg.Int (fun i -> F.set_margin i),
    Some "n",
    "set right margin for the pretty printing functions"
    ;
    "-spec_abs_level",
    Arg.Set_int Config.spec_abs_level,
    Some "n",
    "set the level of abstracting the postconditions of discovered specs (default n=1)"
    ;
    "-trace_error",
    Arg.Set Config.trace_error,
    None,
    "turn on tracing of error explanation"
    ;
    "-trace_join",
    Arg.Set Config.trace_join,
    None,
    "turn on tracing of join"
    ;
    "-trace_rearrange",
    Arg.Set Config.trace_rearrange,
    None,
    "turn on tracing of rearrangement"
    ;
    "-visits_bias",
    Arg.Unit (fun () -> Config.worklist_mode:= 2),
    None,
    "nodes visited fewer times are analyzed first"
    ;
  ]


module Arg = struct

  include Arg

  (** Custom version of Arg.aling so that keywords are on one line and documentation is on the next *)
  let align arg_desc =
    let do_arg (key, spec, doc) =
      let first_space =
        try
          let index = String.index doc ' ' in
          if String.get doc index = '=' then 0 else index
        with Not_found -> 0 in
      let len = String.length doc in
      let doc1 = String.sub doc 0 first_space in
      let doc2 = String.sub doc first_space (len - first_space) in
      if len = 0 then (key, spec, doc)
      else (key, spec, doc1 ^ "\n     " ^ doc2) in
    IList.map do_arg arg_desc

  type aligned = (key * spec * doc)

  (** Create a group of sorted command-line arguments *)
  let create_options_desc double_minus title unsorted_desc =
    let handle_double_minus (opname, spec, param_opt, text) = match param_opt with
      | None ->
          if double_minus then ("-"^opname, spec, " " ^ text)
          else (opname, spec, " " ^ text)
      | Some param ->
          if double_minus then ("-"^opname, spec, "=" ^ param ^ " " ^ text)
          else (opname, spec, param ^ " " ^ text) in
    let unsorted_desc' = IList.map handle_double_minus unsorted_desc in
    let dlist =
      ("", Arg.Unit (fun () -> ()), " \n  " ^ title ^ "\n") ::
      IList.sort (fun (x, _, _) (y, _, _) -> Pervasives.compare x y) unsorted_desc' in
    align dlist

  let env_to_argv env =
    Str.split (Str.regexp ":") env

  let prepend_to_argv args =
    let cl_args = match Array.to_list Sys.argv with _ :: tl -> tl | [] -> [] in
    Sys.executable_name :: args @ cl_args

  let parse env_var spec anon usage =
    let env_args = env_to_argv (try Unix.getenv env_var with Not_found -> "") in
    let env_cl_args = prepend_to_argv env_args in
    try
      Arg.parse_argv (Array.of_list env_cl_args) spec anon usage
    with
    | Bad usage -> Pervasives.prerr_string usage; exit 2;
    | Help usage -> Pervasives.print_string usage; exit 0;

end

(** flags for a procedure *)
type proc_flags = (string, string) Hashtbl.t

let proc_flags_empty () : proc_flags = Hashtbl.create 1

let proc_flag_skip = "skip"
let proc_flag_ignore_return = "ignore_return"

let proc_flags_add proc_flags key value =
  Hashtbl.replace proc_flags key value

let proc_flags_find proc_flags key =
  Hashtbl.find proc_flags key

let join_strings sep = function
  | [] -> ""
  | hd:: tl ->
      IList.fold_left (fun str p -> str ^ sep ^ p) hd tl

let next compare =
  fun x y n ->
    if n <> 0 then n
    else compare x y


let directory_fold f init path =
  let collect current_dir (accu, dirs) path =
    let full_path = (Filename.concat current_dir path) in
    try
      if Sys.is_directory full_path then
        (accu, full_path:: dirs)
      else
        (f accu full_path, dirs)
    with Sys_error _ ->
      (accu, dirs) in
  let rec loop accu dirs =
    match dirs with
    | [] -> accu
    | d:: tl ->
        let (new_accu, new_dirs) = Array.fold_left (collect d) (accu, tl) (Sys.readdir d) in
        loop new_accu new_dirs in
  if Sys.is_directory path then
    loop init [path]
  else
    f init path


let directory_iter f path =
  let apply current_dir dirs path =
    let full_path = (Filename.concat current_dir path) in
    try
      if Sys.is_directory full_path then
        full_path:: dirs
      else
        let () = f full_path in
        dirs
    with Sys_error _ ->
      dirs in
  let rec loop dirs =
    match dirs with
    | [] -> ()
    | d:: tl ->
        let new_dirs = Array.fold_left (apply d) tl (Sys.readdir d) in
        loop new_dirs in
  if Sys.is_directory path then
    loop [path]
  else
    f path

type analyzer = Infer | Eradicate | Checkers | Tracing

let analyzers = [Infer; Eradicate; Checkers; Tracing]

let string_of_analyzer = function
  | Infer -> "infer"
  | Eradicate -> "eradicate"
  | Checkers -> "checkers"
  | Tracing -> "tracing"

exception Unknown_analyzer

let analyzer_of_string = function
  | "infer" -> Infer
  | "eradicate" -> Eradicate
  | "checkers" -> Checkers
  | "tracing" -> Tracing
  | _ -> raise Unknown_analyzer


let string_crc_hex32 =
  Config.string_crc_hex32 (* implemented in Config to avoid circularities *)

let string_append_crc_cutoff ?(cutoff=100) ?(key="") name =
  let name_up_to_cutoff =
    if String.length name <= cutoff
    then name
    else String.sub name 0 cutoff in
  let crc_str =
    let name_for_crc = name ^ key in
    string_crc_hex32 name_for_crc in
  name_up_to_cutoff ^ "." ^ crc_str

let set_reference_and_call_function reference value f x =
  let saved = !reference in
  let restore () =
    reference := saved in
  try
    reference := value;
    let res = f x in
    restore ();
    res
  with
  | exn ->
      restore ();
      raise exn

let run_in_re_execution_mode f x =
  set_reference_and_call_function Config.footprint false f x

let run_in_footprint_mode f x =
  set_reference_and_call_function Config.footprint true f x

let run_with_abs_val_equal_zero f x =
  set_reference_and_call_function Config.abs_val 0 f x

let assert_false ((file, lnum, cnum, _) as ml_loc) =
  Printf.eprintf "\nASSERT FALSE %s\nCALL STACK\n%s\n%!"
    (ml_loc_to_string ml_loc)
    (Printexc.raw_backtrace_to_string (Printexc.get_callstack 1000));
  raise (Assert_failure (file, lnum, cnum))
