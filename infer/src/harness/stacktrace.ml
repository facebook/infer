(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Module for parsing stack traces and using them to guide Infer analysis *)

module L = Logging
module F = Format

type str_frame = {
  class_str : string;
  method_str : string;
  file_str : string;
  line_num : int;
}

(** A stack trace element whose procname / source file we can identify *)
type resolved_frame = {
  possible_calls : Procname.t list;
  file_name : DB.source_file;
  line_num : int;
}

type stack_frame =
  | Resolved of resolved_frame
  | Unresolved of str_frame

(** list representation of a stack trace. head of the list is the top of the stack (line/proc where
    exception occurs *)
type stack_trace = stack_frame list

(** given [str_frame], try to resolve its components in [exe_env] *)
let try_resolve_frame (str_frame : str_frame) exe_env tenv =
  try
    let class_name = Mangled.from_string str_frame.class_str in
    (* find the class name in the tenv and get the procedure(s) whose names match the procedure name
     * in the stack trace. Note that the stack trace does not have any type or argument information;
     * the name is all that we have to go on *)
    match Tenv.lookup tenv (Typename.TN_csu (Csu.Class Csu.Java, class_name)) with
    | Some Sil.Tstruct { Sil.csu = Csu.Class _; def_methods } ->
        let possible_calls =
          IList.filter
            (fun proc -> Procname.java_get_method proc = str_frame.method_str)
            def_methods in
        if IList.length possible_calls > 0 then
          (* using IList.hd here assumes that all of the possible calls are declared in the
           * same file, which will be true in Java but not necessarily in other languages *)
          let file_name = Exe_env.get_source exe_env (IList.hd possible_calls) in
          Resolved
            { possible_calls = possible_calls; file_name = file_name; line_num = str_frame.line_num; }
        else Unresolved str_frame
    | _ -> Unresolved str_frame
  with Not_found -> Unresolved str_frame

(** given a stack trace line like "at com.foo.Class.method(Class.java:42)" extract the class name,
    method name, file name, and line number *)
let parse_frame frame_str exe_env tenv =
  (* separate the qualified method name and the parenthesized text/line number*)
  ignore(Str.string_match (Str.regexp "at \\(.*\\)(\\(.*\\))") frame_str 0);
  let qualified_procname = Str.matched_group 1 frame_str in
  let file_and_line = Str.matched_group 2 frame_str in
  (* separate the class name from the method name *)
  ignore(Str.string_match (Str.regexp "\\(.*\\)\\.\\(.*\\)") qualified_procname 0);
  let class_str = Str.matched_group 1 qualified_procname in
  let method_str = Str.matched_group 2 qualified_procname in
  (* separate the filename and line number *)
  ignore(Str.string_match (Str.regexp "\\(.*\\):\\([0-9]+\\)") file_and_line 0);
  let file_str = Str.matched_group 1 file_and_line in
  let line_num = int_of_string (Str.matched_group 2 file_and_line) in
  try_resolve_frame
    { class_str = class_str; method_str = method_str; file_str = file_str; line_num = line_num }
    exe_env tenv

(** create an Infer-readable representation of a stack trace given its raw text *)
let parse_stack_trace trace_str exe_env =
  let tenv = Exe_env.get_tenv exe_env (IList.hd (Cg.get_defined_nodes (Exe_env.get_cg exe_env))) in
  let trace_list = Str.split (Str.regexp "\n") trace_str in
  IList.map (fun frame_str -> parse_frame frame_str exe_env tenv) trace_list

let pp_str_frame fmt = function
  | Resolved f ->
      F.fprintf fmt "Procs { %a }" (pp_semicolon_seq pe_text Procname.pp) f.possible_calls
  | Unresolved f ->
      F.fprintf fmt "UNRESOLVED: %s %s %s %d" f.class_str f.method_str f.file_str f.line_num

(*
let rec pp_str_stack_trace fmt = function
  | [] -> ()
  | frame :: rest -> F.fprintf fmt "%a;@\n%a" pp_str_frame frame pp_str_stack_trace rest
*)
