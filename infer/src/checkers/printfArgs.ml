(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

type printf_signature = {unique_id: string; format_pos: int; vararg_pos: int option}

let printf_like_functions =
  ref
    [ { unique_id=
          "java.io.PrintStream.printf(java.lang.String,java.lang.Object[]):java.io.PrintStream"
      ; format_pos= 1
      ; vararg_pos= Some 2 }
    ; { unique_id=
          "java.io.PrintStream.printf(java.lang.Locale,java.lang.String,java.lang.Object[]):java.io.PrintStream"
      ; format_pos= 2
      ; vararg_pos= Some 3 }
    ; { unique_id= "java.lang.String(java.lang.String,java.lang.Object[]):java.lang.String"
      ; format_pos= 1
      ; vararg_pos= Some 2 }
    ; { unique_id=
          "java.lang.String(java.lang.Locale,java.lang.String,java.lang.Object[]):java.lang.String"
      ; format_pos= 2
      ; vararg_pos= Some 3 } ]


let printf_like_function (proc_name : Typ.Procname.t) : printf_signature option =
  List.find
    ~f:(fun printf -> String.equal printf.unique_id (Typ.Procname.to_unique_id proc_name))
    !printf_like_functions


let default_format_type_name (format_type : string) : string =
  match format_type with
  | "d" | "i" | "u" | "x" | "X" | "o" ->
      "java.lang.Integer"
  | "a" | "A" | "f" | "F" | "g" | "G" | "e" | "E" ->
      "java.lang.Double"
  | "c" ->
      "java.lang.Character"
  | "b" ->
      "java.lang.Boolean"
  | "s" ->
      "java.lang.String"
  | "h" | "H" ->
      "java.lang.Object"
  | _ ->
      "unknown"


let format_type_matches_given_type (format_type : string) (given_type : string) : bool =
  match format_type with
  | "d" | "i" | "u" | "x" | "X" | "o" ->
      List.mem ~equal:String.equal
        ["java.lang.Integer"; "java.lang.Long"; "java.lang.Short"; "java.lang.Byte"]
        given_type
  | "a" | "A" | "f" | "F" | "g" | "G" | "e" | "E" ->
      List.mem ~equal:String.equal ["java.lang.Double"; "java.lang.Float"] given_type
  | "c" ->
      String.equal given_type "java.lang.Character"
  | "b" | "h" | "H" | "s" ->
      true (* accepts pretty much anything, even null *)
  | _ ->
      false


(* The format string and the nvar of the varargs array *)
let format_arguments (printf : printf_signature) (args : (Exp.t * Typ.t) list) :
    string option * Exp.t option =
  let format_string =
    match List.nth_exn args printf.format_pos with
    | Exp.Const (Const.Cstr fmt), _ ->
        Some fmt
    | _ ->
        None
  in
  let varargs_nvar =
    match printf.vararg_pos with Some pos -> Some (fst (List.nth_exn args pos)) | None -> None
  in
  (format_string, varargs_nvar)


(* Extract type names from format string *)
let rec format_string_type_names (fmt_string : string) (start : int) : string list =
  try
    let fmt_re = Str.regexp "%[0-9]*\\.?[0-9]*[A-mo-z]" in
    (* matches '%2.1d' etc. *)
    ignore (Str.search_forward fmt_re fmt_string start) ;
    let fmt_match = Str.matched_string fmt_string in
    let fmt_type = String.sub fmt_match ~pos:(String.length fmt_match - 1) ~len:1 in
    fmt_type :: format_string_type_names fmt_string (Str.match_end ())
  with Caml.Not_found -> []


let check_printf_args_ok tenv (node : Procdesc.Node.t) (instr : Sil.instr)
    (proc_name : Typ.Procname.t) (proc_desc : Procdesc.t) summary : unit =
  (* Check if format string lines up with arguments *)
  let rec check_type_names instr_loc n_arg instr_proc_name fmt_type_names arg_type_names =
    let instr_name = Typ.Procname.to_simplified_string instr_proc_name in
    let instr_line = Location.to_string instr_loc in
    match (fmt_type_names, arg_type_names) with
    | ft :: fs, gt :: gs ->
        if not (format_type_matches_given_type ft gt) then
          let description =
            Printf.sprintf
              "%s at line %s: parameter %d is expected to be of type %s but %s was given."
              instr_name instr_line n_arg (default_format_type_name ft) gt
          in
          Reporting.log_error summary ~loc:instr_loc IssueType.checkers_printf_args description
        else check_type_names instr_loc (n_arg + 1) instr_proc_name fs gs
    | [], [] ->
        ()
    | _ ->
        let description =
          Printf.sprintf "format string arguments don't mach provided arguments in %s at line %s"
            instr_name instr_line
        in
        Reporting.log_error summary ~loc:instr_loc IssueType.checkers_printf_args description
  in
  (* Get the array ivar for a given nvar *)
  let array_ivar instrs nvar =
    match nvar with
    | Exp.Var nid ->
        Instrs.find_map instrs ~f:(function
          | Sil.Load (id, Exp.Lvar iv, _, _) when Ident.equal id nid ->
              Some iv
          | _ ->
              None )
        |> IOption.find_value_exn
    | _ ->
        raise Caml.Not_found
  in
  match instr with
  | Sil.Call (_, Exp.Const (Const.Cfun pn), args, cl, _) -> (
    match printf_like_function pn with
    | Some printf -> (
      try
        let fmt, array_nvar = format_arguments printf args in
        let instrs = Procdesc.Node.get_instrs node in
        let vararg_ivar_type_names =
          match array_nvar with
          | Some nvar ->
              let ivar = array_ivar instrs nvar in
              PatternMatch.get_vararg_type_names tenv node ivar
          | None ->
              []
        in
        match fmt with
        | Some fmt ->
            check_type_names cl (printf.format_pos + 1) pn (format_string_type_names fmt 0)
              vararg_ivar_type_names
        | None ->
            if not (Reporting.is_suppressed tenv proc_desc IssueType.checkers_printf_args) then
              Reporting.log_warning summary ~loc:cl IssueType.checkers_printf_args
                "Format string must be string literal"
      with e ->
        L.internal_error "%s Exception when analyzing %s: %s@."
          IssueType.checkers_printf_args.unique_id
          (Typ.Procname.to_string proc_name)
          (Exn.to_string e) )
    | None ->
        () )
  | _ ->
      ()


let callback_printf_args {Callbacks.tenv; proc_desc; summary} : Summary.t =
  let proc_name = Procdesc.get_proc_name proc_desc in
  Procdesc.iter_instrs
    (fun n i -> check_printf_args_ok tenv n i proc_name proc_desc summary)
    proc_desc ;
  summary
