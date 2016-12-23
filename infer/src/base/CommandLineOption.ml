(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Definition and parsing of command line arguments *)

open! IStd

module F = Format
module YBU = Yojson.Basic.Util


(** Each command line option may appear in the --help list of any executable, these tags are used to
    specify which executables for which an option will be documented. *)
type exe = Analyze | Clang | Interactive | Java | Print | Toplevel


(** Association list of executable (base)names to their [exe]s. *)
let exes = [
  ("InferAnalyze", Analyze);
  ("InferClang", Clang);
  ("InferJava", Java);
  ("InferPrint", Print);
  ("infer", Toplevel);
  ("interactive", Interactive);
]

let exe_name =
  let exe_to_name = IList.map (fun (n,a) -> (a,n)) exes in
  fun exe -> IList.assoc (=) exe exe_to_name


let frontend_exes = [Clang; Java]

type desc = {
  long: string; short: string; meta: string; doc: string; spec: Arg.spec;
  (** how to go from an option in the json config file to a list of command-line options *)
  decode_json: Yojson.Basic.json -> string list ;
}

let dashdash long =
  match long with
  | "" | "--" -> long
  | _ -> "--" ^ long

let short_meta {short; meta; spec} =
  String.concat ~sep:" "
    ((if short = "" then [] else ["| -" ^ short]) @
     (match spec with
      | Arg.Symbol (symbols, _) ->
          ["{ " ^ (String.concat ~sep:" | " symbols) ^ " }" ^ meta]
      | _ ->
          if meta = "" then [] else ["<" ^ meta ^ ">"]))

let left_length long short_meta =
  (String.length (dashdash long)) + (String.length short_meta)

let max_left_length limit current ({long; spec} as desc) =
  let short_meta =
    match spec with
    | Arg.Symbol _ -> short_meta {desc with spec = Arg.Unit (fun () -> ())}
    | _ -> short_meta desc in
  let length = left_length long short_meta in
  if length > limit then current else max current length

let xdesc {long; short; spec; doc} =
  let key long short =
    match long, short with
    | "", "" -> ""
    | "--", _ -> "--"
    | "", _ -> "-" ^ short
    | _ -> "--" ^ long
  in
  let xspec long spec =
    match spec with
    (* translate Symbol to String for better formatting of --help messages *)
    | Arg.Symbol (symbols, action) ->
        Arg.String (fun arg ->
            if IList.mem ( = ) arg symbols then
              action arg
            else
              raise (Arg.Bad (F.sprintf "wrong argument '%s'; option '%s' expects one of: %s"
                                arg (dashdash long) (String.concat ~sep:" | " symbols)))
          )
    | _ ->
        spec
  in
  (key long short, xspec long spec, doc)

let wrap_line indent_string wrap_length line =
  let indent_length = String.length indent_string in
  let word_sep = " " in
  let words = Str.split (Str.regexp_string word_sep) line in
  let add_word_to_paragraph (rev_lines, non_empty, line, line_length) word =
    let word_length = String.length word in
    let new_length = line_length + (String.length word_sep) + word_length in
    let new_non_empty = non_empty || word <> "" in
    if new_length > wrap_length && non_empty then
      (line::rev_lines, true, indent_string ^ word, indent_length + word_length)
    else
      let sep = if line_length = indent_length then "" else word_sep in
      let new_line = line ^ sep ^ word in
      if new_length > wrap_length && new_non_empty then
        (new_line::rev_lines, false, indent_string, indent_length)
      else
        (rev_lines, new_non_empty, new_line, String.length new_line) in
  let (rev_lines, _, line, _) = IList.fold_left add_word_to_paragraph ([], false, "", 0) words in
  IList.rev (line::rev_lines)

let pad_and_xform doc_width left_width desc =
  match desc with
  | {doc = ""} ->
      xdesc desc
  | {long; doc} ->
      let indent_doc doc =
        (* 2 blank columns before option + 2 columns of gap between flag and doc *)
        let left_indent = 4 + left_width in
        (* align every line after the first one of [doc] *)
        let doc = Str.global_replace (Str.regexp_string "\n")
            ("\n" ^ String.make left_indent ' ') doc in
        (* align the first line of [doc] *)
        let short_meta = short_meta desc in
        let gap = left_width - (left_length long short_meta) in
        if gap < 0 then
          short_meta ^ "\n" ^ (String.make left_indent ' ') ^ doc
        else
          short_meta ^ (String.make (gap + 1) ' ') ^ doc
      in
      let wrapped_lines =
        let lines = Str.split (Str.regexp_string "\n") doc in
        let wrap_line s =
          if String.length s > doc_width then
            wrap_line "" doc_width s
          else [s] in
        IList.map wrap_line lines in
      let doc = indent_doc (String.concat ~sep:"\n" (IList.flatten wrapped_lines)) in
      xdesc {desc with doc}

let align desc_list =
  let min_term_width = 80 in
  let cur_term_width =
    match Lazy.force IOCtl.terminal_width with
    | Ok width -> width
    | Error _ -> min_term_width in
  (* 2 blank columns before option + 2 columns of gap between flag and doc *)
  let extra_space = 4 in
  let min_left_width = 15 in
  let max_left_width = 49 in
  let doc_width term_width left_width = term_width - extra_space - left_width in
  let term_width doc_width left_width = left_width + extra_space + doc_width in
  let max_doc_width = 100 in
  let max_term_width = term_width max_left_width max_doc_width in
  (* how many columns to reserve for the option names
     NOTE: this doesn't take into account "--help | -h" nor "--help-full", but fortunately these
     have short names *)
  let left_width =
    let opt_left_width = IList.fold_left (max_left_length max_left_width) 0 desc_list in
    let (--) a b = float_of_int a -. float_of_int b in
    let multiplier = (max_left_width -- min_left_width) /. (max_term_width -- min_term_width) in
    (* at 80 columns use min_left_width then use extra columns until opt_left_width *)
    let cols_after_min_width = float_of_int (max 0 (cur_term_width - min_term_width)) in
    min (int_of_float (cols_after_min_width *. multiplier) + min_left_width) opt_left_width in
  let doc_width = min max_doc_width (doc_width cur_term_width left_width) in
  (IList.map (pad_and_xform doc_width left_width) desc_list, (doc_width, left_width))


let check_no_duplicates desc_list =
  let rec check_for_duplicates_ = function
    | [] | [_] ->
        true
    | (x, _, _) :: (y, _, _) :: _ when x <> "" && x = y ->
        failwith ("Multiple definitions of command line option: " ^ x)
    | _ :: tl ->
        check_for_duplicates_ tl
  in
  check_for_duplicates_ (IList.sort (fun (x, _, _) (y, _, _) -> String.compare x y) desc_list)


let full_desc_list = ref []

let exe_desc_lists = IList.map (fun (_, exe) -> (exe, ref [])) exes

(** add desc to all desc_lists for the purposes of parsing, include desc in --help only for exes *)
let add exes desc =
  full_desc_list := desc :: !full_desc_list ;
  IList.iter (fun (exe, desc_list) ->
      let desc =
        if IList.mem ( = ) exe exes then
          desc
        else
          {desc with meta = ""; doc = ""} in
      desc_list := desc :: !desc_list
    ) exe_desc_lists

let mk ?(deprecated=[]) ?(exes=[])
    ~long ?(short="") ~default ~meta doc ~default_to_string ~decode_json ~mk_setter ~mk_spec =
  let variable = ref default in
  let closure = mk_setter variable in
  let setter str =
    try closure str
    with exc ->
      raise (Arg.Bad ("bad value " ^ str ^ " for flag " ^ long
                      ^ " (" ^ (Exn.to_string exc) ^ ")")) in
  let spec = mk_spec setter in
  let doc =
    let default_string = default_to_string default in
    if default_string = "" then doc
    else doc ^ " (default: " ^ default_string ^ ")" in
  let desc = {long; short; meta; doc; spec; decode_json} in
  (* add desc for long option, with documentation (which includes any short option) for exes *)
  add exes desc ;
  (* add desc for short option only for parsing, without documentation *)
  if short <> "" then
    add [] {desc with long = ""; meta = ""; doc = ""} ;
  (* add desc for deprecated options only for parsing, without documentation *)
  IList.iter (fun deprecated ->
      add [] {desc with long = ""; short = deprecated; meta = ""; doc = ""}
    ) deprecated ;
  variable

(* arguments passed to Arg.parse_argv_dynamic, susceptible to be modified on the fly when parsing *)
let args_to_parse : string array ref = ref (Array.of_list [])

(* reference used by Arg.parse_argv_dynamic to track the index of the argument being parsed *)
let arg_being_parsed : int ref = ref 0

(* list of arg specifications currently being used by Arg.parse_argv_dynamic *)
let curr_speclist : (Arg.key * Arg.spec * Arg.doc) list ref = ref []

type 'a t =
  ?deprecated:string list -> long:Arg.key -> ?short:Arg.key ->
  ?exes:exe list -> ?meta:string -> Arg.doc ->
  'a

let string_json_decoder ~long json = [dashdash long; YBU.to_string json]

let list_json_decoder json_decoder json = IList.flatten (YBU.convert_each json_decoder json)

let mk_set var value ?(deprecated=[]) ~long ?short ?exes ?(meta="") doc =
  let setter () = var := value in
  ignore(
    mk ~deprecated ~long ?short ~default:() ?exes ~meta doc
      ~default_to_string:(fun () -> "")
      ~decode_json:(string_json_decoder ~long)
      ~mk_setter:(fun _ _ -> setter ())
      ~mk_spec:(fun _ -> Arg.Unit setter) )

let mk_option ?(default=None) ?(default_to_string=fun _ -> "") ~f
    ?(deprecated=[]) ~long ?short ?exes ?(meta="") doc =
  mk ~deprecated ~long ?short ~default ?exes ~meta doc
    ~default_to_string
    ~decode_json:(string_json_decoder ~long)
    ~mk_setter:(fun var str -> var := f str)
    ~mk_spec:(fun set -> Arg.String set)

let mk_bool ?(deprecated_no=[]) ?(default=false) ?(f=fun b -> b)
    ?(deprecated=[]) ~long ?short ?exes ?(meta="") doc =
  let nolong =
    let len = String.length long in
    if len > 3 && String.sub long ~pos:0 ~len:3 = "no-" then
      String.sub long ~pos:3 ~len:(len - 3)
    else
      "no-" ^ long
  and noshort =
    Option.map ~f:(fun short ->
        let len = String.length short in
        if len > 1 && String.sub short ~pos:0 ~len:1 = "n" then
          String.sub short ~pos:1 ~len:(len - 1)
        else
          "n" ^ short
      ) short
  in
  let doc nolong =
    match noshort with
    | Some noshort -> doc ^ " (Conversely: --" ^ nolong ^ " | -" ^ noshort ^ ")"
    | None         -> doc ^ " (Conversely: --" ^ nolong ^ ")"
  in
  let doc, nodoc =
    if not default then
      ("Activates: " ^ doc nolong, "")
    else
      ("", "Deactivates: " ^ doc long) in
  let default_to_string _ = "" in
  let mk_spec set = Arg.Unit (fun () -> set "") in
  let var =
    mk ~long ?short ~deprecated ~default ?exes
      ~meta doc ~default_to_string ~mk_setter:(fun var _ -> var := f true)
      ~decode_json:(fun json ->
          [dashdash (if YBU.to_bool json then long else nolong)])
      ~mk_spec in
  ignore(
    mk ~long:nolong ?short:noshort ~deprecated:deprecated_no ~default:(not default) ?exes
      ~meta nodoc ~default_to_string ~mk_setter:(fun _ _ -> var := f false)
      ~decode_json:(fun json ->
          [dashdash (if YBU.to_bool json then nolong else long)])
      ~mk_spec );
  var

let mk_bool_group ?(deprecated_no=[]) ?(default=false)
    ?(deprecated=[]) ~long ?short ?exes ?(meta="") doc children no_children =
  let f b =
    IList.iter (fun child -> child := b) children ;
    IList.iter (fun child -> child := not b) no_children ;
    b
  in
  mk_bool ~deprecated ~deprecated_no ~default ~long ?short ~f ?exes ~meta doc

let mk_int ~default ?(deprecated=[]) ~long ?short ?exes ?(meta="") doc =
  mk ~deprecated ~long ?short ~default ?exes ~meta doc
    ~default_to_string:string_of_int
    ~mk_setter:(fun var str -> var := (int_of_string str))
    ~decode_json:(string_json_decoder ~long)
    ~mk_spec:(fun set -> Arg.String set)

let mk_int_opt ?default ?(deprecated=[]) ~long ?short ?exes ?(meta="") doc =
  let default_to_string = function Some f -> string_of_int f | None -> "" in
  let f s = Some (int_of_string s) in
  mk_option ~deprecated ~long ?short ~default ~default_to_string ~f ?exes ~meta doc

let mk_float ~default ?(deprecated=[]) ~long ?short ?exes ?(meta="") doc =
  mk ~deprecated ~long ?short ~default ?exes ~meta doc
    ~default_to_string:string_of_float
    ~mk_setter:(fun var str -> var := (float_of_string str))
    ~decode_json:(string_json_decoder ~long)
    ~mk_spec:(fun set -> Arg.String set)

let mk_float_opt ?default ?(deprecated=[]) ~long ?short ?exes ?(meta="") doc =
  let default_to_string = function Some f -> string_of_float f | None -> "" in
  let f s = Some (float_of_string s) in
  mk_option ~deprecated ~long ?short ~default ~default_to_string ~f ?exes ~meta doc

let mk_string ~default ?(f=fun s -> s) ?(deprecated=[]) ~long ?short ?exes ?(meta="") doc =
  mk ~deprecated ~long ?short ~default ?exes ~meta doc
    ~default_to_string:(fun s -> s)
    ~mk_setter:(fun var str -> var := f str)
    ~decode_json:(string_json_decoder ~long)
    ~mk_spec:(fun set -> Arg.String set)

let mk_string_opt ?default ?(f=fun s -> s) ?(deprecated=[]) ~long ?short ?exes ?(meta="") doc =
  let default_to_string = function Some s -> s | None -> "" in
  let f s = Some (f s) in
  mk_option ~deprecated ~long ?short ~default ~default_to_string ~f ?exes ~meta doc

let mk_string_list ?(default=[]) ?(f=fun s -> s)
    ?(deprecated=[]) ~long ?short ?exes ?(meta="") doc =
  mk ~deprecated ~long ?short ~default ?exes ~meta doc
    ~default_to_string:(String.concat ~sep:", ")
    ~mk_setter:(fun var str -> var := (f str) :: !var)
    ~decode_json:(list_json_decoder (string_json_decoder ~long))
    ~mk_spec:(fun set -> Arg.String set)

let mk_path_helper ~setter ~default_to_string
    ~default ~deprecated ~long ~short ~exes ~meta ~decode_json doc =
  let normalize_path_in_args_being_parsed str =
    if Filename.is_relative str then (
      (* Replace relative paths with absolute ones on the fly in the args being parsed. This assumes
         that [!arg_being_parsed] points at the option name position in [!args_to_parse], as is the
         case e.g. when calling
         [Arg.parse_argv_dynamic ~current:arg_being_parsed !args_to_parse ...]. *)
      let abs_path = Utils.filename_to_absolute str in
      (!args_to_parse).(!arg_being_parsed + 1) <- abs_path;
      abs_path
    ) else
      str in
  mk ~deprecated ~long ?short ~default ?exes ~meta doc
    ~decode_json ~default_to_string
    ~mk_setter:(fun var str ->
        let abs_path = normalize_path_in_args_being_parsed str in
        setter var abs_path)
    ~mk_spec:(fun set -> Arg.String set)

let mk_path ~default ?(deprecated=[]) ~long ?short ?exes ?(meta="path") =
  mk_path_helper
    ~setter:(fun var x -> var := x)
    ~decode_json:(string_json_decoder ~long)
    ~default_to_string:(fun s -> s)
    ~default ~deprecated ~long ~short ~exes ~meta

let mk_path_opt ?default ?(deprecated=[]) ~long ?short ?exes ?(meta="path") =
  mk_path_helper
    ~setter:(fun var x -> var := Some x)
    ~decode_json:(string_json_decoder ~long)
    ~default_to_string:(function Some s -> s | None -> "")
    ~default ~deprecated ~long ~short ~exes ~meta

let mk_path_list ?(default=[]) ?(deprecated=[]) ~long ?short ?exes ?(meta="path") =
  mk_path_helper
    ~setter:(fun var x -> var := x :: !var)
    ~decode_json:(list_json_decoder (string_json_decoder ~long))
    ~default_to_string:(String.concat ~sep:", ")
    ~default ~deprecated ~long ~short ~exes ~meta

let mk_symbol ~default ~symbols ?(deprecated=[]) ~long ?short ?exes ?(meta="") doc =
  let strings = IList.map fst symbols in
  let sym_to_str = IList.map (fun (x,y) -> (y,x)) symbols in
  let of_string str = IList.assoc String.equal str symbols in
  let to_string sym = IList.assoc ( = ) sym sym_to_str in
  mk ~deprecated ~long ?short ~default ?exes ~meta doc
    ~default_to_string:(fun s -> to_string s)
    ~mk_setter:(fun var str -> var := of_string str)
    ~decode_json:(string_json_decoder ~long)
    ~mk_spec:(fun set -> Arg.Symbol (strings, set))

let mk_symbol_opt ~symbols ?(deprecated=[]) ~long ?short ?exes ?(meta="") doc =
  let strings = IList.map fst symbols in
  let of_string str = IList.assoc String.equal str symbols in
  mk ~deprecated ~long ?short ~default:None ?exes ~meta doc
    ~default_to_string:(fun _ -> "")
    ~mk_setter:(fun var str -> var := Some (of_string str))
    ~decode_json:(string_json_decoder ~long)
    ~mk_spec:(fun set -> Arg.Symbol (strings, set))

let mk_symbol_seq ?(default=[]) ~symbols ?(deprecated=[]) ~long ?short ?exes ?(meta="") doc =
  let sym_to_str = IList.map (fun (x,y) -> (y,x)) symbols in
  let of_string str = IList.assoc String.equal str symbols in
  let to_string sym = IList.assoc ( = ) sym sym_to_str in
  mk ~deprecated ~long ?short ~default ?exes ~meta:(",-separated sequence" ^ meta) doc
    ~default_to_string:(fun syms -> String.concat ~sep:" " (IList.map to_string syms))
    ~mk_setter:(fun var str_seq ->
        var := IList.map of_string (Str.split (Str.regexp_string ",") str_seq))
    ~decode_json:(fun json ->
        [dashdash long;
         String.concat ~sep:"," (YBU.convert_each YBU.to_string json)])
    ~mk_spec:(fun set -> Arg.String set)

let mk_set_from_json ~default ~default_to_string ~f
    ?(deprecated=[]) ~long ?short ?exes ?(meta="json") doc =
  mk ~deprecated ~long ?short ?exes ~meta doc
    ~default ~default_to_string
    ~mk_setter:(fun var json -> var := f (Yojson.Basic.from_string json))
    ~decode_json:(fun json -> [dashdash long; Yojson.Basic.to_string json])
    ~mk_spec:(fun set -> Arg.String set)

let mk_json ?(deprecated=[]) ~long ?short ?exes ?(meta="json") doc =
  mk ~deprecated ~long ?short ?exes ~meta doc
    ~default:(`List []) ~default_to_string:Yojson.Basic.to_string
    ~mk_setter:(fun var json -> var := Yojson.Basic.from_string json)
    ~decode_json:(fun json -> [dashdash long; Yojson.Basic.to_string json])
    ~mk_spec:(fun set -> Arg.String set)

(** A ref to a function used during argument parsing to process anonymous arguments. By default,
    anonymous arguments are rejected. *)
let anon_fun = ref (fun arg -> raise (Arg.Bad ("unexpected anonymous argument: " ^ arg)))

(** Clients declare that anonymous arguments are acceptable by calling [mk_anon], which returns a
    ref storing the anonymous arguments. *)
let mk_anon () =
  let anon = ref [] in
  anon_fun := (fun arg -> anon := arg :: !anon) ;
  anon

let mk_rest ?(exes=[]) doc =
  let rest = ref [] in
  let spec = Arg.Rest (fun arg -> rest := arg :: !rest) in
  add exes {long = "--"; short = ""; meta = ""; doc; spec; decode_json = fun _ -> []} ;
  rest

let accept_unknown_args = ref false

let mk_subcommand ?(exes=[]) doc command_to_speclist =
  let rest = ref [] in
  let spec =
    Arg.String (fun arg ->
        rest := List.rev (Array.to_list (Array.slice !args_to_parse (!arg_being_parsed + 1) 0)) ;
        accept_unknown_args := true ;
        anon_fun := (fun _ -> ()) ;
        curr_speclist := command_to_speclist arg
      ) in
  add exes {long = "--"; short = ""; meta = ""; doc; spec; decode_json = fun _ -> []} ;
  rest


let decode_inferconfig_to_argv current_exe path =
  let json = match Utils.read_optional_json_file path with
    | Ok json ->
        json
    | Error msg ->
        F.eprintf "WARNING: Could not read or parse Infer config in %s:@\n%s@." path msg ;
        `Assoc [] in
  let desc_list = !(IList.assoc ( = ) current_exe exe_desc_lists) in
  let json_config = YBU.to_assoc json in
  let one_config_item result (key, json_val) =
    try
      let {decode_json} =
        IList.find
          (fun {long; short} ->
             String.equal key long
             || (* for deprecated options *) String.equal key short)
          desc_list in
      decode_json json_val @ result
    with
    | Not_found ->
        F.eprintf "WARNING: while reading config file %s:@\nUnknown option %s@." path key ;
        result
    | YBU.Type_error (msg, json) ->
        F.eprintf "WARNING: while reading config file %s:@\nIll-formed value %s for option %s: %s@."
          path (Yojson.Basic.to_string json) key msg ;
        result in
  IList.fold_left one_config_item [] json_config


(** separator of argv elements when encoded into environment variables *)
let env_var_sep = '^'

let encode_argv_to_env argv =
  String.concat ~sep:(String.make 1 env_var_sep)
    (IList.filter (fun arg ->
         not (String.contains arg env_var_sep)
         || (
           F.eprintf "Ignoring unsupported option containing '%c' character: %s@\n"
             env_var_sep arg ;
           false
         )
       ) argv)

let decode_env_to_argv env =
  Str.split (Str.regexp_string (String.make 1 env_var_sep)) env

let prepend_to_argv args =
  let cl_args = match Array.to_list Sys.argv with _ :: tl -> tl | [] -> [] in
  args @ cl_args

(** [prefix_before_rest (prefix @ ["--" :: rest])] is [prefix] where "--" is not in [prefix]. *)
let rev_prefix_before_rest args =
  let rec rev_prefix_before_rest_ rev_keep = function
    | [] | "--" :: _ -> rev_keep
    | keep :: args -> rev_prefix_before_rest_ (keep :: rev_keep) args in
  rev_prefix_before_rest_ [] args


(** environment variable use to pass arguments from parent to child processes *)
let args_env_var = "INFER_ARGS"

let extra_env_args = ref []

let extend_env_args args =
  extra_env_args := List.rev_append args !extra_env_args

let parse ?(incomplete=false) ?(accept_unknown=false) ?config_file current_exe exe_usage =
  let full_speclist = ref []
  in
  let usage_msg = exe_usage current_exe
  in
  let curr_usage status =
    prerr_endline (String.concat_array ~sep:" " !args_to_parse) ;
    Arg.usage !curr_speclist usage_msg ;
    exit status
  and full_usage status =
    Arg.usage !full_speclist usage_msg ;
    exit status
  in
  (* "-help" and "--help" are automatically recognized by Arg.parse, so we have to give them special
     treatment *)
  let add_or_suppress_help (speclist, (doc_width,left_width)) =
    let unknown opt =
      (opt, Arg.Unit (fun () -> raise (Arg.Bad ("unknown option '" ^ opt ^ "'"))), "") in
    let mk_spec ~long ?(short="") spec doc =
      pad_and_xform doc_width left_width { long; short; meta=""; spec; doc;
                                           decode_json=fun _ -> raise (Arg.Bad long)} in
    if incomplete then
      speclist @ [
        (unknown "--help") ;
        (unknown "-help")
      ]
    else
      speclist @ [
        mk_spec ~long:"help" ~short:"h"
          (Arg.Unit (fun () -> curr_usage 0))
          "Display this list of options";
        mk_spec ~long:"help-full"
          (Arg.Unit (fun () -> full_usage 0))
          "Display the full list of options, including internal and experimental options";
        (unknown "-help")
      ]
  in
  let normalize speclist =
    let norm k =
      let remove_no s =
        let len = String.length k in
        if len > 3 && String.sub s ~pos:0 ~len:3 = "no-"
        then String.sub s ~pos:3 ~len:(len - 3)
        else s in
      let remove_weird_chars = Str.global_replace (Str.regexp "[^a-z0-9-]") "" in
      remove_weird_chars @@ String.lowercase @@ remove_no k in
    let compare_specs {long = x} {long = y} =
      match x, y with
      | "--", "--" -> 0
      | "--", _ -> 1
      | _, "--" -> -1
      | _ ->
          let lower_norm s = String.lowercase @@ norm s in
          String.compare (lower_norm x) (lower_norm y) in
    let sort speclist = IList.sort compare_specs speclist in
    align (sort speclist)
  in
  let add_to_curr_speclist ?(add_help=false) ?header exe =
    let mk_header_spec heading =
      ("", Arg.Unit (fun () -> ()), "\n  " ^ heading ^ "\n") in
    let exe_descs = IList.assoc ( = ) exe exe_desc_lists in
    let (exe_speclist, widths) = normalize !exe_descs in
    let exe_speclist = if add_help
      then add_or_suppress_help (exe_speclist, widths)
      else exe_speclist in
    (* Return false if the same option appears in [speclist], unless [doc] is non-empty and the
       documentation in [speclist] is empty. The goal is to keep only one instance of each option,
       and that instance is the one that has a non-empty docstring if there is one. *)
    let is_not_dup_with_doc speclist (opt, _, doc) =
      opt = "" ||
      IList.for_all (fun (opt', _, doc') ->
          (doc <> "" && doc' = "") || (not (String.equal opt opt'))) speclist in
    let unique_exe_speclist = IList.filter (is_not_dup_with_doc !curr_speclist) exe_speclist in
    curr_speclist := IList.filter (is_not_dup_with_doc unique_exe_speclist) !curr_speclist @
                     (match header with
                      | Some s -> mk_header_spec s:: unique_exe_speclist
                      | None -> unique_exe_speclist)
  in
  (* speclist includes args for current exe with docs, and all other args without docs, so
     that all args can be parsed, but --help and parse failures only show external args for
     current exe *)
  (* reset the speclist between calls to this function *)
  curr_speclist := [];
  if current_exe = Toplevel then
    add_to_curr_speclist ~add_help:true ~header:"Toplevel options" current_exe
  else
    add_to_curr_speclist ~add_help:true current_exe
  ;
  if current_exe = Toplevel then (
    add_to_curr_speclist ~header:"Analysis (backend) options" Analyze;
    add_to_curr_speclist ~header:"Clang frontend options" Clang;
    add_to_curr_speclist ~header:"Java frontend options" Java;
  )
  ;
  assert( check_no_duplicates !curr_speclist )
  ;
  full_speclist := add_or_suppress_help (normalize !full_desc_list)
  ;
  let env_args = decode_env_to_argv (Option.value (Sys.getenv args_env_var) ~default:"") in
  (* begin transitional support for INFERCLANG_ARGS *)
  let c_args =
    Str.split (Str.regexp_string (String.make 1 ':'))
      (Option.value (Sys.getenv "INFERCLANG_ARGS") ~default:"") in
  let env_args = c_args @ env_args in
  (* end transitional support for INFERCLANG_ARGS *)
  let exe_name = Sys.executable_name in
  let should_parse_cl_args = match current_exe with
    | Clang | Interactive -> false
    | Analyze | Java | Print | Toplevel -> true in
  let env_cl_args =
    if should_parse_cl_args then prepend_to_argv env_args
    else env_args in
  let all_args = match config_file with
    | None -> env_cl_args
    | Some path ->
        let json_args = decode_inferconfig_to_argv current_exe path in
        (* read .inferconfig first, as both env vars and command-line options overwrite it *)
        json_args @ env_cl_args in
  args_to_parse := Array.of_list (exe_name :: all_args);
  arg_being_parsed := 0;
  (* tests if msg indicates an unknown option, as opposed to a known option with bad argument *)
  let is_unknown msg = String.is_substring msg ~substring:": unknown option" in
  accept_unknown_args := accept_unknown ;
  let rec parse_loop () =
    try
      Arg.parse_argv_dynamic ~current:arg_being_parsed !args_to_parse curr_speclist
        (fun arg -> !anon_fun arg) usage_msg
    with
    | Arg.Bad _ when incomplete -> parse_loop ()
    | Arg.Bad msg when !accept_unknown_args && is_unknown msg ->
        !anon_fun !args_to_parse.(!arg_being_parsed);
        parse_loop ()
    | Arg.Bad usage_msg -> Pervasives.prerr_string usage_msg; exit 2
    | Arg.Help usage_msg -> Pervasives.print_string usage_msg; exit 0
  in
  parse_loop ();
  if not incomplete then (
    (* reread args_to_parse instead of using all_args since mk_path_helper may have modified them *)
    let prog_args =
      List.rev_append
        (rev_prefix_before_rest (Array.to_list !args_to_parse))
        (List.rev !extra_env_args) in
    (* do not include program path in args passed via env var *)
    let args = Option.value (List.tl prog_args) ~default:[] in
    Unix.putenv ~key:args_env_var ~data:(encode_argv_to_env args)
  );
  curr_usage
