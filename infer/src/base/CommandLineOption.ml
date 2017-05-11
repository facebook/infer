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

let (=) = String.equal

let manpage_s_notes = "NOTES"

let is_env_var_set v =
  Option.value (Option.map (Sys.getenv v) ~f:((=) "1")) ~default:false

(** The working directory of the initial invocation of infer, to which paths passed as command line
    options are relative. *)
let init_work_dir, is_originator =
  match Sys.getenv "INFER_CWD" with
  | Some dir ->
      (dir, false)
  | None ->
      let real_cwd = Utils.realpath (Sys.getcwd ()) in
      Unix.putenv ~key:"INFER_CWD" ~data:real_cwd;
      (real_cwd, true)

let strict_mode_env_var = "INFER_STRICT_MODE"

let strict_mode = is_env_var_set strict_mode_env_var

let warnf =
  if strict_mode then failwithf
  else if not is_originator then fun fmt -> F.ifprintf F.err_formatter fmt
  else F.eprintf

(** This is the subset of Arg.spec that we actually use. What's important is that all these specs
    call back functions. We use this to mark deprecated arguments. What's not important is that, eg,
    Arg.Float is missing. *)
type spec =
  | Unit of (unit -> unit)
  | String of (string -> unit)
  | Symbol of string list * (string -> unit)
  | Rest of (string -> unit)

let to_arg_spec = function
  | Unit f -> Arg.Unit f
  | String f -> Arg.String f
  | Symbol (symbols, f) -> Arg.Symbol (symbols, f)
  | Rest f -> Arg.Rest f

let to_arg_spec_triple (x, spec, y) = (x, to_arg_spec spec, y)
let to_arg_speclist = List.map ~f:to_arg_spec_triple

(* NOTE: All variants must be also added to `all_parse_modes` below *)
type parse_mode = InferCommand | Javac | NoParse [@@deriving compare]
let equal_parse_mode = [%compare.equal : parse_mode]

let all_parse_modes = [InferCommand; Javac; NoParse]

let accept_unknown_args = function
  | Javac | NoParse -> true
  | InferCommand -> false

(* NOTE: All variants must be also added to `all_commands` below *)
type command =
  | Analyze | Capture | Clang | Compile | Report | ReportDiff | Run
[@@deriving compare]

let equal_command = [%compare.equal : command]

let all_commands = [
  Analyze; Capture; Clang; Compile; Report; ReportDiff; Run
]

type command_doc = {
  title : Cmdliner.Manpage.title;
  manual_pre_options : Cmdliner.Manpage.block list;
  manual_options : Cmdliner.Manpage.block list option;
  manual_post_options : Cmdliner.Manpage.block list;
}

type desc = {
  long: string; short: string; meta: string; doc: string; spec: spec;
  (** how to go from an option in the json config file to a list of command-line options *)
  decode_json: inferconfig_dir:string -> Yojson.Basic.json -> string list ;
}

let dashdash long =
  match long with
  | "" | "--" -> long
  | _ -> "--" ^ long

let xdesc {long; short; spec} =
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
    | Symbol (symbols, action) ->
        String (fun arg ->
            if List.mem ~equal:String.equal symbols arg then
              action arg
            else
              raise (Arg.Bad (F.sprintf "wrong argument '%s'; option '%s' expects one of: %s"
                                arg (dashdash long) (String.concat ~sep:" | " symbols)))
          )
    | _ ->
        spec
  in
  (* Arg doesn't need to know anything about documentation since we generate our own *)
  (key long short, xspec long spec, "")

let check_no_duplicates desc_list =
  let rec check_for_duplicates_ = function
    | [] | [_] ->
        true
    | (x, _, _) :: (y, _, _) :: _ when x <> "" && x = y ->
        failwith ("Multiple definitions of command line option: " ^ x)
    | _ :: tl ->
        check_for_duplicates_ tl
  in
  check_for_duplicates_ (List.sort ~cmp:(fun (x, _, _) (y, _, _) -> String.compare x y) desc_list)


let parse_mode_desc_lists = List.map ~f:(fun parse_mode -> (parse_mode, ref [])) all_parse_modes

module SectionMap = Caml.Map.Make (struct
    type t = String.t
    (* this must be the reverse of the order in which we want the sections to appear in the
       manual *)
    let compare s1 s2 =
      if String.equal s1 s2 then
        (* this simplifies the next two cases *)
        0
      else if String.equal s1 Cmdliner.Manpage.s_options then
        (* ensure OPTIONS section is last (hence first in the manual) *)
        1
      else if String.equal s2 Cmdliner.Manpage.s_options then
        (* same as above *)
        -1
      else
        (* reverse order *)
        String.compare s2 s1
  end)

let help_sections_desc_lists =
  List.map all_commands ~f:(fun command -> (command, ref SectionMap.empty))
let hidden_descs_list = ref []

(** add [desc] to the one relevant parse_tag_desc_lists for the purposes of parsing, and, in the
    case of InferCommand, include [desc] in --help only for the relevant sections. *)
let add parse_mode sections desc =
  let desc_list = List.Assoc.find_exn parse_mode_desc_lists parse_mode in
  desc_list := desc :: !desc_list;
  let add_to_section (command, section) =
    let sections = List.Assoc.find_exn ~equal:equal_command help_sections_desc_lists command in
    let prev_contents =
      try SectionMap.find section !sections
      with Not_found -> [] in
    sections := SectionMap.add section (desc::prev_contents) !sections in
  List.iter sections ~f:add_to_section;
  if List.is_empty sections then
    hidden_descs_list := desc :: !hidden_descs_list;
  ()

let deprecate_desc parse_mode ~long ~short ~deprecated desc =
  let warn () = match parse_mode with
    | Javac | NoParse -> ()
    | InferCommand ->
        warnf "WARNING: '-%s' is deprecated. Use '--%s'%s instead.@."
          deprecated long (if short = "" then "" else Printf.sprintf " or '-%s'" short) in
  let warn_then_f f x = warn (); f x in
  let deprecated_spec = match desc.spec with
    | Unit f -> Unit (warn_then_f f)
    | String f -> String (warn_then_f f)
    | Symbol (symbols, f) -> Symbol (symbols, warn_then_f f)
    | Rest _ as spec -> spec in
  let deprecated_decode_json ~inferconfig_dir j =
    warnf "WARNING: in .inferconfig: '%s' is deprecated. Use '%s' instead.@." deprecated long;
    desc.decode_json ~inferconfig_dir j in
  { long = ""; short = deprecated; meta = ""; doc = "";
    spec = deprecated_spec; decode_json = deprecated_decode_json }

let mk ?(deprecated=[]) ?(parse_mode=InferCommand) ?(in_help=[])
    ~long ?short:short0 ~default ~meta doc ~default_to_string ~decode_json ~mk_setter ~mk_spec =
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
    else
      let doc_default_sep = if String.is_suffix ~suffix:"\n" doc then "" else " " in
      doc ^ doc_default_sep ^ "(default: $(i," ^ Cmdliner.Manpage.escape default_string ^ "))" in
  let short = match short0 with Some c -> String.of_char c | None -> "" in
  let desc = {long; short=short; meta; doc; spec; decode_json} in
  (* add desc for long option, with documentation (which includes any short option) for exes *)
  if long <> "" then add parse_mode in_help desc ;
  (* add desc for short option only for parsing, without documentation *)
  if short <> "" then
    add parse_mode [] {desc with long = ""; meta = ""; doc = ""} ;
  (* add desc for deprecated options only for parsing, without documentation *)
  List.iter deprecated ~f:(fun deprecated ->
      deprecate_desc parse_mode ~long ~short:short ~deprecated desc
      |> add parse_mode []) ;
  variable

(* begin parsing state *)

(* arguments passed to Arg.parse_argv_dynamic, susceptible to be modified on the fly when parsing *)
let args_to_parse : string array ref = ref (Array.of_list [])

(* reference used by Arg.parse_argv_dynamic to track the index of the argument being parsed *)
let arg_being_parsed : int ref = ref 0

(* list of arg specifications currently being used by Arg.parse_argv_dynamic *)
let curr_speclist : (Arg.key * Arg.spec * Arg.doc) list ref = ref []

let unknown_args_action = ref `ParseCommands

let subcommands = ref []
let subcommand_actions = ref []

let rev_anon_args = ref []

(* keep track of the current active command to drive the remainder of the program *)
let curr_command = ref None

(* end parsing state *)

type 'a t =
  ?deprecated:string list -> long:Arg.key -> ?short:char ->
  ?parse_mode:parse_mode -> ?in_help:(command * string) list -> ?meta:string -> Arg.doc ->
  'a

let string_json_decoder ~long ~inferconfig_dir:_ json =
  [dashdash long; YBU.to_string json]

let path_json_decoder ~long ~inferconfig_dir json =
  let abs_path =
    let path = YBU.to_string json in
    if Filename.is_relative path then inferconfig_dir ^/ path
    else path in
  [dashdash long; abs_path]

let list_json_decoder json_decoder ~inferconfig_dir json =
  List.concat (YBU.convert_each (json_decoder ~inferconfig_dir) json)

let mk_set var value ?(deprecated=[]) ~long ?short ?parse_mode ?in_help ?(meta="") doc =
  let setter () = var := value in
  ignore(
    mk ~deprecated ~long ?short ~default:() ?parse_mode ?in_help ~meta doc
      ~default_to_string:(fun () -> "")
      ~decode_json:(string_json_decoder ~long)
      ~mk_setter:(fun _ _ -> setter ())
      ~mk_spec:(fun _ -> Unit setter) )

let mk_option ?(default=None) ?(default_to_string=fun _ -> "") ~f
    ?(deprecated=[]) ~long ?short ?parse_mode ?in_help ?(meta="string") doc =
  mk ~deprecated ~long ?short ~default ?parse_mode ?in_help ~meta doc
    ~default_to_string
    ~decode_json:(string_json_decoder ~long)
    ~mk_setter:(fun var str -> var := f str)
    ~mk_spec:(fun set -> String set)

let mk_bool ?(deprecated_no=[]) ?(default=false) ?(f=fun b -> b)
    ?(deprecated=[]) ~long ?short ?parse_mode ?in_help ?(meta="") doc =
  let nolong =
    let len = String.length long in
    if len > 3 && String.sub long ~pos:0 ~len:3 = "no-" then
      String.sub long ~pos:3 ~len:(len - 3)
    else
      "no-" ^ long
  and noshort =
    Option.map ~f:(fun short ->
        if Char.is_lowercase short then Char.uppercase short
        else Char.lowercase short
      ) short
  in
  let doc long short =
    match short with
    | Some short -> doc ^ " (Conversely: $(b,--" ^ long ^ ") | $(b,-" ^ String.of_char short ^ "))"
    | None       -> doc ^ " (Conversely: $(b,--" ^ long ^ "))"
  in
  let doc, nodoc =
    if not default then
      ("Activates: " ^ doc nolong noshort, "")
    else
      ("", "Deactivates: " ^ doc long short) in
  let default_to_string _ = "" in
  let mk_spec set = Unit (fun () -> set "") in
  let var =
    mk ~long ?short ~deprecated ~default ?parse_mode ?in_help
      ~meta doc ~default_to_string ~mk_setter:(fun var _ -> var := f true)
      ~decode_json:(fun ~inferconfig_dir:_ json ->
          [dashdash (if YBU.to_bool json then long else nolong)])
      ~mk_spec in
  ignore(
    mk ~long:nolong ?short:noshort ~deprecated:deprecated_no ~default:(not default)
      ?parse_mode ?in_help
      ~meta nodoc ~default_to_string ~mk_setter:(fun _ _ -> var := f false)
      ~decode_json:(fun ~inferconfig_dir:_ json ->
          [dashdash (if YBU.to_bool json then nolong else long)])
      ~mk_spec );
  var

let mk_bool_group ?(deprecated_no=[]) ?(default=false)
    ?(deprecated=[]) ~long ?short ?parse_mode ?in_help ?meta doc children no_children =
  let f b =
    List.iter ~f:(fun child -> child := b) children ;
    List.iter ~f:(fun child -> child := not b) no_children ;
    b
  in
  mk_bool ~deprecated ~deprecated_no ~default ~long ?short ~f ?parse_mode ?in_help ?meta doc

let mk_int ~default ?(deprecated=[]) ~long ?short ?parse_mode ?in_help ?(meta="int") doc =
  mk ~deprecated ~long ?short ~default ?parse_mode ?in_help ~meta doc
    ~default_to_string:string_of_int
    ~mk_setter:(fun var str -> var := (int_of_string str))
    ~decode_json:(string_json_decoder ~long)
    ~mk_spec:(fun set -> String set)

let mk_int_opt ?default ?(deprecated=[]) ~long ?short ?parse_mode ?in_help ?(meta="int") doc =
  let default_to_string = function Some f -> string_of_int f | None -> "" in
  let f s = Some (int_of_string s) in
  mk_option ~deprecated ~long ?short ~default ~default_to_string ~f ?parse_mode ?in_help ~meta doc

let mk_float ~default ?(deprecated=[]) ~long ?short ?parse_mode ?in_help ?(meta="float") doc =
  mk ~deprecated ~long ?short ~default ?parse_mode ?in_help ~meta doc
    ~default_to_string:string_of_float
    ~mk_setter:(fun var str -> var := (float_of_string str))
    ~decode_json:(string_json_decoder ~long)
    ~mk_spec:(fun set -> String set)

let mk_float_opt ?default ?(deprecated=[]) ~long ?short ?parse_mode ?in_help ?(meta="float") doc =
  let default_to_string = function Some f -> string_of_float f | None -> "" in
  let f s = Some (float_of_string s) in
  mk_option ~deprecated ~long ?short ~default ~default_to_string ~f ?parse_mode ?in_help ~meta doc

let mk_string ~default ?(f=fun s -> s) ?(deprecated=[]) ~long ?short ?parse_mode ?in_help
    ?(meta="string") doc =
  mk ~deprecated ~long ?short ~default ?parse_mode ?in_help ~meta doc
    ~default_to_string:(fun s -> s)
    ~mk_setter:(fun var str -> var := f str)
    ~decode_json:(string_json_decoder ~long)
    ~mk_spec:(fun set -> String set)

let mk_string_opt ?default ?(f=fun s -> s) ?(deprecated=[]) ~long ?short ?parse_mode ?in_help
    ?(meta="string") doc =
  let default_to_string = function Some s -> s | None -> "" in
  let f s = Some (f s) in
  mk_option ~deprecated ~long ?short ~default ~default_to_string ~f ?parse_mode ?in_help ~meta doc

let mk_string_list ?(default=[]) ?(f=fun s -> s)
    ?(deprecated=[]) ~long ?short ?parse_mode ?in_help ?(meta="+string") doc =
  mk ~deprecated ~long ?short ~default ?parse_mode ?in_help ~meta doc
    ~default_to_string:(String.concat ~sep:", ")
    ~mk_setter:(fun var str -> var := (f str) :: !var)
    ~decode_json:(list_json_decoder (string_json_decoder ~long))
    ~mk_spec:(fun set -> String set)

let mk_path_helper ~setter ~default_to_string
    ~default ~deprecated ~long ~short ~parse_mode ~in_help ~meta ~decode_json doc =
  let normalize_path_in_args_being_parsed str =
    if Filename.is_relative str then (
      (* Replace relative paths with absolute ones on the fly in the args being parsed. This assumes
         that [!arg_being_parsed] points at the option name position in [!args_to_parse], as is the
         case e.g. when calling
         [Arg.parse_argv_dynamic ~current:arg_being_parsed !args_to_parse ...]. *)
      let root = Unix.getcwd () in
      let abs_path = Utils.filename_to_absolute ~root str in
      (!args_to_parse).(!arg_being_parsed + 1) <- abs_path;
      abs_path
    ) else
      str in
  mk ~deprecated ~long ?short ~default ?parse_mode ?in_help ~meta doc
    ~decode_json ~default_to_string
    ~mk_setter:(fun var str ->
        let abs_path = normalize_path_in_args_being_parsed str in
        setter var abs_path)
    ~mk_spec:(fun set -> String set)

let mk_path ~default ?(deprecated=[]) ~long ?short ?parse_mode ?in_help ?(meta="path") =
  mk_path_helper
    ~setter:(fun var x -> var := x)
    ~decode_json:(path_json_decoder ~long)
    ~default_to_string:(fun s -> s)
    ~default ~deprecated ~long ~short ~parse_mode ~in_help ~meta

let mk_path_opt ?default ?(deprecated=[]) ~long ?short ?parse_mode ?in_help ?(meta="path") =
  mk_path_helper
    ~setter:(fun var x -> var := Some x)
    ~decode_json:(path_json_decoder ~long)
    ~default_to_string:(function Some s -> s | None -> "")
    ~default ~deprecated ~long ~short ~parse_mode ~in_help ~meta

let mk_path_list ?(default=[]) ?(deprecated=[]) ~long ?short ?parse_mode ?in_help ?(meta="+path") =
  mk_path_helper
    ~setter:(fun var x -> var := x :: !var)
    ~decode_json:(list_json_decoder (path_json_decoder ~long))
    ~default_to_string:(String.concat ~sep:", ")
    ~default ~deprecated ~long ~short ~parse_mode ~in_help ~meta

let mk_symbols_meta symbols =
  let strings = List.map ~f:fst symbols in
  Printf.sprintf "{ %s }" (String.concat ~sep:" | " strings)

let mk_symbol ~default ~symbols ~eq ?(deprecated=[]) ~long ?short ?parse_mode ?in_help ?meta doc =
  let strings = List.map ~f:fst symbols in
  let sym_to_str = List.map ~f:(fun (x,y) -> (y,x)) symbols in
  let of_string str = List.Assoc.find_exn ~equal:String.equal symbols str in
  let to_string sym = List.Assoc.find_exn ~equal:eq sym_to_str sym in
  let meta = Option.value meta ~default:(mk_symbols_meta symbols) in
  mk ~deprecated ~long ?short ~default ?parse_mode ?in_help ~meta doc
    ~default_to_string:(fun s -> to_string s)
    ~mk_setter:(fun var str -> var := of_string str)
    ~decode_json:(string_json_decoder ~long)
    ~mk_spec:(fun set -> Symbol (strings, set))

let mk_symbol_opt ~symbols ?(deprecated=[]) ~long ?short ?parse_mode ?in_help ?meta doc =
  let strings = List.map ~f:fst symbols in
  let of_string str = List.Assoc.find_exn ~equal:String.equal symbols str in
  let meta = Option.value meta ~default:(mk_symbols_meta symbols) in
  mk ~deprecated ~long ?short ~default:None ?parse_mode ?in_help ~meta doc
    ~default_to_string:(fun _ -> "")
    ~mk_setter:(fun var str -> var := Some (of_string str))
    ~decode_json:(string_json_decoder ~long)
    ~mk_spec:(fun set -> Symbol (strings, set))

let mk_symbol_seq ?(default=[]) ~symbols ~eq ?(deprecated=[]) ~long ?short ?parse_mode ?in_help
    ?meta doc =
  let sym_to_str = List.map ~f:(fun (x,y) -> (y,x)) symbols in
  let of_string str = List.Assoc.find_exn ~equal:String.equal symbols str in
  let to_string sym = List.Assoc.find_exn ~equal:eq sym_to_str sym in
  let meta = Option.value meta ~default:(",-separated sequence of " ^ mk_symbols_meta symbols) in
  mk ~deprecated ~long ?short ~default ?parse_mode ?in_help
    ~meta doc
    ~default_to_string:(fun syms -> String.concat ~sep:" " (List.map ~f:to_string syms))
    ~mk_setter:(fun var str_seq ->
        var := List.map ~f:of_string (String.split ~on:',' str_seq))
    ~decode_json:(fun ~inferconfig_dir:_ json ->
        [dashdash long;
         String.concat ~sep:"," (YBU.convert_each YBU.to_string json)])
    ~mk_spec:(fun set -> String set)

let mk_set_from_json ~default ~default_to_string ~f
    ?(deprecated=[]) ~long ?short ?parse_mode ?in_help ?(meta="json") doc =
  mk ~deprecated ~long ?short ?parse_mode ?in_help ~meta doc
    ~default ~default_to_string
    ~mk_setter:(fun var json -> var := f (Yojson.Basic.from_string json))
    ~decode_json:(fun ~inferconfig_dir:_ json -> [dashdash long; Yojson.Basic.to_string json])
    ~mk_spec:(fun set -> String set)

let mk_json ?(deprecated=[]) ~long ?short ?parse_mode ?in_help ?(meta="json") doc =
  mk ~deprecated ~long ?short ?parse_mode ?in_help ~meta doc
    ~default:(`List []) ~default_to_string:Yojson.Basic.to_string
    ~mk_setter:(fun var json -> var := Yojson.Basic.from_string json)
    ~decode_json:(fun ~inferconfig_dir:_ json -> [dashdash long; Yojson.Basic.to_string json])
    ~mk_spec:(fun set -> String set)

(** [mk_anon] always return the same ref. Anonymous arguments are only accepted if
    [parse_action_accept_unknown_args] is true. *)
let mk_anon () = rev_anon_args

let mk_rest ?(parse_mode=InferCommand) ?(in_help=[]) doc =
  let rest = ref [] in
  let spec = Rest (fun arg -> rest := arg :: !rest) in
  add parse_mode in_help {long = "--"; short = ""; meta = ""; doc; spec;
                          decode_json = fun ~inferconfig_dir:_ _ -> []} ;
  rest

let normalize_desc_list speclist =
  let norm k =
    let remove_no s =
      let len = String.length k in
      if len > 3 && String.sub s ~pos:0 ~len:3 = "no-"
      then String.sub s ~pos:3 ~len:(len - 3)
      else s in
    let remove_weird_chars =
      String.filter ~f:(function
          | 'a'..'z' | '0'..'9' | '-' -> true
          | _ -> false) in
    remove_weird_chars @@ String.lowercase @@ remove_no k in
  let compare_specs {long = x} {long = y} =
    match x, y with
    | "--", "--" -> 0
    | "--", _ -> 1
    | _, "--" -> -1
    | _ ->
        let lower_norm s = String.lowercase @@ norm s in
        String.compare (lower_norm x) (lower_norm y) in
  let sort speclist = List.sort ~cmp:compare_specs speclist in
  sort speclist

let mk_command_doc ~title ~section ~version ~date ~short_description ~synopsis ~description
    ?options ?exit_status ?environment ?files ?notes ?bugs ?examples ~see_also
    command_str =
  let add_if section blocks = match blocks with
    | None -> `Blocks []
    | Some bs -> `Blocks (`S section :: bs) in
  let manual_pre_options = [
    `S Cmdliner.Manpage.s_name;
    (* the format of the following line is mandated by man(7) *)
    `Pre (Printf.sprintf "%s - %s" command_str short_description);
    `S Cmdliner.Manpage.s_synopsis;
    `Blocks synopsis;
    `S Cmdliner.Manpage.s_description;
    `Blocks description;
  ] in
  let manual_post_options = [
    add_if Cmdliner.Manpage.s_exit_status exit_status;
    add_if Cmdliner.Manpage.s_environment environment;
    add_if Cmdliner.Manpage.s_files files;
    add_if manpage_s_notes notes;
    add_if Cmdliner.Manpage.s_bugs bugs;
    add_if Cmdliner.Manpage.s_examples examples;
    `S Cmdliner.Manpage.s_see_also;
    `Blocks see_also;
  ] in
  let command_doc = {
    title = command_str, section, date, version, title;
    manual_pre_options; manual_options = options; manual_post_options;
  } in
  command_doc

let set_curr_speclist_for_parse_mode ~usage parse_mode =
  let curr_usage status =
    prerr_endline (String.concat_array ~sep:" " !args_to_parse) ;
    prerr_endline usage ;
    exit status
  in
  (* "-help" and "--help" are automatically recognized by Arg.parse, so we have to give them special
     treatment *)
  let add_or_suppress_help speclist =
    let unknown opt =
      (opt, Unit (fun () -> raise (Arg.Bad ("unknown option '" ^ opt ^ "'"))), "") in
    let has_opt opt = List.exists ~f:(fun (o, _, _) -> String.equal opt o) speclist in
    let add_unknown opt = if not (has_opt opt) then List.cons (unknown opt) else Fn.id in
    add_unknown "-help" @@ add_unknown "--help" @@ speclist
  in
  let full_desc_list =
    List.Assoc.find_exn ~equal:equal_parse_mode parse_mode_desc_lists parse_mode in
  curr_speclist := normalize_desc_list !full_desc_list
                   |> List.map ~f:xdesc
                   |> add_or_suppress_help
                   |> to_arg_speclist;
  assert( check_no_duplicates !curr_speclist );
  curr_usage


let select_parse_mode ~usage action =
  let usage = set_curr_speclist_for_parse_mode ~usage action in
  unknown_args_action := if accept_unknown_args action then `Add else `ParseCommands;
  usage

let string_of_command command =
  let (_, s, _) = List.Assoc.find_exn !subcommands ~equal:equal_command command in
  s

let anon_fun arg =
  match !unknown_args_action with
  | `ParseCommands -> (
      match !curr_command, List.Assoc.find !subcommand_actions ~equal:String.equal arg with
      | None, Some switch -> switch ()
      | Some command, Some _ ->
          raise (Arg.Bad
                   ("More than one subcommand specified: " ^ string_of_command command ^ ", " ^
                    arg))
      | _, None ->
          raise (Arg.Bad ("unexpected anonymous argument: " ^ arg))
    )
  | `Skip ->
      ()
  | `Add ->
      rev_anon_args := arg::!rev_anon_args

let mk_rest_actions ?(parse_mode=InferCommand) ?(in_help=[]) doc ~usage decode_action =
  let rest = ref [] in
  let spec = String (fun arg ->
      rest := List.rev (Array.to_list (Array.slice !args_to_parse (!arg_being_parsed + 1) 0)) ;
      select_parse_mode ~usage (decode_action arg) |> ignore;
      (* stop accepting new anonymous arguments *)
      unknown_args_action := `Skip) in
  add parse_mode in_help {long = "--"; short = ""; meta = ""; doc; spec;
                          decode_json = fun ~inferconfig_dir:_ _ -> []} ;
  rest

let mk_subcommand command ?(accept_unknown_args=false) ?deprecated ~long ?(name=long)
    ?parse_mode ?in_help command_doc =
  let switch () =
    curr_command := Some command;
    unknown_args_action := if accept_unknown_args then `Add else `ParseCommands in
  ignore(
    mk ?deprecated ~long ~default:() ?parse_mode ?in_help ~meta:""
      (Printf.sprintf "activates the %s subcommand (see $(i,`infer %s --help`))" long long)
      ~default_to_string:(fun () -> "")
      ~decode_json:(string_json_decoder ~long)
      ~mk_setter:(fun _ _ -> switch ())
      ~mk_spec:(fun _ -> Unit switch));
  subcommands := (command, (command_doc, name, in_help))::!subcommands;
  subcommand_actions := (name, switch)::!subcommand_actions

let decode_inferconfig_to_argv path =
  let json = match Utils.read_json_file path with
    | Ok json ->
        json
    | Error msg ->
        warnf "WARNING: Could not read or parse Infer config in %s:@\n%s@." path msg ;
        `Assoc [] in
  let desc_list = List.Assoc.find_exn ~equal:equal_parse_mode parse_mode_desc_lists InferCommand in
  let json_config = YBU.to_assoc json in
  let inferconfig_dir = Filename.dirname path in
  let one_config_item result (key, json_val) =
    try
      let {decode_json} =
        List.find_exn
          ~f:(fun {long; short} ->
              String.equal key long
              || (* for deprecated options *) String.equal key short)
          !desc_list in
      decode_json ~inferconfig_dir json_val @ result
    with
    | Not_found ->
        warnf "WARNING: while reading config file %s:@\nUnknown option %s@." path key ;
        result
    | YBU.Type_error (msg, json) ->
        warnf "WARNING: while reading config file %s:@\nIll-formed value %s for option %s: %s@."
          path (Yojson.Basic.to_string json) key msg ;
        result in
  List.fold ~f:one_config_item ~init:[] json_config


(** separator of argv elements when encoded into environment variables *)
let env_var_sep = '^'

let encode_argv_to_env argv =
  String.concat ~sep:(String.make 1 env_var_sep)
    (List.filter ~f:(fun arg ->
         not (String.contains arg env_var_sep)
         || (
           warnf "Ignoring unsupported option containing '%c' character: %s@\n"
             env_var_sep arg ;
           false
         )
       ) argv)

let decode_env_to_argv env =
  String.split ~on:env_var_sep env |> List.filter ~f:(Fn.non String.is_empty)

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

(* TODO(t18057447) [should_expand_args] is a bogus hack to side-step a bug with expansion of
   @argfiles *)
let parse_args ~usage initial_action ?(should_expand_args=true) ?initial_command args0 =
  (* look inside argfiles so we can move select arguments into the top line CLI and parse them into
     Config vars. note that we don't actually delete the arguments to the file, we just duplicate
     them on the CLI. javac is ok with this.  *)
  let expand_argfiles acc arg =
    if String.is_prefix arg ~prefix:"@"
    then
      (* for now, we only need to parse -d. we could parse more if we wanted to, but we would risk
         incurring the wrath of ARGUMENT_LIST_TOO_LONG *)
      let should_parse = function
        | "-d" | "-cp" | "-classpath" -> true
        | _ -> false in
      let fname = String.slice arg 1 (String.length arg) in
      match In_channel.read_lines fname with
      | lines ->
          (* crude but we only care about simple cases that will not involve trickiness, eg
             unbalanced or escaped quotes such as "ending in\"" *)
          let strip =
            String.strip ~drop:(function '"' | '\'' -> true | _ -> false) in
          let rec parse_argfile_args acc = function
            | flag :: ((value :: args) as rest) ->
                let stripped_flag = strip flag in
                if should_parse stripped_flag
                then parse_argfile_args (stripped_flag :: strip value :: acc) args
                else parse_argfile_args acc rest
            | _ ->
                acc in
          parse_argfile_args (arg :: acc) lines
      | exception _ ->
          acc
    else
      arg :: acc in
  let args = if should_expand_args then
      List.fold ~f:expand_argfiles ~init:[] (List.rev args0)
    else
      args0 in

  let exe_name = Sys.executable_name in
  args_to_parse := Array.of_list (exe_name :: args);
  arg_being_parsed := 0;
  let curr_usage = select_parse_mode ~usage initial_action in
  Option.iter initial_command ~f:(fun command ->
      let switch = List.Assoc.find_exn !subcommand_actions ~equal:String.equal
          (string_of_command command) in
      switch ());
  (* tests if msg indicates an unknown option, as opposed to a known option with bad argument *)
  let is_unknown msg = String.is_substring msg ~substring:": unknown option" in
  let rec parse_loop () =
    try
      Arg.parse_argv_dynamic ~current:arg_being_parsed !args_to_parse curr_speclist
        anon_fun usage
    with
    | Arg.Bad usage_msg ->
        if !unknown_args_action <> `ParseCommands && is_unknown usage_msg then (
          anon_fun !args_to_parse.(!arg_being_parsed);
          parse_loop ()
        ) else (
          Pervasives.prerr_string usage_msg;
          exit 2
        )
    | Arg.Help _ ->
        (* we handle --help by ourselves and error on -help, so Arg has no way to raise Help
           anymore *)
        assert false
  in
  parse_loop ();
  curr_usage

let parse ?config_file ~usage action initial_command =
  let env_args = decode_env_to_argv (Option.value (Sys.getenv args_env_var) ~default:"") in
  let inferconfig_args =
    Option.map ~f:decode_inferconfig_to_argv config_file |> Option.value ~default:[] in
  let args_to_export = ref "" in
  let add_parsed_args_to_args_to_export () =
    (* reread args_to_parse instead of using all_args since mk_path_helper may have modified them *)
    let prog_args =
      List.rev_append
        (rev_prefix_before_rest (Array.to_list !args_to_parse))
        (List.rev !extra_env_args) in
    (* do not include program path in args passed via env var *)
    let args = Option.value (List.tl prog_args) ~default:[] in
    if not (List.is_empty args) then
      let arg_string =
        if String.equal !args_to_export "" then encode_argv_to_env args
        else !args_to_export ^ String.of_char env_var_sep ^ encode_argv_to_env args in
      args_to_export := arg_string in
  (* read .inferconfig first, then env vars, then command-line options *)
  (* TODO(t18057447) [should_expand_args] is a bogus hack to side-step a bug with expansion of
     @argfiles *)
  parse_args ~usage ~should_expand_args:false InferCommand inferconfig_args |> ignore;
  (* NOTE: do not add the contents of .inferconfig to INFER_ARGS. This helps avoid hitting the
     command line size limit. *)
  (* TODO(t18057447) [should_expand_args] is a bogus hack to side-step a bug with expansion of
     @argfiles *)
  parse_args ~usage ~should_expand_args:false InferCommand env_args |> ignore;
  add_parsed_args_to_args_to_export ();
  let curr_usage =
    let cl_args = match Array.to_list Sys.argv with _ :: tl -> tl | [] -> [] in
    let curr_usage = parse_args ~usage action ?initial_command cl_args in
    add_parsed_args_to_args_to_export ();
    curr_usage in
  Unix.putenv ~key:args_env_var ~data:!args_to_export;
  !curr_command, curr_usage

let wrap_line indent_string wrap_length line0 =
  let line = indent_string ^ line0 in
  let indent_length = String.length indent_string in
  let word_sep = ' ' in
  let words = String.split ~on:word_sep line in
  let word_sep_str = String.of_char word_sep in
  let add_word_to_paragraph (rev_lines, non_empty, line, line_length) word =
    let word_length =
      let len = String.length word in
      if String.is_prefix ~prefix:"$(b," word || String.is_prefix ~prefix:"$(i," word then
        len - 4 (* length of formatting tag prefix *)
        - 1 (* APPROXIMATION: closing parenthesis that will come after the word, or maybe later *)
      else
        len in
    let new_length = line_length + (String.length word_sep_str) + word_length in
    let new_non_empty = non_empty || word <> "" in
    if new_length > wrap_length && non_empty then
      (line::rev_lines, true, indent_string ^ word, indent_length + word_length)
    else
      let sep = if Int.equal line_length indent_length then "" else word_sep_str in
      let new_line = line ^ sep ^ word in
      if new_length > wrap_length && new_non_empty then
        (new_line::rev_lines, false, indent_string, indent_length)
      else
        (rev_lines, new_non_empty, new_line, new_length) in
  let (rev_lines, _, line, _) =
    List.fold ~f:add_word_to_paragraph ~init:([], false, "", 0) words in
  List.rev (line::rev_lines)

let show_manual ?internal_section default_doc command_opt =
  let command_doc = match command_opt with
    | None ->
        default_doc
    | Some command ->
        let (command_doc, _, _) = List.Assoc.find_exn !subcommands command in
        command_doc in
  let pp_meta f meta = match meta with
    | "" -> ()
    | meta -> F.fprintf f " $(i,%s)" (Cmdliner.Manpage.escape meta) in
  let pp_short f = function
    | "" -> ()
    | s -> Format.fprintf f ",$(b,-%s)" s in
  let block_of_desc { long; meta; short; doc } =
    if String.equal doc "" then
      []
    else
      let doc_first_line, doc_other_lines = match String.split ~on:'\n' doc with
        | first::other -> first, other
        | [] -> "", [] in
      (* Cmdline.Manpage does not format multi-paragraph documentation strings correctly for `I
         blocks, so we do a bit of formatting by hand *)
      let indent_string = "    " in
      let width = 77 (* Cmdliner.Manpage width limit it seems *)
                  - 7 (* base indentation of documentation strings *) in
      `I (Format.asprintf "$(b,%s)%a%a" (dashdash long) pp_short short pp_meta meta,
          doc_first_line)
      :: List.concat_map (List.concat_map ~f:(wrap_line indent_string width) doc_other_lines)
        ~f:(fun s -> [`Noblank; `Pre s]) in
  let option_blocks = match command_doc.manual_options, command_opt with
    | None, None ->
        failwithf "Cannot create %s section" Cmdliner.Manpage.s_options
    | Some blocks, _ ->
        `S Cmdliner.Manpage.s_options :: blocks
    | None, Some command ->
        let sections = List.Assoc.find_exn help_sections_desc_lists command in
        let hidden =
          match internal_section with
          | Some section ->
              `S section :: `P "Use at your own risk."
              :: List.concat_map ~f:block_of_desc (normalize_desc_list !hidden_descs_list)
          | None ->
              [] in
        SectionMap.fold (fun section descs result ->
            `S section ::
            List.concat_map ~f:block_of_desc (normalize_desc_list descs) @ result)
          !sections hidden in
  let blocks = [`Blocks command_doc.manual_pre_options; `Blocks option_blocks;
                `Blocks command_doc.manual_post_options] in
  Cmdliner.Manpage.print `Auto Format.std_formatter (command_doc.title, blocks);
  ()
