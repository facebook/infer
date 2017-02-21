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

let strict_mode = is_env_var_set "INFER_STRICT_MODE"

let warnf =
  if strict_mode then failwithf
  else if not is_originator then fun fmt -> F.ifprintf F.err_formatter fmt
  else F.eprintf

type section =
    Analysis | BufferOverrun | Checkers | Clang | Crashcontext | Driver | Java | Print | Quandary
[@@deriving compare]

let equal_section = [%compare.equal : section ]
let all_sections =
  [ Analysis; BufferOverrun; Checkers; Clang; Crashcontext; Driver; Java; Print; Quandary ]

type 'a parse = Infer of 'a | Javac | NoParse [@@deriving compare]

type parse_mode = section list parse [@@deriving compare]

type parse_action = section parse [@@deriving compare]

let equal_parse_action = [%compare.equal : parse_action ]

type parse_tag = unit parse [@@deriving compare]

let equal_parse_tag = [%compare.equal : parse_tag ]
let all_parse_tags = [ Infer (); Javac; NoParse ]

let to_parse_tag = function | Infer _ -> Infer () | Javac -> Javac | NoParse -> NoParse

let accept_unknown_args = function
  | Infer Print | Javac | NoParse -> true
  | Infer (Analysis | BufferOverrun | Checkers | Clang | Crashcontext | Driver | Java | Quandary) ->
      false

type desc = {
  long: string; short: string; meta: string; doc: string; spec: spec;
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
      | Symbol (symbols, _) ->
          ["{ " ^ (String.concat ~sep:" | " symbols) ^ " }" ^ meta]
      | _ ->
          if meta = "" then [] else ["<" ^ meta ^ ">"]))

let left_length long short_meta =
  (String.length (dashdash long)) + (String.length short_meta)

let max_left_length limit current ({long; spec} as desc) =
  let short_meta =
    match spec with
    | Symbol _ -> short_meta {desc with spec = Unit (fun () -> ())}
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
      let sep = if Int.equal line_length indent_length then "" else word_sep in
      let new_line = line ^ sep ^ word in
      if new_length > wrap_length && new_non_empty then
        (new_line::rev_lines, false, indent_string, indent_length)
      else
        (rev_lines, new_non_empty, new_line, String.length new_line) in
  let (rev_lines, _, line, _) =
    List.fold ~f:add_word_to_paragraph ~init:([], false, "", 0) words in
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
      let doc = indent_doc (String.concat ~sep:"\n" (List.concat wrapped_lines)) in
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
    let opt_left_width =
      List.fold ~f:(max_left_length max_left_width) ~init:0 desc_list in
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


let parse_tag_desc_lists = List.map ~f:(fun parse_tag -> (parse_tag, ref [])) all_parse_tags

let infer_section_desc_lists = List.map ~f:(fun section -> (section, ref [])) all_sections

(** add [desc] to the one relevant parse_tag_desc_lists for the purposes of parsing, and, in the
    case of Infer, include [desc] in --help only for the relevant sections. *)
let add parse_mode desc =
  let tag = to_parse_tag parse_mode in
  let full_desc_list = List.Assoc.find_exn parse_tag_desc_lists tag in
  full_desc_list := desc :: !full_desc_list ;
  match parse_mode with
  | Javac | NoParse -> ()
  | Infer sections ->
      List.iter infer_section_desc_lists ~f:(fun (section, desc_list) ->
          let desc = if List.mem ~equal:equal_section sections section then
              desc
            else
              {desc with meta = ""; doc = ""} in
          desc_list := desc :: !desc_list)

let deprecate_desc parse_mode ~long ~short ~deprecated desc =
  let warn () = match parse_mode with
    | Javac | NoParse -> ()
    | Infer _ ->
        warnf "WARNING: '-%s' is deprecated. Use '--%s'%s instead.@."
          deprecated long (if short = "" then "" else Printf.sprintf " or '-%s'" short) in
  let warn_then_f f x = warn (); f x in
  let deprecated_spec = match desc.spec with
    | Unit f -> Unit (warn_then_f f)
    | String f -> String (warn_then_f f)
    | Symbol (symbols, f) -> Symbol (symbols, warn_then_f f)
    | Rest _ as spec -> spec in
  let deprecated_decode_json j =
    warnf "WARNING: in .inferconfig: '%s' is deprecated. Use '%s' instead.@." deprecated long;
    desc.decode_json j in
  { long = ""; short = deprecated; meta = ""; doc = "";
    spec = deprecated_spec; decode_json = deprecated_decode_json }

let mk ?(deprecated=[]) ?(parse_mode=Infer [])
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
  if long <> "" then add parse_mode desc ;
  (* add desc for short option only for parsing, without documentation *)
  let parse_mode_no_sections = match parse_mode with
    | Infer _ -> Infer []
    | Javac | NoParse -> parse_mode in
  if short <> "" then
    add parse_mode_no_sections {desc with long = ""; meta = ""; doc = ""} ;
  (* add desc for deprecated options only for parsing, without documentation *)
  List.iter deprecated ~f:(fun deprecated ->
      deprecate_desc parse_mode ~long ~short ~deprecated desc
      |> add parse_mode_no_sections) ;
  variable

(* begin parsing state *)

(* arguments passed to Arg.parse_argv_dynamic, susceptible to be modified on the fly when parsing *)
let args_to_parse : string array ref = ref (Array.of_list [])

(* reference used by Arg.parse_argv_dynamic to track the index of the argument being parsed *)
let arg_being_parsed : int ref = ref 0

(* list of arg specifications currently being used by Arg.parse_argv_dynamic *)
let curr_speclist : (Arg.key * Arg.spec * Arg.doc) list ref = ref []

let unknown_args_action = ref `Reject

let rev_anon_args = ref []
let anon_fun arg = match !unknown_args_action with
  | `Skip ->
      ()
  | `Add ->
      rev_anon_args := arg::!rev_anon_args
  | `Reject ->
      raise (Arg.Bad ("unexpected anonymous argument: " ^ arg))

(* keep track of the final parse action to drive the remainder of the program *)
let final_parse_action = ref (Infer Driver)

(* end parsing state *)

type 'a t =
  ?deprecated:string list -> long:Arg.key -> ?short:Arg.key ->
  ?parse_mode:parse_mode -> ?meta:string -> Arg.doc ->
  'a

let string_json_decoder ~long json = [dashdash long; YBU.to_string json]

let list_json_decoder json_decoder json = List.concat (YBU.convert_each json_decoder json)

let mk_set var value ?(deprecated=[]) ~long ?short ?parse_mode ?(meta="") doc =
  let setter () = var := value in
  ignore(
    mk ~deprecated ~long ?short ~default:() ?parse_mode ~meta doc
      ~default_to_string:(fun () -> "")
      ~decode_json:(string_json_decoder ~long)
      ~mk_setter:(fun _ _ -> setter ())
      ~mk_spec:(fun _ -> Unit setter) )

let mk_option ?(default=None) ?(default_to_string=fun _ -> "") ~f
    ?(deprecated=[]) ~long ?short ?parse_mode ?(meta="") doc =
  mk ~deprecated ~long ?short ~default ?parse_mode ~meta doc
    ~default_to_string
    ~decode_json:(string_json_decoder ~long)
    ~mk_setter:(fun var str -> var := f str)
    ~mk_spec:(fun set -> String set)

let mk_bool ?(deprecated_no=[]) ?(default=false) ?(f=fun b -> b)
    ?(deprecated=[]) ~long ?short ?parse_mode ?(meta="") doc =
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
  let mk_spec set = Unit (fun () -> set "") in
  let var =
    mk ~long ?short ~deprecated ~default ?parse_mode
      ~meta doc ~default_to_string ~mk_setter:(fun var _ -> var := f true)
      ~decode_json:(fun json ->
          [dashdash (if YBU.to_bool json then long else nolong)])
      ~mk_spec in
  ignore(
    mk ~long:nolong ?short:noshort ~deprecated:deprecated_no ~default:(not default) ?parse_mode
      ~meta nodoc ~default_to_string ~mk_setter:(fun _ _ -> var := f false)
      ~decode_json:(fun json ->
          [dashdash (if YBU.to_bool json then nolong else long)])
      ~mk_spec );
  var

let mk_bool_group ?(deprecated_no=[]) ?(default=false)
    ?(deprecated=[]) ~long ?short ?parse_mode ?(meta="") doc children no_children =
  let f b =
    IList.iter (fun child -> child := b) children ;
    IList.iter (fun child -> child := not b) no_children ;
    b
  in
  mk_bool ~deprecated ~deprecated_no ~default ~long ?short ~f ?parse_mode ~meta doc

let mk_int ~default ?(deprecated=[]) ~long ?short ?parse_mode ?(meta="") doc =
  mk ~deprecated ~long ?short ~default ?parse_mode ~meta doc
    ~default_to_string:string_of_int
    ~mk_setter:(fun var str -> var := (int_of_string str))
    ~decode_json:(string_json_decoder ~long)
    ~mk_spec:(fun set -> String set)

let mk_int_opt ?default ?(deprecated=[]) ~long ?short ?parse_mode ?(meta="") doc =
  let default_to_string = function Some f -> string_of_int f | None -> "" in
  let f s = Some (int_of_string s) in
  mk_option ~deprecated ~long ?short ~default ~default_to_string ~f ?parse_mode ~meta doc

let mk_float ~default ?(deprecated=[]) ~long ?short ?parse_mode ?(meta="") doc =
  mk ~deprecated ~long ?short ~default ?parse_mode ~meta doc
    ~default_to_string:string_of_float
    ~mk_setter:(fun var str -> var := (float_of_string str))
    ~decode_json:(string_json_decoder ~long)
    ~mk_spec:(fun set -> String set)

let mk_float_opt ?default ?(deprecated=[]) ~long ?short ?parse_mode ?(meta="") doc =
  let default_to_string = function Some f -> string_of_float f | None -> "" in
  let f s = Some (float_of_string s) in
  mk_option ~deprecated ~long ?short ~default ~default_to_string ~f ?parse_mode ~meta doc

let mk_string ~default ?(f=fun s -> s) ?(deprecated=[]) ~long ?short ?parse_mode ?(meta="") doc =
  mk ~deprecated ~long ?short ~default ?parse_mode ~meta doc
    ~default_to_string:(fun s -> s)
    ~mk_setter:(fun var str -> var := f str)
    ~decode_json:(string_json_decoder ~long)
    ~mk_spec:(fun set -> String set)

let mk_string_opt ?default ?(f=fun s -> s) ?(deprecated=[]) ~long ?short ?parse_mode
    ?(meta="") doc =
  let default_to_string = function Some s -> s | None -> "" in
  let f s = Some (f s) in
  mk_option ~deprecated ~long ?short ~default ~default_to_string ~f ?parse_mode ~meta doc

let mk_string_list ?(default=[]) ?(f=fun s -> s)
    ?(deprecated=[]) ~long ?short ?parse_mode ?(meta="") doc =
  mk ~deprecated ~long ?short ~default ?parse_mode ~meta doc
    ~default_to_string:(String.concat ~sep:", ")
    ~mk_setter:(fun var str -> var := (f str) :: !var)
    ~decode_json:(list_json_decoder (string_json_decoder ~long))
    ~mk_spec:(fun set -> String set)

let mk_path_helper ~setter ~default_to_string
    ~default ~deprecated ~long ~short ~parse_mode ~meta ~decode_json doc =
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
  mk ~deprecated ~long ?short ~default ?parse_mode ~meta doc
    ~decode_json ~default_to_string
    ~mk_setter:(fun var str ->
        let abs_path = normalize_path_in_args_being_parsed str in
        setter var abs_path)
    ~mk_spec:(fun set -> String set)

let mk_path ~default ?(deprecated=[]) ~long ?short ?parse_mode ?(meta="path") =
  mk_path_helper
    ~setter:(fun var x -> var := x)
    ~decode_json:(string_json_decoder ~long)
    ~default_to_string:(fun s -> s)
    ~default ~deprecated ~long ~short ~parse_mode ~meta

let mk_path_opt ?default ?(deprecated=[]) ~long ?short ?parse_mode ?(meta="path") =
  mk_path_helper
    ~setter:(fun var x -> var := Some x)
    ~decode_json:(string_json_decoder ~long)
    ~default_to_string:(function Some s -> s | None -> "")
    ~default ~deprecated ~long ~short ~parse_mode ~meta

let mk_path_list ?(default=[]) ?(deprecated=[]) ~long ?short ?parse_mode ?(meta="path") =
  mk_path_helper
    ~setter:(fun var x -> var := x :: !var)
    ~decode_json:(list_json_decoder (string_json_decoder ~long))
    ~default_to_string:(String.concat ~sep:", ")
    ~default ~deprecated ~long ~short ~parse_mode ~meta

let mk_symbol ~default ~symbols ~eq ?(deprecated=[]) ~long ?short ?parse_mode ?(meta="") doc =
  let strings = IList.map fst symbols in
  let sym_to_str = IList.map (fun (x,y) -> (y,x)) symbols in
  let of_string str = IList.assoc String.equal str symbols in
  let to_string sym = IList.assoc eq sym sym_to_str in
  mk ~deprecated ~long ?short ~default ?parse_mode ~meta doc
    ~default_to_string:(fun s -> to_string s)
    ~mk_setter:(fun var str -> var := of_string str)
    ~decode_json:(string_json_decoder ~long)
    ~mk_spec:(fun set -> Symbol (strings, set))

let mk_symbol_opt ~symbols ?(deprecated=[]) ~long ?short ?parse_mode ?(meta="") doc =
  let strings = IList.map fst symbols in
  let of_string str = IList.assoc String.equal str symbols in
  mk ~deprecated ~long ?short ~default:None ?parse_mode ~meta doc
    ~default_to_string:(fun _ -> "")
    ~mk_setter:(fun var str -> var := Some (of_string str))
    ~decode_json:(string_json_decoder ~long)
    ~mk_spec:(fun set -> Symbol (strings, set))

let mk_symbol_seq ?(default=[]) ~symbols ~eq ?(deprecated=[]) ~long ?short ?parse_mode
    ?(meta="") doc =
  let sym_to_str = IList.map (fun (x,y) -> (y,x)) symbols in
  let of_string str = IList.assoc String.equal str symbols in
  let to_string sym = IList.assoc eq sym sym_to_str in
  mk ~deprecated ~long ?short ~default ?parse_mode ~meta:(",-separated sequence" ^ meta) doc
    ~default_to_string:(fun syms -> String.concat ~sep:" " (IList.map to_string syms))
    ~mk_setter:(fun var str_seq ->
        var := IList.map of_string (Str.split (Str.regexp_string ",") str_seq))
    ~decode_json:(fun json ->
        [dashdash long;
         String.concat ~sep:"," (YBU.convert_each YBU.to_string json)])
    ~mk_spec:(fun set -> String set)

let mk_set_from_json ~default ~default_to_string ~f
    ?(deprecated=[]) ~long ?short ?parse_mode ?(meta="json") doc =
  mk ~deprecated ~long ?short ?parse_mode ~meta doc
    ~default ~default_to_string
    ~mk_setter:(fun var json -> var := f (Yojson.Basic.from_string json))
    ~decode_json:(fun json -> [dashdash long; Yojson.Basic.to_string json])
    ~mk_spec:(fun set -> String set)

let mk_json ?(deprecated=[]) ~long ?short ?parse_mode ?(meta="json") doc =
  mk ~deprecated ~long ?short ?parse_mode ~meta doc
    ~default:(`List []) ~default_to_string:Yojson.Basic.to_string
    ~mk_setter:(fun var json -> var := Yojson.Basic.from_string json)
    ~decode_json:(fun json -> [dashdash long; Yojson.Basic.to_string json])
    ~mk_spec:(fun set -> String set)

(** [mk_anon] always return the same ref. Anonymous arguments are only accepted if
    [parse_action_accept_unknown_args] is true. *)
let mk_anon () = rev_anon_args

let mk_rest ?(parse_mode=Infer []) doc =
  let rest = ref [] in
  let spec = Rest (fun arg -> rest := arg :: !rest) in
  add parse_mode {long = "--"; short = ""; meta = ""; doc; spec; decode_json = fun _ -> []} ;
  rest

let set_curr_speclist_for_parse_action ~incomplete ~usage parse_action =
  let full_speclist = ref [] in

  let curr_usage status =
    prerr_endline (String.concat_array ~sep:" " !args_to_parse) ;
    Arg.usage !curr_speclist usage ;
    exit status
  and full_usage status =
    Arg.usage (to_arg_speclist !full_speclist) usage ;
    exit status
  in
  let parse_tag = to_parse_tag parse_action in
  (* "-help" and "--help" are automatically recognized by Arg.parse, so we have to give them special
     treatment *)
  let add_or_suppress_help (speclist, (doc_width,left_width)) =
    let unknown opt =
      (opt, Unit (fun () -> raise (Arg.Bad ("unknown option '" ^ opt ^ "'"))), "") in
    let mk_spec ~long ?(short="") spec doc =
      pad_and_xform doc_width left_width { long; short; meta=""; spec; doc;
                                           decode_json=fun _ -> raise (Arg.Bad long)} in
    if not (equal_parse_tag parse_tag (Infer ())) then
      let skip opt =
        (opt, Unit (fun () -> ()), "") in
      speclist @ [
        (skip "--help") ;
        (skip "-help")
      ]
    else if incomplete then
      speclist @ [
        (unknown "--help") ;
        (unknown "-help")
      ]
    else
      speclist @ [
        mk_spec ~long:"help" ~short:"h"
          (Unit (fun () -> curr_usage 0))
          "Display this list of options";
        mk_spec ~long:"help-full"
          (Unit (fun () -> full_usage 0))
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
  let add_to_curr_speclist ?(add_help=false) ?header parse_action =
    let mk_header_spec heading =
      ("", Unit (fun () -> ()), "\n## " ^ heading ^ "\n") in
    let exe_descs =
      match parse_action with
      | Infer section ->
          List.Assoc.find_exn ~equal:equal_section infer_section_desc_lists section
      | Javac | NoParse ->
          to_parse_tag parse_action
          |> List.Assoc.find_exn ~equal:equal_parse_tag parse_tag_desc_lists in
    let (exe_speclist, widths) = normalize !exe_descs in
    let exe_speclist = if add_help
      then add_or_suppress_help (exe_speclist, widths)
      else exe_speclist in
    let exe_speclist = to_arg_speclist exe_speclist in
    (* Return false if the same option appears in [speclist], unless [doc] is non-empty and the
       documentation in [speclist] is empty. The goal is to keep only one instance of each option,
       and that instance is the one that has a non-empty docstring if there is one. *)
    let is_not_dup_with_doc speclist (opt, _, doc) =
      opt = "" ||
      IList.for_all (fun (opt', _, doc') ->
          (doc <> "" && doc' = "") || (not (String.equal opt opt'))) speclist in
    let unique_exe_speclist = List.filter ~f:(is_not_dup_with_doc !curr_speclist) exe_speclist in
    curr_speclist := List.filter ~f:(is_not_dup_with_doc unique_exe_speclist) !curr_speclist @
                     (match header with
                      | Some s -> (to_arg_spec_triple (mk_header_spec s)):: unique_exe_speclist
                      | None -> unique_exe_speclist)
  in
  (* speclist includes args for current exe with docs, and all other args without docs, so
     that all args can be parsed, but --help and parse failures only show external args for
     current exe *)
  (* reset the speclist between calls to this function *)
  curr_speclist := [];
  if equal_parse_action parse_action (Infer Driver) then (
    add_to_curr_speclist ~add_help:true ~header:"Driver options" (Infer Driver);
    add_to_curr_speclist ~header:"Checkers options" (Infer Checkers);
    add_to_curr_speclist ~header:"Clang-specific options" (Infer Clang);
    add_to_curr_speclist ~header:"Java-specific options" (Infer Java);
    add_to_curr_speclist ~header:"Quandary checker options" (Infer Quandary)
  ) else
    add_to_curr_speclist ~add_help:true parse_action
  ;
  assert( check_no_duplicates !curr_speclist )
  ;
  let full_desc_list = List.Assoc.find_exn ~equal:equal_parse_tag parse_tag_desc_lists parse_tag in
  full_speclist := add_or_suppress_help (normalize !full_desc_list)
  ;
  curr_usage


let select_parse_action ~incomplete ~usage action =
  let usage = set_curr_speclist_for_parse_action ~incomplete ~usage action in
  unknown_args_action := if accept_unknown_args action then `Add else `Reject;
  final_parse_action := action;
  usage

let mk_rest_actions ?(parse_mode=Infer []) doc ~usage decode_action =
  let rest = ref [] in
  let spec = String (fun arg ->
      rest := List.rev (Array.to_list (Array.slice !args_to_parse (!arg_being_parsed + 1) 0)) ;
      select_parse_action ~incomplete:false ~usage (decode_action arg) |> ignore;
      (* stop accepting new anonymous arguments *)
      unknown_args_action := `Skip) in
  add parse_mode {long = "--"; short = ""; meta = ""; doc; spec; decode_json = fun _ -> []} ;
  rest


let decode_inferconfig_to_argv path =
  let json = match Utils.read_optional_json_file path with
    | Ok json ->
        json
    | Error msg ->
        warnf "WARNING: Could not read or parse Infer config in %s:@\n%s@." path msg ;
        `Assoc [] in
  let desc_list = List.Assoc.find_exn ~equal:equal_parse_tag parse_tag_desc_lists (Infer ()) in
  let json_config = YBU.to_assoc json in
  let one_config_item result (key, json_val) =
    try
      let {decode_json} =
        List.find_exn
          ~f:(fun {long; short} ->
              String.equal key long
              || (* for deprecated options *) String.equal key short)
          !desc_list in
      decode_json json_val @ result
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
  Str.split (Str.regexp_string (String.make 1 env_var_sep)) env

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

let parse_args ~incomplete ~usage action args =
  let exe_name = Sys.executable_name in
  args_to_parse := Array.of_list (exe_name :: args);
  arg_being_parsed := 0;
  let curr_usage = select_parse_action ~incomplete ~usage action in
  (* tests if msg indicates an unknown option, as opposed to a known option with bad argument *)
  let is_unknown msg = String.is_substring msg ~substring:": unknown option" in
  let rec parse_loop () =
    try
      Arg.parse_argv_dynamic ~current:arg_being_parsed !args_to_parse curr_speclist
        anon_fun usage
    with
    | Arg.Bad _ when incomplete -> parse_loop ()
    | Arg.Bad usage_msg ->
        if !unknown_args_action <> `Reject && is_unknown usage_msg then (
          anon_fun !args_to_parse.(!arg_being_parsed);
          parse_loop ()
        ) else (
          Pervasives.prerr_string usage_msg;
          exit 2
        )
    | Arg.Help usage_msg -> Pervasives.print_string usage_msg; exit 0
  in
  parse_loop ();
  curr_usage

let parse ?(incomplete=false) ?config_file ~usage action =
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
  parse_args ~incomplete ~usage (Infer Driver) inferconfig_args |> ignore;
  (* NOTE: do not add the contents of .inferconfig to INFER_ARGS. This helps avoid hitting the
     command line size limit. *)
  parse_args ~incomplete ~usage (Infer Driver) env_args |> ignore;
  if not incomplete then add_parsed_args_to_args_to_export ();
  let curr_usage =
    let cl_args = match Array.to_list Sys.argv with _ :: tl -> tl | [] -> [] in
    let curr_usage = parse_args ~incomplete ~usage action cl_args in
    if not incomplete then add_parsed_args_to_args_to_export ();
    curr_usage in
  if not incomplete then Unix.putenv ~key:args_env_var ~data:!args_to_export;
  !final_parse_action, curr_usage
