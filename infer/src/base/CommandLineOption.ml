(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Definition and parsing of command line arguments *)

open! IStd
module F = Format
module YBU = Yojson.Basic.Util
module L = Die

let ( = ) = String.equal

let manpage_s_notes = "NOTES"

let is_env_var_set v = Option.value (Option.map (Sys.getenv v) ~f:(( = ) "1")) ~default:false

(** The working directory of the initial invocation of infer, to which paths passed as command line
    options are relative. *)
let init_work_dir, is_originator =
  match Sys.getenv "INFER_CWD" with
  | Some dir ->
      (dir, false)
  | None ->
      let real_cwd = Utils.realpath (Sys.getcwd ()) in
      Unix.putenv ~key:"INFER_CWD" ~data:real_cwd ;
      (real_cwd, true)


let strict_mode_env_var = "INFER_STRICT_MODE"

let strict_mode = is_env_var_set strict_mode_env_var

let warnf =
  if strict_mode then fun fmt -> L.(die UserError) fmt
  else if not is_originator then fun fmt -> F.ifprintf F.err_formatter fmt
  else F.eprintf


(** This is the subset of Arg.spec that we actually use. What's important is that all these specs
    call back functions. We use this to mark deprecated arguments. What's not important is that, eg,
    Arg.Float is missing. *)
type spec =
  | Unit of (unit -> unit)
  | String of (string -> unit)
  | Symbol of string list * (string -> unit)

let to_arg_spec = function
  | Unit f ->
      Arg.Unit f
  | String f ->
      Arg.String f
  | Symbol (symbols, f) ->
      Arg.Symbol (symbols, f)


let to_arg_spec_triple (x, spec, y) = (x, to_arg_spec spec, y)

let to_arg_speclist = List.map ~f:to_arg_spec_triple

(* NOTE: All variants must be also added to `all_parse_modes` below *)
type parse_mode = InferCommand | Javac | NoParse [@@deriving compare]

let equal_parse_mode = [%compare.equal: parse_mode]

let all_parse_modes = [InferCommand; Javac; NoParse]

type anon_arg_action =
  {parse_subcommands: bool; parse_argfiles: bool; on_unknown: [`Add | `Reject | `Skip]}

let anon_arg_action_of_parse_mode parse_mode =
  let parse_subcommands, parse_argfiles, on_unknown =
    match parse_mode with
    | InferCommand ->
        (true, true, `Reject)
    | Javac ->
        (false, true, `Skip)
    | NoParse ->
        (false, false, `Skip)
  in
  {parse_subcommands; parse_argfiles; on_unknown}


type command_doc =
  { title: Cmdliner.Manpage.title
  ; manual_before_options: Cmdliner.Manpage.block list
  ; manual_options:
      [`Prepend of Cmdliner.Manpage.block list | `Replace of Cmdliner.Manpage.block list]
  ; manual_after_options: Cmdliner.Manpage.block list }

type desc =
  { long: string
  ; short: string
  ; meta: string
  ; doc: string
  ; spec: spec
        (** how to go from an option in the json config file to a list of command-line options *)
  ; decode_json: inferconfig_dir:string -> Yojson.Basic.json -> string list }

let dashdash ?short long =
  match (long, short) with
  | "", (None | Some "") | "--", _ ->
      long
  | "", Some short ->
      "-" ^ short
  | _ ->
      "--" ^ long


let xdesc {long; short; spec} =
  let key long short =
    match (long, short) with
    | "", "" ->
        ""
    | "--", _ ->
        "--"
    | "", _ ->
        "-" ^ short
    | _ ->
        "--" ^ long
  in
  let xspec =
    match spec with
    (* translate Symbol to String for better formatting of --help messages *)
    | Symbol (symbols, action) ->
        String
          (fun arg ->
            if List.mem ~equal:String.equal symbols arg then action arg
            else
              raise
                (Arg.Bad
                   (F.sprintf "wrong argument '%s'; option '%s' expects one of: %s" arg
                      (dashdash ~short long)
                      (String.concat ~sep:" | " symbols))) )
    | _ ->
        spec
  in
  (* Arg doesn't need to know anything about documentation since we generate our own *)
  (key long short, xspec, "")


let check_no_duplicates desc_list =
  let rec check_for_duplicates_ = function
    | [] | [_] ->
        true
    | (x, _, _) :: (y, _, _) :: _ when x <> "" && x = y ->
        L.(die InternalError) "Multiple definitions of command line option: %s" x
    | _ :: tl ->
        check_for_duplicates_ tl
  in
  check_for_duplicates_
    (List.sort ~compare:(fun (x, _, _) (y, _, _) -> String.compare x y) desc_list)


let parse_mode_desc_lists = List.map ~f:(fun parse_mode -> (parse_mode, ref [])) all_parse_modes

module SectionMap = Caml.Map.Make (struct
  type t = String.t

  (* this must be the reverse of the order in which we want the sections to appear in the
       manual *)
  let compare s1 s2 =
    if String.equal s1 s2 then (* this simplifies the next two cases *)
      0
    else if String.equal s1 Cmdliner.Manpage.s_options then
      (* ensure OPTIONS section is last (hence first in the manual) *)
      1
    else if String.equal s2 Cmdliner.Manpage.s_options then (* same as above *)
      -1
    else (* reverse order *)
      String.compare s2 s1
end)

let help_sections_desc_lists =
  List.map InferCommand.all_commands ~f:(fun command -> (command, ref SectionMap.empty))


let visible_descs_list = ref []

let hidden_descs_list = ref []

(** add [desc] to the one relevant parse_tag_desc_lists for the purposes of parsing, and, in the
    case of InferCommand, include [desc] in --help only for the relevant sections. *)
let add parse_mode sections desc =
  let desc_list = List.Assoc.find_exn ~equal:equal_parse_mode parse_mode_desc_lists parse_mode in
  desc_list := desc :: !desc_list ;
  let add_to_section (command, section) =
    let sections =
      List.Assoc.find_exn ~equal:InferCommand.equal help_sections_desc_lists command
    in
    let prev_contents = try SectionMap.find section !sections with Caml.Not_found -> [] in
    sections := SectionMap.add section (desc :: prev_contents) !sections
  in
  List.iter sections ~f:add_to_section ;
  if List.is_empty sections then hidden_descs_list := desc :: !hidden_descs_list
  else
    let desc_infer =
      if String.equal "" desc.doc then desc
      else
        let oxford_comma l =
          let rec aux acc l =
            match (l, acc) with
            | [], _ ->
                assert false
            | [x], [] ->
                x
            | [x; y], [] ->
                Printf.sprintf "%s and %s" x y
            | [x; y], acc ->
                Printf.sprintf "%s, %s, and %s" (String.concat ~sep:", " (List.rev acc)) x y
            | x :: tl, acc ->
                aux (x :: acc) tl
          in
          aux [] l
        in
        (* in the help of `infer` itself, show in which specific commands the option is used *)
        let commands =
          List.map ~f:fst sections
          |> List.sort ~compare:InferCommand.compare
          |> List.remove_consecutive_duplicates ~equal:InferCommand.equal
          |> List.map ~f:(fun cmd ->
                 let exe = InferCommand.to_exe_name cmd in
                 Printf.sprintf "$(b,%s)(1)" (Cmdliner.Manpage.escape exe) )
          |> oxford_comma
        in
        {desc with doc= Printf.sprintf "%s\nSee also %s." desc.doc commands}
    in
    visible_descs_list := desc_infer :: !visible_descs_list ;
    ()


let deprecate_desc parse_mode ~long ~short ~deprecated doc desc =
  let warn source =
    let source_s =
      match source with
      | `CLI ->
          ""
      | `Inferconfig root ->
          Printf.sprintf " in %s/.inferconfig:" root
    in
    match parse_mode with
    | Javac | NoParse ->
        ()
    | InferCommand when long <> "" ->
        warnf "WARNING:%s '-%s' is deprecated. Use '--%s'%s instead.@." source_s deprecated long
          (if short = "" then "" else Printf.sprintf " or '-%s'" short)
    | InferCommand ->
        warnf "WARNING:%s '-%s' is deprecated. Here is its documentation:@\n%s@." source_s
          deprecated doc
  in
  let warn_then_f f x =
    warn `CLI ;
    f x
  in
  let deprecated_spec =
    match desc.spec with
    | Unit f ->
        Unit (warn_then_f f)
    | String f ->
        String (warn_then_f f)
    | Symbol (symbols, f) ->
        Symbol (symbols, warn_then_f f)
  in
  let deprecated_decode_json ~inferconfig_dir j =
    warn (`Inferconfig inferconfig_dir) ;
    desc.decode_json ~inferconfig_dir j
  in
  { long= ""
  ; short= deprecated
  ; meta= ""
  ; doc= ""
  ; spec= deprecated_spec
  ; decode_json= deprecated_decode_json }


let mk ?(deprecated = []) ?(parse_mode = InferCommand) ?(in_help = []) ~long ?short:short0 ~default
    ~meta doc ~default_to_string ~decode_json ~mk_setter ~mk_spec =
  let variable = ref default in
  let closure = mk_setter variable in
  let setter str =
    try closure str with exc ->
      raise (Arg.Bad (F.sprintf "bad value %s for flag %s (%s)" str long (Exn.to_string exc)))
  in
  let spec = mk_spec setter in
  let doc =
    let default_string = default_to_string default in
    if default_string = "" then doc
    else
      let doc_default_sep = if String.is_suffix ~suffix:"\n" doc then "" else " " in
      doc ^ doc_default_sep ^ "(default: $(i," ^ Cmdliner.Manpage.escape default_string ^ "))"
  in
  let short = match short0 with Some c -> String.of_char c | None -> "" in
  let desc = {long; short; meta; doc; spec; decode_json} in
  (* add desc for long option, with documentation (which includes any short option) for exes *)
  if long <> "" then add parse_mode in_help desc ;
  (* add desc for short option only for parsing, without documentation *)
  if short <> "" then add parse_mode [] {desc with long= ""; meta= ""; doc= ""} ;
  (* add desc for deprecated options only for parsing, without documentation *)
  List.iter deprecated ~f:(fun deprecated ->
      deprecate_desc parse_mode ~long ~short ~deprecated doc desc |> add parse_mode [] ) ;
  variable


(* begin parsing state *)
(* arguments passed to Arg.parse_argv_dynamic, susceptible to be modified on the fly when parsing *)
let args_to_parse : string array ref = ref (Array.of_list [])

(* reference used by Arg.parse_argv_dynamic to track the index of the argument being parsed *)
let arg_being_parsed : int ref = ref 0

(* list of arg specifications currently being used by Arg.parse_argv_dynamic *)
let curr_speclist : (Arg.key * Arg.spec * Arg.doc) list ref = ref []

let anon_arg_action = ref (anon_arg_action_of_parse_mode InferCommand)

let subcommands = ref []

let subcommand_actions = ref []

let rev_anon_args = ref []

(* keep track of the current active command to drive the remainder of the program *)
let curr_command = ref None

(* end parsing state *)

type 'a t =
     ?deprecated:string list
  -> long:Arg.key
  -> ?short:char
  -> ?parse_mode:parse_mode
  -> ?in_help:(InferCommand.t * string) list
  -> ?meta:string
  -> Arg.doc
  -> 'a

let int_json_decoder ~flag ~inferconfig_dir json =
  let int_as_string =
    match json with
    | `String s ->
        warnf "WARNING: in %s/.inferconfig for option '%s', use an integer instead of a string.@."
          inferconfig_dir flag ;
        s
    | json ->
        string_of_int (YBU.to_int json)
  in
  [flag; int_as_string]


let string_json_decoder ~flag ~inferconfig_dir:_ json = [flag; YBU.to_string json]

let path_json_decoder ~flag ~inferconfig_dir json =
  let abs_path =
    let path = YBU.to_string json in
    if Filename.is_relative path then inferconfig_dir ^/ path else path
  in
  [flag; abs_path]


let list_json_decoder json_decoder ~inferconfig_dir json =
  List.concat (YBU.convert_each (json_decoder ~inferconfig_dir) json)


(* selects "--long" if not empty, or some non-empty "-deprecated" or "-short" *)
let mk_flag ~deprecated ?short ~long =
  if String.is_empty long then
    match short with
    | Some c ->
        Printf.sprintf "-%c" c
    | None -> (
      match deprecated with
      | s :: _ ->
          "-" ^ s
      | [] ->
          L.die InternalError "Option has no corresponding flag that is not empty" )
  else dashdash long


let mk_set var value ?(deprecated = []) ~long ?short ?parse_mode ?in_help ?(meta = "") doc =
  let setter () = var := value in
  let flag = mk_flag ~deprecated ?short ~long in
  ignore
    (mk ~deprecated ~long ?short ~default:() ?parse_mode ?in_help ~meta doc
       ~default_to_string:(fun () -> "")
       ~decode_json:(string_json_decoder ~flag)
       ~mk_setter:(fun _ _ -> setter ())
       ~mk_spec:(fun _ -> Unit setter))


let mk_with_reset value ~reset_doc ?deprecated ~long ?parse_mode mk =
  let var = mk () in
  if not (String.equal "" long) then
    (* Do not pass any ~in_help value so that the reset options only show up in --help-full and do
       not clutter --help. *)
    mk_set var value ?deprecated ~long:(long ^ "-reset") ?parse_mode reset_doc ;
  var


let reset_doc_opt ~long = Printf.sprintf "Cancel the effect of $(b,%s)." (dashdash long)

let reset_doc_list ~long = Printf.sprintf "Set $(b,%s) to the empty list." (dashdash long)

let mk_option ?(default = None) ?(default_to_string = fun _ -> "") ~f ?(mk_reset = true)
    ?(deprecated = []) ~long ?short ?parse_mode ?in_help ?(meta = "string") doc =
  let flag = mk_flag ~deprecated ?short ~long in
  let mk () =
    mk ~deprecated ~long ?short ~default ?parse_mode ?in_help ~meta doc ~default_to_string
      ~decode_json:(string_json_decoder ~flag)
      ~mk_setter:(fun var str -> var := f str)
      ~mk_spec:(fun set -> String set)
  in
  if mk_reset then
    let reset_doc = reset_doc_opt ~long in
    mk_with_reset None ~reset_doc ~long ?parse_mode mk
  else mk ()


let mk_bool ?(deprecated_no = []) ?(default = false) ?(f = fun b -> b) ?(deprecated = []) ~long
    ?short ?parse_mode ?in_help ?(meta = "") doc0 =
  let nolong =
    let len = String.length long in
    if len > 3 && String.sub long ~pos:0 ~len:3 = "no-" then String.sub long ~pos:3 ~len:(len - 3)
    else "no-" ^ long
  and noshort =
    Option.map
      ~f:(fun short ->
        if Char.is_lowercase short then Char.uppercase short else Char.lowercase short )
      short
  in
  let doc long short =
    match short with
    | Some short ->
        doc0 ^ " (Conversely: $(b,--" ^ long ^ ") | $(b,-" ^ String.of_char short ^ "))"
    | None ->
        doc0 ^ " (Conversely: $(b,--" ^ long ^ "))"
  in
  let doc, nodoc =
    if String.equal doc0 "" then ("", "")
    else if not default then ("Activates: " ^ doc nolong noshort, "")
    else ("", "Deactivates: " ^ doc long short)
  in
  let default_to_string _ = "" in
  let mk_spec set = Unit (fun () -> set "") in
  let var =
    mk ~long ?short ~deprecated ~default ?parse_mode ?in_help ~meta doc ~default_to_string
      ~mk_setter:(fun var _ -> var := f true)
      ~decode_json:(fun ~inferconfig_dir:_ json ->
        [dashdash (if YBU.to_bool json then long else nolong)] )
      ~mk_spec
  in
  ignore
    (mk ~long:nolong ?short:noshort ~deprecated:deprecated_no ~default:(not default) ?parse_mode
       ?in_help ~meta nodoc ~default_to_string
       ~mk_setter:(fun _ _ -> var := f false)
       ~decode_json:(fun ~inferconfig_dir:_ json ->
         [dashdash (if YBU.to_bool json then nolong else long)] )
       ~mk_spec) ;
  var


let mk_bool_group ?(deprecated_no = []) ?(default = false) ?f:(f0 = Fn.id) ?(deprecated = []) ~long
    ?short ?parse_mode ?in_help ?meta doc children no_children =
  let f b =
    List.iter ~f:(fun child -> child := b) children ;
    List.iter ~f:(fun child -> child := not b) no_children ;
    f0 b
  in
  mk_bool ~deprecated ~deprecated_no ~default ~long ?short ~f ?parse_mode ?in_help ?meta doc


let mk_int ~default ?(default_to_string = string_of_int) ?(f = Fn.id) ?(deprecated = []) ~long
    ?short ?parse_mode ?in_help ?(meta = "int") doc =
  let flag = mk_flag ~deprecated ?short ~long in
  mk ~deprecated ~long ?short ~default ?parse_mode ?in_help ~meta doc ~default_to_string
    ~mk_setter:(fun var str -> var := f (int_of_string str))
    ~decode_json:(int_json_decoder ~flag)
    ~mk_spec:(fun set -> String set)


let mk_int_opt ?default ?(default_to_string = Option.value_map ~default:"" ~f:string_of_int)
    ?f:(f0 = Fn.id) ?(deprecated = []) ~long ?short ?parse_mode ?in_help ?(meta = "int") doc =
  let f s = Some (f0 (int_of_string s)) in
  mk_option ~deprecated ~long ?short ~default ~default_to_string ~f ?parse_mode ?in_help ~meta doc


let mk_float_opt ?default ?(default_to_string = Option.value_map ~default:"" ~f:string_of_float)
    ?(deprecated = []) ~long ?short ?parse_mode ?in_help ?(meta = "float") doc =
  let f s = Some (float_of_string s) in
  mk_option ~deprecated ~long ?short ~default ~default_to_string ~f ?parse_mode ?in_help ~meta doc


let mk_string ~default ?(default_to_string = Fn.id) ?(f = fun s -> s) ?(deprecated = []) ~long
    ?short ?parse_mode ?in_help ?(meta = "string") doc =
  let flag = mk_flag ~deprecated ?short ~long in
  mk ~deprecated ~long ?short ~default ?parse_mode ?in_help ~meta doc ~default_to_string
    ~mk_setter:(fun var str -> var := f str)
    ~decode_json:(string_json_decoder ~flag)
    ~mk_spec:(fun set -> String set)


let mk_string_opt ?default ?(default_to_string = Option.value ~default:"") ?(f = fun s -> s)
    ?mk_reset ?(deprecated = []) ~long ?short ?parse_mode ?in_help ?(meta = "string") doc =
  let f s = Some (f s) in
  mk_option ~deprecated ~long ?short ~default ~default_to_string ~f ?mk_reset ?parse_mode ?in_help
    ~meta doc


let mk_string_list ?(default = []) ?(default_to_string = String.concat ~sep:",") ?(f = fun s -> s)
    ?(deprecated = []) ~long ?short ?parse_mode ?in_help ?(meta = "string") doc =
  let flag = mk_flag ~deprecated ?short ~long in
  let mk () =
    mk ~deprecated ~long ?short ~default ?parse_mode ?in_help ~meta:("+" ^ meta) doc
      ~default_to_string
      ~mk_setter:(fun var str -> var := f str :: !var)
      ~decode_json:(list_json_decoder (string_json_decoder ~flag))
      ~mk_spec:(fun set -> String set)
  in
  let reset_doc = reset_doc_list ~long in
  mk_with_reset [] ~reset_doc ~long ?parse_mode mk


let normalize_path_in_args_being_parsed ?(f = Fn.id) ~is_anon_arg str =
  if Filename.is_relative str then (
    (* Replace relative paths with absolute ones on the fly in the args being parsed. This assumes
       that [!arg_being_parsed] points at either [str] (if [is_anon_arg]) or at the option name
       position in [!args_to_parse], as is the case e.g. when calling
       [Arg.parse_argv_dynamic ~current:arg_being_parsed !args_to_parse ...]. *)
    let root = Unix.getcwd () in
    let abs_path = Utils.filename_to_absolute ~root str in
    !args_to_parse.((!arg_being_parsed + if is_anon_arg then 0 else 1)) <- f abs_path ;
    abs_path )
  else str


let mk_path_helper ~setter ~default_to_string ~default ~deprecated ~long ~short ~parse_mode
    ~in_help ~meta ~decode_json doc =
  mk ~deprecated ~long ?short ~default ?parse_mode ?in_help ~meta doc ~decode_json
    ~default_to_string
    ~mk_setter:(fun var str ->
      let abs_path = normalize_path_in_args_being_parsed ~is_anon_arg:false str in
      setter var abs_path )
    ~mk_spec:(fun set -> String set)


let mk_path ~default ?(default_to_string = Fn.id) ?(f = Fn.id) ?(deprecated = []) ~long ?short
    ?parse_mode ?in_help ?(meta = "path") =
  let flag = mk_flag ~deprecated ?short ~long in
  mk_path_helper
    ~setter:(fun var x -> var := f x)
    ~decode_json:(path_json_decoder ~flag) ~default_to_string ~default ~deprecated ~long ~short
    ~parse_mode ~in_help ~meta


let mk_path_opt ?default ?(default_to_string = Option.value ~default:"") ?(deprecated = []) ~long
    ?short ?parse_mode ?in_help ?(meta = "path") doc =
  let mk () =
    let flag = mk_flag ~deprecated ?short ~long in
    mk_path_helper
      ~setter:(fun var x -> var := Some x)
      ~decode_json:(path_json_decoder ~flag) ~default_to_string ~default ~deprecated ~long ~short
      ~parse_mode ~in_help ~meta doc
  in
  let reset_doc = reset_doc_opt ~long in
  mk_with_reset None ~reset_doc ~long ?parse_mode mk


let mk_path_list ?(default = []) ?(default_to_string = String.concat ~sep:", ") ?(deprecated = [])
    ~long ?short ?parse_mode ?in_help ?(meta = "path") doc =
  let flag = mk_flag ~deprecated ?short ~long in
  let mk () =
    mk_path_helper
      ~setter:(fun var x -> var := x :: !var)
      ~decode_json:(list_json_decoder (path_json_decoder ~flag))
      ~default_to_string ~default ~deprecated ~long ~short ~parse_mode ~in_help ~meta:("+" ^ meta)
      doc
  in
  let reset_doc = reset_doc_list ~long in
  mk_with_reset [] ~reset_doc ~long ?parse_mode mk


let mk_symbols_meta symbols =
  let strings = List.map ~f:fst symbols in
  Printf.sprintf "{ %s }" (String.concat ~sep:" | " strings)


let mk_symbol ~default ~symbols ~eq ?(f = Fn.id) ?(deprecated = []) ~long ?short ?parse_mode
    ?in_help ?meta doc =
  let strings = List.map ~f:fst symbols in
  let sym_to_str = List.map ~f:(fun (x, y) -> (y, x)) symbols in
  let of_string str = List.Assoc.find_exn ~equal:String.equal symbols str in
  let to_string sym = List.Assoc.find_exn ~equal:eq sym_to_str sym in
  let meta = Option.value meta ~default:(mk_symbols_meta symbols) in
  let flag = mk_flag ~deprecated ?short ~long in
  mk ~deprecated ~long ?short ~default ?parse_mode ?in_help ~meta doc
    ~default_to_string:(fun s -> to_string s)
    ~mk_setter:(fun var str -> var := of_string str |> f)
    ~decode_json:(string_json_decoder ~flag)
    ~mk_spec:(fun set -> Symbol (strings, set))


let mk_symbol_opt ~symbols ?(f = Fn.id) ?(mk_reset = true) ?(deprecated = []) ~long ?short
    ?parse_mode ?in_help ?meta doc =
  let strings = List.map ~f:fst symbols in
  let of_string str = List.Assoc.find_exn ~equal:String.equal symbols str in
  let meta = Option.value meta ~default:(mk_symbols_meta symbols) in
  let flag = mk_flag ~deprecated ?short ~long in
  let mk () =
    mk ~deprecated ~long ?short ~default:None ?parse_mode ?in_help ~meta doc
      ~default_to_string:(fun _ -> "")
      ~mk_setter:(fun var str -> var := Some (f (of_string str)))
      ~decode_json:(string_json_decoder ~flag)
      ~mk_spec:(fun set -> Symbol (strings, set))
  in
  if mk_reset then
    let reset_doc = reset_doc_opt ~long in
    mk_with_reset None ~reset_doc ~long ?parse_mode mk
  else mk ()


let mk_symbol_seq ?(default = []) ~symbols ~eq ?(deprecated = []) ~long ?short ?parse_mode ?in_help
    ?meta doc =
  let sym_to_str = List.map ~f:(fun (x, y) -> (y, x)) symbols in
  let of_string str = List.Assoc.find_exn ~equal:String.equal symbols str in
  let to_string sym = List.Assoc.find_exn ~equal:eq sym_to_str sym in
  let meta = Option.value meta ~default:(",-separated sequence of " ^ mk_symbols_meta symbols) in
  mk ~deprecated ~long ?short ~default ?parse_mode ?in_help ~meta doc
    ~default_to_string:(fun syms -> String.concat ~sep:" " (List.map ~f:to_string syms))
    ~mk_setter:(fun var str_seq -> var := List.map ~f:of_string (String.split ~on:',' str_seq))
    ~decode_json:(fun ~inferconfig_dir:_ json ->
      [dashdash long; String.concat ~sep:"," (YBU.convert_each YBU.to_string json)] )
    ~mk_spec:(fun set -> String set)


let mk_json ?(deprecated = []) ~long ?short ?parse_mode ?in_help ?(meta = "json") doc =
  mk ~deprecated ~long ?short ?parse_mode ?in_help ~meta doc ~default:(`List [])
    ~default_to_string:Yojson.Basic.to_string
    ~mk_setter:(fun var json -> var := Yojson.Basic.from_string json)
    ~decode_json:(fun ~inferconfig_dir:_ json -> [dashdash long; Yojson.Basic.to_string json])
    ~mk_spec:(fun set -> String set)


(** [mk_anon] always return the same ref. Anonymous arguments are only accepted if
    [parse_action_accept_unknown_args] is true. *)
let mk_anon () = rev_anon_args

let normalize_desc_list speclist =
  let norm k =
    let remove_no s =
      let len = String.length k in
      if len > 3 && String.sub s ~pos:0 ~len:3 = "no-" then String.sub s ~pos:3 ~len:(len - 3)
      else s
    in
    let remove_weird_chars =
      String.filter ~f:(function 'a' .. 'z' | '0' .. '9' | '-' -> true | _ -> false)
    in
    remove_weird_chars @@ String.lowercase @@ remove_no k
  in
  let compare_specs {long= x} {long= y} =
    match (x, y) with
    | "--", "--" ->
        0
    | "--", _ ->
        1
    | _, "--" ->
        -1
    | _ ->
        let lower_norm s = String.lowercase @@ norm s in
        String.compare (lower_norm x) (lower_norm y)
  in
  let sort speclist = List.sort ~compare:compare_specs speclist in
  sort speclist


let mk_command_doc ~title ~section ~version ~date ~short_description ~synopsis ~description
    ?options ?exit_status ?environment ?files ?notes ?bugs ?examples ~see_also command_str =
  let add_if section blocks =
    match blocks with None -> `Blocks [] | Some bs -> `Blocks (`S section :: bs)
  in
  let manual_before_options =
    [ `S Cmdliner.Manpage.s_name
    ; (* the format of the following line is mandated by man(7) *)
      `Pre (Printf.sprintf "%s - %s" command_str short_description)
    ; `S Cmdliner.Manpage.s_synopsis
    ; `Blocks synopsis
    ; `S Cmdliner.Manpage.s_description
    ; `Blocks description ]
  in
  let manual_options = Option.value ~default:(`Prepend []) options in
  let manual_after_options =
    [ add_if Cmdliner.Manpage.s_exit_status exit_status
    ; add_if Cmdliner.Manpage.s_environment environment
    ; add_if Cmdliner.Manpage.s_files files
    ; add_if manpage_s_notes notes
    ; add_if Cmdliner.Manpage.s_bugs bugs
    ; add_if Cmdliner.Manpage.s_examples examples
    ; `S Cmdliner.Manpage.s_see_also
    ; `Blocks see_also ]
  in
  let command_doc =
    { title= (command_str, section, date, version, title)
    ; manual_before_options
    ; manual_options
    ; manual_after_options }
  in
  command_doc


let set_curr_speclist_for_parse_mode ~usage parse_mode =
  let curr_usage status =
    prerr_endline (String.concat_array ~sep:" " !args_to_parse) ;
    prerr_endline usage ;
    Pervasives.exit status
  in
  (* "-help" and "--help" are automatically recognized by Arg.parse, so we have to give them special
     treatment *)
  let add_or_suppress_help speclist =
    let unknown opt =
      (opt, Unit (fun () -> raise (Arg.Bad ("unknown option '" ^ opt ^ "'"))), "")
    in
    let has_opt opt = List.exists ~f:(fun (o, _, _) -> String.equal opt o) speclist in
    let add_unknown opt = if not (has_opt opt) then List.cons (unknown opt) else Fn.id in
    add_unknown "-help" @@ add_unknown "--help" @@ speclist
  in
  let full_desc_list =
    List.Assoc.find_exn ~equal:equal_parse_mode parse_mode_desc_lists parse_mode
  in
  curr_speclist :=
    normalize_desc_list !full_desc_list
    |> List.map ~f:xdesc |> add_or_suppress_help |> to_arg_speclist ;
  assert (check_no_duplicates !curr_speclist) ;
  curr_usage


let select_parse_mode ~usage parse_mode =
  let print_usage = set_curr_speclist_for_parse_mode ~usage parse_mode in
  anon_arg_action := anon_arg_action_of_parse_mode parse_mode ;
  print_usage


let string_of_command command =
  let _, s, _ = List.Assoc.find_exn !subcommands ~equal:InferCommand.equal command in
  s


let mk_rest_actions ?(parse_mode = InferCommand) ?(in_help = []) doc ~usage decode_action =
  let rest = ref [] in
  let spec =
    String
      (fun arg ->
        rest := List.rev (Array.to_list (Array.slice !args_to_parse (!arg_being_parsed + 1) 0)) ;
        select_parse_mode ~usage (decode_action arg) |> ignore )
  in
  add parse_mode in_help
    {long= "--"; short= ""; meta= ""; doc; spec; decode_json= (fun ~inferconfig_dir:_ _ -> [])} ;
  rest


let mk_subcommand command ?on_unknown_arg:(on_unknown = `Reject) ~name ?deprecated_long ?parse_mode
    ?in_help command_doc =
  let switch () =
    curr_command := Some command ;
    anon_arg_action := {!anon_arg_action with on_unknown}
  in
  ( match deprecated_long with
  | Some long ->
      ignore
        (mk ~long ~default:() ?parse_mode ?in_help ~meta:"" ""
           ~default_to_string:(fun () -> "")
           ~decode_json:(fun ~inferconfig_dir:_ _ ->
             raise (Arg.Bad ("Bad option in config file: " ^ long)) )
           ~mk_setter:(fun _ _ ->
             warnf "WARNING: '%s' is deprecated. Please use '%s' instead.@\n" (dashdash long) name ;
             switch () )
           ~mk_spec:(fun set -> Unit (fun () -> set "")))
  | None ->
      () ) ;
  subcommands := (command, (command_doc, name, in_help)) :: !subcommands ;
  subcommand_actions := (name, switch) :: !subcommand_actions


let args_from_argfile arg =
  let abs_fname =
    let fname = String.slice arg 1 (String.length arg) in
    normalize_path_in_args_being_parsed ~f:(fun s -> "@" ^ s) ~is_anon_arg:true fname
  in
  match In_channel.read_lines abs_fname with
  | lines ->
      let strip = Utils.strip_balanced_once ~drop:(function '"' | '\'' -> true | _ -> false) in
      List.map ~f:strip lines
  | exception e ->
      raise (Arg.Bad ("Error reading argument file '" ^ abs_fname ^ "': " ^ Exn.to_string e))


exception SubArguments of string list

let anon_fun arg =
  if !anon_arg_action.parse_argfiles && String.is_prefix arg ~prefix:"@" then
    (* stop parsing the current args and go look in that argfile *)
    raise (SubArguments (args_from_argfile arg))
  else if
    !anon_arg_action.parse_subcommands
    && List.Assoc.mem !subcommand_actions ~equal:String.equal arg
  then
    let command_switch = List.Assoc.find_exn !subcommand_actions ~equal:String.equal arg in
    match (!curr_command, is_originator) with
    | None, _ | Some _, false ->
        command_switch ()
    | Some command, true ->
        raise
          (Arg.Bad
             (Printf.sprintf "More than one subcommand specified: '%s', '%s'"
                (string_of_command command) arg))
  else
    match !anon_arg_action.on_unknown with
    | `Add ->
        rev_anon_args := arg :: !rev_anon_args
    | `Skip ->
        ()
    | `Reject ->
        raise (Arg.Bad (Printf.sprintf "Unexpected anonymous argument: '%s'" arg))


let decode_inferconfig_to_argv path =
  let json =
    match Utils.read_json_file path with
    | Ok json ->
        json
    | Error msg ->
        warnf "WARNING: Could not read or parse Infer config in %s:@\n%s@." path msg ;
        `Assoc []
  in
  let desc_list = List.Assoc.find_exn ~equal:equal_parse_mode parse_mode_desc_lists InferCommand in
  let json_config = YBU.to_assoc json in
  let inferconfig_dir = Filename.dirname path in
  let one_config_item result (key, json_val) =
    try
      let {decode_json} =
        List.find_exn
          ~f:(fun {long; short} ->
            String.equal key long || String.equal key short
            (* for deprecated options *)
            || (* for deprecated options that start with "-" *) String.equal ("-" ^ key) short )
          !desc_list
      in
      decode_json ~inferconfig_dir json_val @ result
    with
    | Not_found_s _ | Caml.Not_found ->
        warnf "WARNING: while reading config file %s:@\nUnknown option %s@." path key ;
        result
    | YBU.Type_error (msg, json) ->
        warnf "WARNING: while reading config file %s:@\nIll-formed value %s for option %s: %s@."
          path (Yojson.Basic.to_string json) key msg ;
        result
  in
  List.fold ~f:one_config_item ~init:[] json_config


(** separator of argv elements when encoded into environment variables *)
let env_var_sep = '^'

let encode_argv_to_env argv =
  String.concat ~sep:(String.make 1 env_var_sep)
    (List.filter
       ~f:(fun arg ->
         (not (String.contains arg env_var_sep))
         ||
         ( warnf "WARNING: Ignoring unsupported option containing '%c' character: %s@\n"
             env_var_sep arg ;
           false ) )
       argv)


let decode_env_to_argv env =
  String.split ~on:env_var_sep env |> List.filter ~f:(Fn.non String.is_empty)


(** [prefix_before_rest (prefix @ ["--" :: rest])] is [prefix] where "--" is not in [prefix]. *)
let rev_prefix_before_rest args =
  let rec rev_prefix_before_rest_ rev_keep = function
    | [] | "--" :: _ ->
        rev_keep
    | keep :: args ->
        rev_prefix_before_rest_ (keep :: rev_keep) args
  in
  rev_prefix_before_rest_ [] args


(** environment variable use to pass arguments from parent to child processes *)
let args_env_var = "INFER_ARGS"

let extra_env_args = ref []

let extend_env_args args = extra_env_args := List.rev_append args !extra_env_args

let parse_args ~usage initial_action ?initial_command args =
  let exe_name = Sys.executable_name in
  args_to_parse := Array.of_list (exe_name :: args) ;
  arg_being_parsed := 0 ;
  let curr_usage = select_parse_mode ~usage initial_action in
  Option.iter initial_command ~f:(fun command ->
      let switch =
        List.Assoc.find_exn !subcommand_actions ~equal:String.equal (string_of_command command)
      in
      switch () ) ;
  (* tests if msg indicates an unknown option, as opposed to a known option with bad argument *)
  let is_unknown msg = String.is_substring msg ~substring:": unknown option" in
  let rec parse_loop () =
    try
      Arg.parse_argv_dynamic ~current:arg_being_parsed !args_to_parse curr_speclist anon_fun usage
    with
    | SubArguments args ->
        (* stop parsing the current arguments and parse [args] for a while *)
        let saved_args = !args_to_parse in
        let saved_current = !arg_being_parsed in
        args_to_parse := Array.of_list (exe_name :: args) ;
        arg_being_parsed := 0 ;
        parse_loop () ;
        (* resume argument parsing *)
        args_to_parse := saved_args ;
        arg_being_parsed := saved_current ;
        parse_loop ()
    | Arg.Bad usage_msg ->
        if !anon_arg_action.on_unknown <> `Reject && is_unknown usage_msg then (
          anon_fun !args_to_parse.(!arg_being_parsed) ;
          parse_loop () )
        else (
          ANSITerminal.prerr_string L.(term_styles_of_style Fatal) usage_msg ;
          Pervasives.exit 1 )
    | Arg.Help _ ->
        (* we handle --help by ourselves and error on -help, so Arg has no way to raise Help
           anymore *)
        assert false
  in
  parse_loop () ; curr_usage


let keep_args_file = ref false

let parse ?config_file ~usage action initial_command =
  let env_args = decode_env_to_argv (Option.value (Sys.getenv args_env_var) ~default:"") in
  let inferconfig_args =
    Option.map ~f:decode_inferconfig_to_argv config_file |> Option.value ~default:[]
  in
  let args_to_export = ref "" in
  let add_parsed_args_to_args_to_export () =
    (* reread args_to_parse instead of using all_args since mk_path_helper may have modified them *)
    let prog_args =
      List.rev_append
        (rev_prefix_before_rest (Array.to_list !args_to_parse))
        (List.rev !extra_env_args)
    in
    (* do not include program path in args passed via env var *)
    let args = Option.value (List.tl prog_args) ~default:[] in
    if not (List.is_empty args) then
      let arg_string =
        if String.equal !args_to_export "" then encode_argv_to_env args
        else !args_to_export ^ String.of_char env_var_sep ^ encode_argv_to_env args
      in
      args_to_export := arg_string
  in
  (* read .inferconfig first, then env vars, then command-line options *)
  parse_args ~usage InferCommand inferconfig_args |> ignore ;
  (* NOTE: do not add the contents of .inferconfig to INFER_ARGS. This helps avoid hitting the
     command line size limit. *)
  parse_args ~usage InferCommand env_args |> ignore ;
  add_parsed_args_to_args_to_export () ;
  let curr_usage =
    let cl_args = match Array.to_list Sys.argv with _ :: tl -> tl | [] -> [] in
    let curr_usage = parse_args ~usage action ?initial_command cl_args in
    add_parsed_args_to_args_to_export () ;
    curr_usage
  in
  let to_export =
    let argv_to_export = decode_env_to_argv !args_to_export in
    if argv_to_export <> [] then (
      (* We have to be careful not to add too much data to the environment because the size of the
         environment contributes to the length of the command to be run. If the environment + CLI is
         too big, running any command will fail with a cryptic "exit code 127" error. Use an argfile
         to prevent this from happening *)
      let file = Filename.temp_file "args" "" in
      Out_channel.with_file file ~f:(fun oc -> Out_channel.output_lines oc argv_to_export) ;
      if not !keep_args_file then Utils.unlink_file_on_exit file ;
      "@" ^ file )
    else ""
  in
  Unix.putenv ~key:args_env_var ~data:to_export ;
  (!curr_command, curr_usage)


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
      else len
    in
    let new_length = line_length + String.length word_sep_str + word_length in
    let new_non_empty = non_empty || word <> "" in
    if new_length > wrap_length && non_empty then
      (line :: rev_lines, true, indent_string ^ word, indent_length + word_length)
    else
      let sep = if Int.equal line_length indent_length then "" else word_sep_str in
      let new_line = line ^ sep ^ word in
      if new_length > wrap_length && new_non_empty then
        (new_line :: rev_lines, false, indent_string, indent_length)
      else (rev_lines, new_non_empty, new_line, new_length)
  in
  let rev_lines, _, line, _ = List.fold ~f:add_word_to_paragraph ~init:([], false, "", 0) words in
  List.rev (line :: rev_lines)


let show_manual ?internal_section format default_doc command_opt =
  let command_doc =
    match command_opt with
    | None ->
        default_doc
    | Some command -> (
      match List.Assoc.find_exn ~equal:InferCommand.equal !subcommands command with
      | Some command_doc, _, _ ->
          command_doc
      | None, _, _ ->
          L.(die InternalError) "No manual for internal command %s" (string_of_command command) )
  in
  let pp_meta f meta =
    match meta with "" -> () | meta -> F.fprintf f " $(i,%s)" (Cmdliner.Manpage.escape meta)
  in
  let pp_short f = function "" -> () | s -> Format.fprintf f ",$(b,-%s)" s in
  let block_of_desc {long; meta; short; doc} =
    if String.equal doc "" then []
    else
      let doc_first_line, doc_other_lines =
        match String.split ~on:'\n' doc with first :: other -> (first, other) | [] -> ("", [])
      in
      (* Cmdline.Manpage does not format multi-paragraph documentation strings correctly for `I
         blocks, so we do a bit of formatting by hand *)
      let indent_string = "    " in
      let width =
        77 (* Cmdliner.Manpage width limit it seems *)
        - 7
        (* base indentation of documentation strings *)
      in
      `I (Format.asprintf "$(b,%s)%a%a" (dashdash long) pp_short short pp_meta meta, doc_first_line)
      :: List.concat_map
           (List.concat_map ~f:(wrap_line indent_string width) doc_other_lines)
           ~f:(fun s -> [`Noblank; `Pre s])
  in
  let option_blocks =
    match command_doc.manual_options with
    | `Replace blocks ->
        `S Cmdliner.Manpage.s_options :: blocks
    | `Prepend blocks -> (
        let hidden =
          match internal_section with
          | Some section ->
              `S section :: `P "Use at your own risk."
              :: List.concat_map ~f:block_of_desc (normalize_desc_list !hidden_descs_list)
          | None ->
              []
        in
        match command_opt with
        | Some command ->
            let sections =
              List.Assoc.find_exn ~equal:InferCommand.equal help_sections_desc_lists command
            in
            SectionMap.fold
              (fun section descs result ->
                `S section
                :: (if String.equal section Cmdliner.Manpage.s_options then blocks else [])
                @ List.concat_map ~f:block_of_desc (normalize_desc_list descs)
                @ result )
              !sections hidden
        | None ->
            (`S Cmdliner.Manpage.s_options :: blocks)
            @ List.concat_map ~f:block_of_desc (normalize_desc_list !visible_descs_list)
            @ hidden )
  in
  let blocks =
    [ `Blocks command_doc.manual_before_options
    ; `Blocks option_blocks
    ; `Blocks command_doc.manual_after_options ]
  in
  Cmdliner.Manpage.print format Format.std_formatter (command_doc.title, blocks) ;
  ()
