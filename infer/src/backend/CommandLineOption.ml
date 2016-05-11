(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Definition and parsing of command line arguments *)

open! Utils

module F = Format


(* Each command line option may appear in the --help list of any executable, these tags are used to
   specify which executables for which an option will be documented. *)
type exe = A | C | J | L | P | T

let current_exe =
  match Filename.basename Sys.executable_name with
  | "InferAnalyze" -> A
  | "InferClang" -> C
  | "InferJava" -> J
  | "InferLLVM" -> L
  | "InferPrint" -> P
  | _ -> T


type desc = { long: string; short: string; meta: string; doc: string; spec: Arg.spec }

let dashdash long =
  match long with
  | "" | "--" -> long
  | _ -> "--" ^ long

let short_meta {short; meta; spec} =
  String.concat " "
    ((if short = "" then [] else ["| -" ^ short]) @
     (match spec with
      | Arg.Symbol (symbols, _) ->
          ["{ " ^ (String.concat " | " symbols) ^ " }" ^ meta]
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
                                arg (dashdash long) (String.concat " | " symbols)))
          )
    | _ ->
        spec
  in
  (key long short, xspec long spec, doc)

let pad_and_xform left_width desc =
  match desc with
  | {doc = ""} ->
      xdesc desc
  | {long; doc} ->
      let short_meta = short_meta desc in
      let gap = left_width - (left_length long short_meta) in
      if gap < 0 then
        xdesc {desc with doc = short_meta ^ "\n" ^ (String.make (4 + left_width) ' ') ^ doc}
      else
        xdesc {desc with doc = short_meta ^ (String.make (gap + 1) ' ') ^ doc}

let align ?(limit=max_int) desc_list =
  let left_width = IList.fold_left (max_left_length limit) 0 desc_list in
  (IList.map (pad_and_xform left_width) desc_list)


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

let exe_desc_lists = [(A, ref []); (C, ref []); (J, ref []); (L, ref []); (P, ref []); (T, ref [])]

(* add desc to all desc_lists for the purposes of parsing, include desc in --help only for exes *)
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
    ~long ?(short="") ~default ~meta doc ~default_to_string ~mk_setter ~mk_spec =
  let variable = ref default in
  let closure = mk_setter variable in
  let setter str =
    try closure str
    with _ -> raise (Arg.Bad ("bad value " ^ str ^ " for flag " ^ long)) in
  let spec = mk_spec setter in
  let doc =
    let default_string = default_to_string default in
    if default_string = "" then doc
    else doc ^ " (default: " ^ default_string ^ ")" in
  let desc = {long; short; meta; doc; spec} in
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

type 'a t =
  ?deprecated:string list -> long:Arg.key -> ?short:Arg.key ->
  ?exes:exe list -> ?meta:string -> Arg.doc ->
  'a

let mk_set var value ?(deprecated=[]) ~long ?short ?exes ?(meta="") doc =
  let setter () = var := value in
  ignore(
    mk ~deprecated ~long ?short ~default:() ?exes ~meta doc
      ~default_to_string:(fun () -> "")
      ~mk_setter:(fun _ _ -> setter ())
      ~mk_spec:(fun _ -> Arg.Unit setter) )

let mk_option ?(default=None) ?(default_to_string=fun _ -> "") ~f
    ?(deprecated=[]) ~long ?short ?exes ?(meta="") doc =
  mk ~deprecated ~long ?short ~default ?exes ~meta doc
    ~default_to_string
    ~mk_setter:(fun var str -> var := f str)
    ~mk_spec:(fun set -> Arg.String set)

let mk_bool ?(deprecated_no=[]) ?(default=false) ?(f=fun b -> b)
    ?(deprecated=[]) ~long ?short ?exes ?(meta="") doc =
  let nolong =
    let len = String.length long in
    if len > 3 && String.sub long 0 3 = "no-" then
      String.sub long 3 (len - 3)
    else
      "no-" ^ long
  and noshort =
    Option.map (fun short ->
        let len = String.length short in
        if len > 1 && String.sub short 0 1 = "n" then
          String.sub short 1 (len - 1)
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
      ~meta doc ~default_to_string ~mk_setter:(fun var _ -> var := f true) ~mk_spec in
  ignore(
    mk ~long:nolong ?short:noshort ~deprecated:deprecated_no ~default:(not default) ?exes
      ~meta nodoc ~default_to_string ~mk_setter:(fun _ _ -> var := f false) ~mk_spec );
  var

let mk_bool_group ?(deprecated_no=[]) ?(default=false)
    ?(deprecated=[]) ~long ?short ?exes ?(meta="") doc children =
  let f b =
    IList.iter (fun child -> child := b) children ;
    b
  in
  mk_bool ~deprecated ~deprecated_no ~default ~long ?short ~f ?exes ~meta doc

let mk_int ~default ?(deprecated=[]) ~long ?short ?exes ?(meta="") doc =
  mk ~deprecated ~long ?short ~default ?exes ~meta doc
    ~default_to_string:string_of_int
    ~mk_setter:(fun var str -> var := (int_of_string str))
    ~mk_spec:(fun set -> Arg.String set)

let mk_float ~default ?(deprecated=[]) ~long ?short ?exes ?(meta="") doc =
  mk ~deprecated ~long ?short ~default ?exes ~meta doc
    ~default_to_string:string_of_float
    ~mk_setter:(fun var str -> var := (float_of_string str))
    ~mk_spec:(fun set -> Arg.String set)

let mk_string ~default ?(f=fun s -> s) ?(deprecated=[]) ~long ?short ?exes ?(meta="") doc =
  mk ~deprecated ~long ?short ~default ?exes ~meta doc
    ~default_to_string:(fun s -> s)
    ~mk_setter:(fun var str -> var := f str)
    ~mk_spec:(fun set -> Arg.String set)

let mk_string_opt ?default ?(f=fun s -> s) ?(deprecated=[]) ~long ?short ?exes ?(meta="") doc =
  let default_to_string = function Some s -> s | None -> "" in
  let f s = Some (f s) in
  mk_option ~deprecated ~long ?short ~default ~default_to_string ~f ?exes ~meta doc

let mk_string_list ?(default=[]) ?(f=fun s -> s)
    ?(deprecated=[]) ~long ?short ?exes ?(meta="") doc =
  mk ~deprecated ~long ?short ~default ?exes ~meta doc
    ~default_to_string:(String.concat ", ")
    ~mk_setter:(fun var str -> var := (f str) :: !var)
    ~mk_spec:(fun set -> Arg.String set)

let mk_symbol ~default ~symbols ?(deprecated=[]) ~long ?short ?exes ?(meta="") doc =
  let strings = IList.map fst symbols in
  let sym_to_str = IList.map (fun (x,y) -> (y,x)) symbols in
  let of_string str = IList.assoc string_equal str symbols in
  let to_string sym = IList.assoc ( = ) sym sym_to_str in
  mk ~deprecated ~long ?short ~default ?exes ~meta doc
    ~default_to_string:(fun s -> to_string s)
    ~mk_setter:(fun var str -> var := of_string str)
    ~mk_spec:(fun set -> Arg.Symbol (strings, set))

let mk_symbol_opt ~symbols ?(deprecated=[]) ~long ?short ?exes ?(meta="") doc =
  let strings = IList.map fst symbols in
  let of_string str = IList.assoc string_equal str symbols in
  mk ~deprecated ~long ?short ~default:None ?exes ~meta doc
    ~default_to_string:(fun _ -> "")
    ~mk_setter:(fun var str -> var := Some (of_string str))
    ~mk_spec:(fun set -> Arg.Symbol (strings, set))

let mk_symbol_seq ?(default=[]) ~symbols ?(deprecated=[]) ~long ?short ?exes ?(meta="") doc =
  let strings = IList.map fst symbols in
  let sym_to_str = IList.map (fun (x,y) -> (y,x)) symbols in
  let of_string str = IList.assoc string_equal str symbols in
  let to_string sym = IList.assoc ( = ) sym sym_to_str in
  mk ~deprecated ~long ?short ~default ?exes ~meta:(" ,-separated sequence" ^ meta) doc
    ~default_to_string:(fun syms -> String.concat " " (IList.map to_string syms))
    ~mk_setter:(fun var str_seq -> var := IList.map of_string (Str.split (Str.regexp ",") str_seq))
    ~mk_spec:(fun set -> Arg.Symbol (strings, set))

let anon_fun = ref (fun arg -> raise (Arg.Bad ("unexpected anonymous argument: " ^ arg)))

let mk_anon () =
  let anon = ref [] in
  anon_fun := (fun arg -> anon := arg :: !anon) ;
  anon



(** [sep_char] is used to separate elements of argv when encoded into environment variables *)
let sep_char = '^'

let encode_argv_to_env argv =
  String.concat (String.make 1 sep_char)
    (IList.filter (fun arg ->
         not (String.contains arg sep_char)
         || (
           F.eprintf "Ignoring unsupported option containing '%c' character: %s@\n" sep_char arg ;
           false
         )
       ) argv)

let decode_env_to_argv env =
  Str.split (Str.regexp_string (String.make 1 sep_char)) env

let prepend_to_argv args =
  let cl_args = match Array.to_list Sys.argv with _ :: tl -> tl | [] -> [] in
  (Sys.executable_name, args @ cl_args)

(** [prefix_before_rest (prefix @ ["--" :: rest])] is [prefix] where "--" is not in [prefix]. *)
let prefix_before_rest args =
  let rec prefix_before_rest_ rev_keep = function
    | [] | "--" :: _ -> IList.rev rev_keep
    | keep :: args -> prefix_before_rest_ (keep :: rev_keep) args in
  prefix_before_rest_ [] args


let parse env_var exe_usage =
  let curr_speclist = ref []
  and full_speclist = ref []
  in
  let usage_msg = exe_usage current_exe
  in
  let curr_usage status =
    Arg.usage !curr_speclist usage_msg ;
    exit status
  and full_usage status =
    Arg.usage !full_speclist usage_msg ;
    exit status
  in
  let help_desc_list =
    [ { long = "help"; short = ""; meta = ""; spec = Arg.Unit (fun () -> curr_usage 0);
        doc = "Display this list of options" }
    ; { long = "help-full"; short = ""; meta = ""; spec = Arg.Unit (fun () -> full_usage 0);
        doc = "Display the full list of options, including internal and experimental options" }
    ] in
  let section heading speclist =
    let norm k =
      let len = String.length k in
      if len > 3 && String.sub k 0 3 = "no-" then String.sub k 3 (len - 3) else k in
    let compare_specs {long = x} {long = y} =
      match x, y with
      | "--", "--" -> 0
      | "--", _ -> 1
      | _, "--" -> -1
      | _ -> String.compare (norm x) (norm y) in
    let sort speclist = IList.sort compare_specs speclist in
    let add_heading speclist =
      match heading with
      | Some heading ->
          let doc = "\n  " ^ heading ^ "\n" in
          { doc; long = ""; short = ""; meta = ""; spec = Arg.Unit (fun () -> ()) } :: speclist
      | None ->
          speclist in
    let suppress_help speclist =
      ("-help", Arg.Unit (fun () -> raise (Arg.Bad "unknown option '-help'")), "") :: speclist in
    suppress_help (align ~limit:32 (add_heading (sort speclist)))
  in
  let curr_desc_list = IList.assoc ( = ) current_exe exe_desc_lists
  in
  (* curr_speclist includes args for current exe with docs, and all other args without docs, so
     that all args can be parsed, but --help and parse failures only show external args for
     current exe *)
  curr_speclist := (section None (help_desc_list @ !curr_desc_list))
  ;
  assert( check_no_duplicates !curr_speclist )
  ;
  full_speclist := (section None (help_desc_list @ !full_desc_list))
  ;
  let env_args = decode_env_to_argv (try Unix.getenv env_var with Not_found -> "") in
  (* begin transitional support for INFERCLANG_ARGS *)
  let c_args =
    Str.split (Str.regexp_string (String.make 1 ':'))
      (try Unix.getenv "INFERCLANG_ARGS" with Not_found -> "") in
  let env_args = c_args @ env_args in
  (* end transitional support for INFERCLANG_ARGS *)
  let exe_name, env_cl_args = prepend_to_argv env_args in
  Unix.putenv env_var (encode_argv_to_env (prefix_before_rest env_cl_args)) ;
  (try
     Arg.parse_argv (Array.of_list (exe_name :: env_cl_args)) !curr_speclist !anon_fun usage_msg
   with
   | Arg.Bad usage_msg -> Pervasives.prerr_string usage_msg; exit 2
   | Arg.Help usage_msg -> Pervasives.print_string usage_msg; exit 0
  );
  curr_usage
