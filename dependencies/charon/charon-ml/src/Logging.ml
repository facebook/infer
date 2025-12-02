module H = Easy_logging.Handlers
module L = Easy_logging.Logging

let _ = L.make_logger "MainLogger" Debug [ Cli Debug ]

(** The main logger *)
let main_log = L.get_logger "MainLogger"

(** Below, we create subgloggers for various submodules, so that we can
    precisely toggle logging on/off, depending on which information we need *)

(** Logger for LlbcOfJson *)
let llbc_of_json_logger = L.get_logger "MainLogger.LlbcOfJson"

(** Terminal colors - TODO: comes from easy_logging (did not manage to reuse the
    module directly) *)
type color =
  | Default
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | Gray
  | White
  | LRed
  | LGreen
  | LYellow
  | LBlue
  | LMagenta
  | LCyan
  | LGray

(** Terminal styles - TODO: comes from easy_logging (did not manage to reuse the
    module directly) *)
type format = Bold | Underline | Invert | Fg of color | Bg of color

(** TODO: comes from easy_logging (did not manage to reuse the module directly)
*)
let to_fg_code c =
  match c with
  | Default -> 39
  | Black -> 30
  | Red -> 31
  | Green -> 32
  | Yellow -> 33
  | Blue -> 34
  | Magenta -> 35
  | Cyan -> 36
  | Gray -> 90
  | White -> 97
  | LRed -> 91
  | LGreen -> 92
  | LYellow -> 93
  | LBlue -> 94
  | LMagenta -> 95
  | LCyan -> 96
  | LGray -> 37

(** TODO: comes from easy_logging (did not manage to reuse the module directly)
*)
let to_bg_code c =
  match c with
  | Default -> 49
  | Black -> 40
  | Red -> 41
  | Green -> 42
  | Yellow -> 43
  | Blue -> 44
  | Magenta -> 45
  | Cyan -> 46
  | Gray -> 100
  | White -> 107
  | LRed -> 101
  | LGreen -> 102
  | LYellow -> 103
  | LBlue -> 104
  | LMagenta -> 105
  | LCyan -> 106
  | LGray -> 47

(** TODO: comes from easy_logging (did not manage to reuse the module directly)
*)
let style_to_codes s =
  match s with
  | Bold -> (1, 21)
  | Underline -> (4, 24)
  | Invert -> (7, 27)
  | Fg c -> (to_fg_code c, to_fg_code Default)
  | Bg c -> (to_bg_code c, to_bg_code Default)

(** TODO: comes from easy_logging (did not manage to reuse the module directly)
    I made a minor modifications, though. *)
let level_to_color (lvl : L.level) =
  match lvl with
  | L.Flash -> LMagenta
  | Error -> LRed
  | Warning -> LYellow
  | Info -> LGreen
  | Trace -> Cyan
  | Debug -> LBlue
  | NoLevel -> Default

(** [format styles str] formats [str] to the given [styles] - TODO: comes from
    {{:http://ocamlverse.net/content/documentation_guidelines.html}[easy_logging]}
    (did not manage to reuse the module directly) *)
let rec format styles str =
  match styles with
  | (_ as s) :: styles' ->
      let set, reset = style_to_codes s in
      Printf.sprintf "\027[%dm%s\027[%dm" set (format styles' str) reset
  | [] -> str

(** TODO: comes from
    {{:http://ocamlverse.net/content/documentation_guidelines.html}[easy_logging]}
    (did not manage to reuse the module directly) *)
let format_tags (tags : string list) =
  match tags with
  | [] -> ""
  | _ ->
      let elems_str = String.concat " | " tags in
      "[" ^ elems_str ^ "] "

(* Reimplementing this to make minor modifications *)
let show_level (lvl : L.level) =
  match lvl with
  | Debug -> "Debug"
  | Trace -> "Trace"
  | Info -> "Info"
  | Warning -> "Warn"
  | Error -> "Error"
  | Flash -> "Flash"
  | NoLevel -> "NoLevel"

(* Change the formatters *)
let main_logger_handler =
  (* TODO: comes from easy_logging *)
  let formatter (item : L.log_item) : string =
    let item_level_fmt =
      format [ Fg (level_to_color item.level) ] (show_level item.level)
    and item_msg_fmt =
      match item.level with
      | Flash -> format [ Fg Black; Bg LMagenta ] item.msg
      | _ -> item.msg
    in

    Format.pp_set_max_indent Format.str_formatter 200;
    Format.sprintf "@[[%-15s] %s%s@]" item_level_fmt (format_tags item.tags)
      item_msg_fmt
  in
  (* There should be exactly one handler *)
  let handlers = main_log#get_handlers in
  List.iter (fun h -> H.set_formatter h formatter) handlers;
  match handlers with
  | [ handler ] -> handler
  | _ -> raise (Failure "Unexpected")

(*
 * Subloggers
 *)
let name_matcher_logger = L.get_logger "NameMatcher"
