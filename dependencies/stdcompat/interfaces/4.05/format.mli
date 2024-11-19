val open_box : int -> unit
val close_box : unit -> unit
val print_string : string -> unit
val print_as : int -> string -> unit
val print_int : int -> unit
val print_float : float -> unit
val print_char : char -> unit
val print_bool : bool -> unit
val print_space : unit -> unit
val print_cut : unit -> unit
val print_break : int -> int -> unit
val print_flush : unit -> unit
val print_newline : unit -> unit
val force_newline : unit -> unit
val print_if_newline : unit -> unit
val set_margin : int -> unit
val get_margin : unit -> int
val set_max_indent : int -> unit
val get_max_indent : unit -> int
val set_max_boxes : int -> unit
val get_max_boxes : unit -> int
val over_max_boxes : unit -> bool
val open_hbox : unit -> unit
val open_vbox : int -> unit
val open_hvbox : int -> unit
val open_hovbox : int -> unit
val set_ellipsis_text : string -> unit
val get_ellipsis_text : unit -> string
type tag = string
val open_tag : tag -> unit
val close_tag : unit -> unit
val set_tags : bool -> unit
val set_print_tags : bool -> unit
val set_mark_tags : bool -> unit
val get_print_tags : unit -> bool
val get_mark_tags : unit -> bool
val set_formatter_out_channel : out_channel -> unit
val set_formatter_output_functions :
  (string -> int -> int -> unit) -> (unit -> unit) -> unit
val get_formatter_output_functions :
  unit -> ((string -> int -> int -> unit) * (unit -> unit))
type formatter_out_functions =
  {
  out_string: string -> int -> int -> unit ;
  out_flush: unit -> unit ;
  out_newline: unit -> unit ;
  out_spaces: int -> unit }
val set_formatter_out_functions : formatter_out_functions -> unit
val get_formatter_out_functions : unit -> formatter_out_functions
type formatter_tag_functions =
  {
  mark_open_tag: tag -> string ;
  mark_close_tag: tag -> string ;
  print_open_tag: tag -> unit ;
  print_close_tag: tag -> unit }
val set_formatter_tag_functions : formatter_tag_functions -> unit
val get_formatter_tag_functions : unit -> formatter_tag_functions
type formatter
val formatter_of_out_channel : out_channel -> formatter
val std_formatter : formatter
val err_formatter : formatter
val formatter_of_buffer : Buffer.t -> formatter
val stdbuf : Buffer.t
val str_formatter : formatter
val flush_str_formatter : unit -> string
val make_formatter :
  (string -> int -> int -> unit) -> (unit -> unit) -> formatter
val pp_open_hbox : formatter -> unit -> unit
val pp_open_vbox : formatter -> int -> unit
val pp_open_hvbox : formatter -> int -> unit
val pp_open_hovbox : formatter -> int -> unit
val pp_open_box : formatter -> int -> unit
val pp_close_box : formatter -> unit -> unit
val pp_open_tag : formatter -> string -> unit
val pp_close_tag : formatter -> unit -> unit
val pp_print_string : formatter -> string -> unit
val pp_print_as : formatter -> int -> string -> unit
val pp_print_int : formatter -> int -> unit
val pp_print_float : formatter -> float -> unit
val pp_print_char : formatter -> char -> unit
val pp_print_bool : formatter -> bool -> unit
val pp_print_break : formatter -> int -> int -> unit
val pp_print_cut : formatter -> unit -> unit
val pp_print_space : formatter -> unit -> unit
val pp_force_newline : formatter -> unit -> unit
val pp_print_flush : formatter -> unit -> unit
val pp_print_newline : formatter -> unit -> unit
val pp_print_if_newline : formatter -> unit -> unit
val pp_set_tags : formatter -> bool -> unit
val pp_set_print_tags : formatter -> bool -> unit
val pp_set_mark_tags : formatter -> bool -> unit
val pp_get_print_tags : formatter -> unit -> bool
val pp_get_mark_tags : formatter -> unit -> bool
val pp_set_margin : formatter -> int -> unit
val pp_get_margin : formatter -> unit -> int
val pp_set_max_indent : formatter -> int -> unit
val pp_get_max_indent : formatter -> unit -> int
val pp_set_max_boxes : formatter -> int -> unit
val pp_get_max_boxes : formatter -> unit -> int
val pp_over_max_boxes : formatter -> unit -> bool
val pp_set_ellipsis_text : formatter -> string -> unit
val pp_get_ellipsis_text : formatter -> unit -> string
val pp_set_formatter_out_channel : formatter -> out_channel -> unit
val pp_set_formatter_output_functions :
  formatter -> (string -> int -> int -> unit) -> (unit -> unit) -> unit
val pp_get_formatter_output_functions :
  formatter -> unit -> ((string -> int -> int -> unit) * (unit -> unit))
val pp_set_formatter_tag_functions :
  formatter -> formatter_tag_functions -> unit
val pp_get_formatter_tag_functions :
  formatter -> unit -> formatter_tag_functions
val pp_set_formatter_out_functions :
  formatter -> formatter_out_functions -> unit
val pp_get_formatter_out_functions :
  formatter -> unit -> formatter_out_functions
val pp_flush_formatter : formatter -> unit
val pp_print_list :
  ?pp_sep:(formatter -> unit -> unit) ->
    (formatter -> 'a -> unit) -> formatter -> 'a list -> unit
val pp_print_text : formatter -> string -> unit
val fprintf : formatter -> ('a, formatter, unit) format -> 'a
val printf : ('a, formatter, unit) format -> 'a
val eprintf : ('a, formatter, unit) format -> 'a
val sprintf : ('a, unit, string) format -> 'a
val asprintf : ('a, formatter, unit, string) format4 -> 'a
val ifprintf : formatter -> ('a, formatter, unit) format -> 'a
val kfprintf :
  (formatter -> 'a) -> formatter -> ('b, formatter, unit, 'a) format4 -> 'b
val ikfprintf :
  (formatter -> 'a) -> formatter -> ('b, formatter, unit, 'a) format4 -> 'b
val ksprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b
val kasprintf : (string -> 'a) -> ('b, formatter, unit, 'a) format4 -> 'b
val bprintf : Buffer.t -> ('a, formatter, unit) format -> 'a[@@ocaml.deprecated
                                                              "- : Buffer.t -> ('a, Format.formatter, unit) format -> 'a = <fun>"]
val kprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b[@@ocaml.deprecated
                                                                    "Use Format.ksprintf instead."]
val set_all_formatter_output_functions :
  out:(string -> int -> int -> unit) ->
    flush:(unit -> unit) ->
      newline:(unit -> unit) -> spaces:(int -> unit) -> unit[@@ocaml.deprecated
                                                              "Use Format.set_formatter_out_functions instead."]
val get_all_formatter_output_functions :
  unit ->
    ((string -> int -> int -> unit) * (unit -> unit) * (unit -> unit) *
      (int -> unit))[@@ocaml.deprecated
                      "Use Format.get_formatter_out_functions instead."]
val pp_set_all_formatter_output_functions :
  formatter ->
    out:(string -> int -> int -> unit) ->
      flush:(unit -> unit) ->
        newline:(unit -> unit) -> spaces:(int -> unit) -> unit[@@ocaml.deprecated
                                                                "Use Format.pp_set_formatter_out_functions instead."]
val pp_get_all_formatter_output_functions :
  formatter ->
    unit ->
      ((string -> int -> int -> unit) * (unit -> unit) * (unit -> unit) *
        (int -> unit))[@@ocaml.deprecated
                        "Use Format.pp_get_formatter_out_functions instead."]
val pp_open_tbox : formatter -> unit -> unit[@@ocaml.deprecated
                                              "Tabulation boxes are not supported any more."]
val pp_close_tbox : formatter -> unit -> unit[@@ocaml.deprecated
                                               "Tabulation boxes are not supported any more."]
val pp_print_tbreak : formatter -> int -> int -> unit[@@ocaml.deprecated
                                                       "Tabulation boxes are not supported any more."]
val pp_set_tab : formatter -> unit -> unit[@@ocaml.deprecated
                                            "Tabulation boxes are not supported any more."]
val pp_print_tab : formatter -> unit -> unit[@@ocaml.deprecated
                                              "Tabulation boxes are not supported any more."]
val open_tbox : unit -> unit[@@ocaml.deprecated
                              "Tabulation boxes are not supported any more."]
val close_tbox : unit -> unit[@@ocaml.deprecated
                               "Tabulation boxes are not supported any more."]
val print_tbreak : int -> int -> unit[@@ocaml.deprecated
                                       "Tabulation boxes are not supported any more."]
val set_tab : unit -> unit[@@ocaml.deprecated
                            "Tabulation boxes are not supported any more."]
val print_tab : unit -> unit[@@ocaml.deprecated
                              "Tabulation boxes are not supported any more."]
