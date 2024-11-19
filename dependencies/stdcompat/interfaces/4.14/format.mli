type formatter
val pp_open_box : formatter -> int -> unit
val open_box : int -> unit
val pp_close_box : formatter -> unit -> unit
val close_box : unit -> unit
val pp_open_hbox : formatter -> unit -> unit
val open_hbox : unit -> unit
val pp_open_vbox : formatter -> int -> unit
val open_vbox : int -> unit
val pp_open_hvbox : formatter -> int -> unit
val open_hvbox : int -> unit
val pp_open_hovbox : formatter -> int -> unit
val open_hovbox : int -> unit
val pp_print_string : formatter -> string -> unit
val print_string : string -> unit
val pp_print_bytes : formatter -> bytes -> unit
val print_bytes : bytes -> unit
val pp_print_as : formatter -> int -> string -> unit
val print_as : int -> string -> unit
val pp_print_int : formatter -> int -> unit
val print_int : int -> unit
val pp_print_float : formatter -> float -> unit
val print_float : float -> unit
val pp_print_char : formatter -> char -> unit
val print_char : char -> unit
val pp_print_bool : formatter -> bool -> unit
val print_bool : bool -> unit
val pp_print_space : formatter -> unit -> unit
val print_space : unit -> unit
val pp_print_cut : formatter -> unit -> unit
val print_cut : unit -> unit
val pp_print_break : formatter -> int -> int -> unit
val print_break : int -> int -> unit
val pp_print_custom_break :
  formatter ->
    fits:(string * int * string) -> breaks:(string * int * string) -> unit
val pp_force_newline : formatter -> unit -> unit
val force_newline : unit -> unit
val pp_print_if_newline : formatter -> unit -> unit
val print_if_newline : unit -> unit
val pp_print_flush : formatter -> unit -> unit
val print_flush : unit -> unit
val pp_print_newline : formatter -> unit -> unit
val print_newline : unit -> unit
val pp_set_margin : formatter -> int -> unit
val set_margin : int -> unit
val pp_get_margin : formatter -> unit -> int
val get_margin : unit -> int
val pp_set_max_indent : formatter -> int -> unit
val set_max_indent : int -> unit
val pp_get_max_indent : formatter -> unit -> int
val get_max_indent : unit -> int
type geometry = {
  max_indent: int ;
  margin: int }
val check_geometry : geometry -> bool
val pp_set_geometry : formatter -> max_indent:int -> margin:int -> unit
val set_geometry : max_indent:int -> margin:int -> unit
val pp_safe_set_geometry : formatter -> max_indent:int -> margin:int -> unit
val safe_set_geometry : max_indent:int -> margin:int -> unit
val pp_update_geometry : formatter -> (geometry -> geometry) -> unit
val update_geometry : (geometry -> geometry) -> unit
val pp_get_geometry : formatter -> unit -> geometry
val get_geometry : unit -> geometry
val pp_set_max_boxes : formatter -> int -> unit
val set_max_boxes : int -> unit
val pp_get_max_boxes : formatter -> unit -> int
val get_max_boxes : unit -> int
val pp_over_max_boxes : formatter -> unit -> bool
val over_max_boxes : unit -> bool
val pp_open_tbox : formatter -> unit -> unit
val open_tbox : unit -> unit
val pp_close_tbox : formatter -> unit -> unit
val close_tbox : unit -> unit
val pp_set_tab : formatter -> unit -> unit
val set_tab : unit -> unit
val pp_print_tab : formatter -> unit -> unit
val print_tab : unit -> unit
val pp_print_tbreak : formatter -> int -> int -> unit
val print_tbreak : int -> int -> unit
val pp_set_ellipsis_text : formatter -> string -> unit
val set_ellipsis_text : string -> unit
val pp_get_ellipsis_text : formatter -> unit -> string
val get_ellipsis_text : unit -> string
type stag = ..
type tag = string
type stag +=  
  | String_tag of tag 
val pp_open_stag : formatter -> stag -> unit
val open_stag : stag -> unit
val pp_close_stag : formatter -> unit -> unit
val close_stag : unit -> unit
val pp_set_tags : formatter -> bool -> unit
val set_tags : bool -> unit
val pp_set_print_tags : formatter -> bool -> unit
val set_print_tags : bool -> unit
val pp_set_mark_tags : formatter -> bool -> unit
val set_mark_tags : bool -> unit
val pp_get_print_tags : formatter -> unit -> bool
val get_print_tags : unit -> bool
val pp_get_mark_tags : formatter -> unit -> bool
val get_mark_tags : unit -> bool
val pp_set_formatter_out_channel : formatter -> out_channel -> unit
val set_formatter_out_channel : out_channel -> unit
val pp_set_formatter_output_functions :
  formatter -> (string -> int -> int -> unit) -> (unit -> unit) -> unit
val set_formatter_output_functions :
  (string -> int -> int -> unit) -> (unit -> unit) -> unit
val pp_get_formatter_output_functions :
  formatter -> unit -> ((string -> int -> int -> unit) * (unit -> unit))
val get_formatter_output_functions :
  unit -> ((string -> int -> int -> unit) * (unit -> unit))
type formatter_out_functions =
  {
  out_string: string -> int -> int -> unit ;
  out_flush: unit -> unit ;
  out_newline: unit -> unit ;
  out_spaces: int -> unit ;
  out_indent: int -> unit }
val pp_set_formatter_out_functions :
  formatter -> formatter_out_functions -> unit
val set_formatter_out_functions : formatter_out_functions -> unit
val pp_get_formatter_out_functions :
  formatter -> unit -> formatter_out_functions
val get_formatter_out_functions : unit -> formatter_out_functions
type formatter_stag_functions =
  {
  mark_open_stag: stag -> string ;
  mark_close_stag: stag -> string ;
  print_open_stag: stag -> unit ;
  print_close_stag: stag -> unit }
val pp_set_formatter_stag_functions :
  formatter -> formatter_stag_functions -> unit
val set_formatter_stag_functions : formatter_stag_functions -> unit
val pp_get_formatter_stag_functions :
  formatter -> unit -> formatter_stag_functions
val get_formatter_stag_functions : unit -> formatter_stag_functions
val formatter_of_out_channel : out_channel -> formatter
val std_formatter : formatter
val err_formatter : formatter
val formatter_of_buffer : Buffer.t -> formatter
val stdbuf : Buffer.t
val str_formatter : formatter
val flush_str_formatter : unit -> string
val make_formatter :
  (string -> int -> int -> unit) -> (unit -> unit) -> formatter
val formatter_of_out_functions : formatter_out_functions -> formatter
type symbolic_output_item =
  | Output_flush 
  | Output_newline 
  | Output_string of string 
  | Output_spaces of int 
  | Output_indent of int 
type symbolic_output_buffer
val make_symbolic_output_buffer : unit -> symbolic_output_buffer
val clear_symbolic_output_buffer : symbolic_output_buffer -> unit
val get_symbolic_output_buffer :
  symbolic_output_buffer -> symbolic_output_item list
val flush_symbolic_output_buffer :
  symbolic_output_buffer -> symbolic_output_item list
val add_symbolic_output_item :
  symbolic_output_buffer -> symbolic_output_item -> unit
val formatter_of_symbolic_output_buffer : symbolic_output_buffer -> formatter
val pp_print_list :
  ?pp_sep:(formatter -> unit -> unit) ->
    (formatter -> 'a -> unit) -> formatter -> 'a list -> unit
val pp_print_seq :
  ?pp_sep:(formatter -> unit -> unit) ->
    (formatter -> 'a -> unit) -> formatter -> 'a Seq.t -> unit
val pp_print_text : formatter -> string -> unit
val pp_print_option :
  ?none:(formatter -> unit -> unit) ->
    (formatter -> 'a -> unit) -> formatter -> 'a option -> unit
val pp_print_result :
  ok:(formatter -> 'a -> unit) ->
    error:(formatter -> 'e -> unit) -> formatter -> ('a, 'e) result -> unit
val pp_print_either :
  left:(formatter -> 'a -> unit) ->
    right:(formatter -> 'b -> unit) -> formatter -> ('a, 'b) Either.t -> unit
val fprintf : formatter -> ('a, formatter, unit) format -> 'a
val printf : ('a, formatter, unit) format -> 'a
val eprintf : ('a, formatter, unit) format -> 'a
val sprintf : ('a, unit, string) format -> 'a
val asprintf : ('a, formatter, unit, string) format4 -> 'a
val dprintf : ('a, formatter, unit, formatter -> unit) format4 -> 'a
val ifprintf : formatter -> ('a, formatter, unit) format -> 'a
val kfprintf :
  (formatter -> 'a) -> formatter -> ('b, formatter, unit, 'a) format4 -> 'b
val kdprintf :
  ((formatter -> unit) -> 'a) -> ('b, formatter, unit, 'a) format4 -> 'b
val ikfprintf :
  (formatter -> 'a) -> formatter -> ('b, formatter, unit, 'a) format4 -> 'b
val ksprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b
val kasprintf : (string -> 'a) -> ('b, formatter, unit, 'a) format4 -> 'b
val bprintf : Buffer.t -> ('a, formatter, unit) format -> 'a
val kprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b
val set_all_formatter_output_functions :
  out:(string -> int -> int -> unit) ->
    flush:(unit -> unit) ->
      newline:(unit -> unit) -> spaces:(int -> unit) -> unit
val get_all_formatter_output_functions :
  unit ->
    ((string -> int -> int -> unit) * (unit -> unit) * (unit -> unit) *
      (int -> unit))
val pp_set_all_formatter_output_functions :
  formatter ->
    out:(string -> int -> int -> unit) ->
      flush:(unit -> unit) ->
        newline:(unit -> unit) -> spaces:(int -> unit) -> unit
val pp_get_all_formatter_output_functions :
  formatter ->
    unit ->
      ((string -> int -> int -> unit) * (unit -> unit) * (unit -> unit) *
        (int -> unit))
val pp_open_tag : formatter -> tag -> unit
val open_tag : tag -> unit
val pp_close_tag : formatter -> unit -> unit
val close_tag : unit -> unit
type formatter_tag_functions =
  {
  mark_open_tag: tag -> string ;
  mark_close_tag: tag -> string ;
  print_open_tag: tag -> unit ;
  print_close_tag: tag -> unit }
val pp_set_formatter_tag_functions :
  formatter -> formatter_tag_functions -> unit
val set_formatter_tag_functions : formatter_tag_functions -> unit
val pp_get_formatter_tag_functions :
  formatter -> unit -> formatter_tag_functions
val get_formatter_tag_functions : unit -> formatter_tag_functions
