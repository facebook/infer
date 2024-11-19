val fprintf : out_channel -> ('a, out_channel, unit) format -> 'a
val printf : ('a, out_channel, unit) format -> 'a
val eprintf : ('a, out_channel, unit) format -> 'a
val sprintf : ('a, unit, string) format -> 'a
val bprintf : Buffer.t -> ('a, Buffer.t, unit) format -> 'a
val ifprintf : 'a -> ('b, 'a, unit) format -> 'b
val kfprintf :
  (out_channel -> 'a) ->
    out_channel -> ('b, out_channel, unit, 'a) format4 -> 'b
val ikfprintf :
  (out_channel -> 'a) ->
    out_channel -> ('b, out_channel, unit, 'a) format4 -> 'b
val ksprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b
val kbprintf :
  (Buffer.t -> 'a) -> Buffer.t -> ('b, Buffer.t, unit, 'a) format4 -> 'b
val kprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b
module CamlinternalPr :
sig
  module Sformat :
  sig
    type index
    val index_of_int : int -> index
    external int_of_index : index -> int = "%identity"
    external unsafe_index_of_int : int -> index = "%identity"
    val succ_index : index -> index
    val add_int_index : int -> index -> index
    val sub : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> index -> int -> string
    val to_string : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> string
    external length :
      ('a, 'b, 'c, 'd, 'e, 'f) format6 -> int = "%string_length"
    external get :
      ('a, 'b, 'c, 'd, 'e, 'f) format6 -> int -> char = "%string_safe_get"
    external unsafe_to_string :
      ('a, 'b, 'c, 'd, 'e, 'f) format6 -> string = "%identity"
    external unsafe_get :
      ('a, 'b, 'c, 'd, 'e, 'f) format6 -> int -> char = "%string_unsafe_get"
  end
  module Tformat :
  sig
    type ac =
      {
      mutable ac_rglr: int ;
      mutable ac_skip: int ;
      mutable ac_rdrs: int }
    val ac_of_format : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> ac
    val count_printing_arguments_of_format :
      ('a, 'b, 'c, 'd, 'e, 'f) format6 -> int
    val sub_format :
      (('a, 'b, 'c, 'd, 'e, 'f) format6 -> int) ->
        (('a, 'b, 'c, 'd, 'e, 'f) format6 -> int -> char -> int) ->
          char -> ('a, 'b, 'c, 'd, 'e, 'f) format6 -> int -> int
    val summarize_format_type : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> string
    val scan_format :
      ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
        'g array ->
          Sformat.index ->
            int ->
              (Sformat.index -> string -> int -> 'h) ->
                (Sformat.index -> 'i -> 'j -> int -> 'h) ->
                  (Sformat.index -> 'k -> int -> 'h) ->
                    (Sformat.index -> int -> 'h) ->
                      (Sformat.index ->
                         ('l, 'm, 'n, 'o, 'p, 'q) format6 -> int -> 'h)
                        -> 'h
    val kapr :
      (('a, 'b, 'c, 'd, 'e, 'f) format6 -> Obj.t array -> 'g) ->
        ('a, 'b, 'c, 'd, 'e, 'f) format6 -> 'g
  end
end
