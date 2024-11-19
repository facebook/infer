val fprintf : out_channel -> ('a, out_channel, unit) format -> 'a
val printf : ('a, out_channel, unit) format -> 'a
val eprintf : ('a, out_channel, unit) format -> 'a
val sprintf : ('a, unit, string) format -> 'a
val bprintf : Buffer.t -> ('a, Buffer.t, unit) format -> 'a
val kfprintf :
  (out_channel -> 'a) ->
    out_channel -> ('b, out_channel, unit, 'a) format4 -> 'b
val ksprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b
val kprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b
type index
external index_of_int : int -> index = "%identity"
val scan_format :
  string ->
    'a array ->
      index ->
        int ->
          (index -> string -> int -> 'b) ->
            (index -> 'c -> 'd -> int -> 'b) ->
              (index -> 'e -> int -> 'b) ->
                (index -> int -> 'b) ->
                  (index -> ('f, 'g, 'h, 'i) format4 -> int -> 'b) -> 'b
val sub_format :
  (string -> int) ->
    (string -> int -> char -> int) -> char -> string -> int -> int
val summarize_format_type : string -> string
val kapr : (string -> Obj.t array -> 'a) -> string -> 'a
