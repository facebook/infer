val fprintf : out_channel -> ('a, out_channel, unit) format -> 'a
val printf : ('a, out_channel, unit) format -> 'a
val eprintf : ('a, out_channel, unit) format -> 'a
val sprintf : ('a, unit, string) format -> 'a
val bprintf : Buffer.t -> ('a, Buffer.t, unit) format -> 'a
val kprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b
val scan_format :
  string ->
    int ->
      (string -> int -> 'a) ->
        ('b -> 'c -> int -> 'a) -> ('d -> int -> 'a) -> (int -> 'a) -> 'a
