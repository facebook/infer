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
