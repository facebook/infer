val fprintf : out_channel -> ('a, out_channel, unit) format -> 'a
val printf : ('a, out_channel, unit) format -> 'a
val eprintf : ('a, out_channel, unit) format -> 'a
val sprintf : ('a, unit, string) format -> 'a
val bprintf : Buffer.t -> ('a, Buffer.t, unit) format -> 'a
val ifprintf : 'b -> ('a, 'b, 'c, unit) format4 -> 'a
val kfprintf :
  (out_channel -> 'd) ->
    out_channel -> ('a, out_channel, unit, 'd) format4 -> 'a
val ikfprintf : ('b -> 'd) -> 'b -> ('a, 'b, 'c, 'd) format4 -> 'a
val ksprintf : (string -> 'd) -> ('a, unit, string, 'd) format4 -> 'a
val kbprintf :
  (Buffer.t -> 'd) -> Buffer.t -> ('a, Buffer.t, unit, 'd) format4 -> 'a
val kprintf : (string -> 'b) -> ('a, unit, string, 'b) format4 -> 'a
