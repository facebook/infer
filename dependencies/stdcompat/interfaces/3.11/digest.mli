type t = string
val string : string -> t
val substring : string -> int -> int -> t
external channel : in_channel -> int -> t = "caml_md5_chan"
val file : string -> t
val output : out_channel -> t -> unit
val input : in_channel -> t
val to_hex : t -> string
