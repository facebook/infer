type t = string
val compare : t -> t -> int
val string : string -> t
val bytes : bytes -> t
val substring : string -> int -> int -> t
val subbytes : bytes -> int -> int -> t
external channel : in_channel -> int -> t = "caml_md5_chan"
val file : string -> t
val output : out_channel -> t -> unit
val input : in_channel -> t
val to_hex : t -> string
val from_hex : string -> t
