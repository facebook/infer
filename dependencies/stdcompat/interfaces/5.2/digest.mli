type t = string
val compare : t -> t -> int
val equal : t -> t -> bool
val string : string -> t
val bytes : bytes -> t
val substring : string -> int -> int -> t
val subbytes : bytes -> int -> int -> t
val channel : in_channel -> int -> t
val file : string -> t
val output : out_channel -> t -> unit
val input : in_channel -> t
val to_hex : t -> string
val of_hex : string -> t
val from_hex : string -> t
module type S  =
  sig
    type t = string
    val hash_length : int
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val string : string -> t
    val bytes : bytes -> t
    val substring : string -> int -> int -> t
    val subbytes : bytes -> int -> int -> t
    val channel : in_channel -> int -> t
    val file : string -> t
    val output : out_channel -> t -> unit
    val input : in_channel -> t
    val to_hex : t -> string
    val of_hex : string -> t
  end
module BLAKE128 : S
module BLAKE256 : S
module BLAKE512 : S
module MD5 : S
