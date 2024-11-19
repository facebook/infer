module Option : sig
  type 'a t = 'a option

  val map : ('a -> 'b) -> 'a option -> 'b option

  val equal : ('a -> 'b -> bool) -> 'a option -> 'b option -> bool

  val exists : ('a -> bool) -> 'a option -> bool

  val some : 'a -> 'a option

  val iter : ('a -> unit) -> 'a option -> unit

  val filter : ('a -> bool) -> 'a option -> 'a option
end

module Version : sig
  type t = {
      major : int;
      minor : int;
      patch : int;
    }

  val mk : int -> int -> int -> t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val hash : t -> int

  val of_string : string -> t

(*
  val of_command_line : string -> t
*)

  val to_string : ?sep:string -> ?include_patch:bool -> t -> string
end

val signature_of_in_channel :
    ?filename:string -> in_channel -> Parsetree.signature

(*
module Interpreter : sig
  type t = {
      command_line : string;
      version : Version.t;
    }

  val of_command_line : string -> t
end
*)

module Buffer : sig
  include module type of (struct include Buffer end)

  val add_channel_no_wait : Buffer.t -> in_channel -> int -> int

  val add_channel_to_the_end : ?chunk_size:int -> ?continue:(unit -> bool) ->
    Buffer.t -> in_channel -> unit

  val suffix_of_length : Buffer.t -> int -> string

  val has_suffix : Buffer.t -> string -> bool
end

module String : sig
  include module type of (struct include String end)

  val suffix_of_length : string -> int -> string

  val has_suffix : string -> suffix:string -> bool

  val prefix_of_length : string -> int -> string

  val has_prefix : string -> prefix:string -> bool

  val suffix_from : string -> int -> string
end
