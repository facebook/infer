type t = out_channel
type open_flag = open_flag =
  | Open_rdonly 
  | Open_wronly 
  | Open_append 
  | Open_creat 
  | Open_trunc 
  | Open_excl 
  | Open_binary 
  | Open_text 
  | Open_nonblock 
val stdout : t
val stderr : t
val open_bin : string -> t
val open_text : string -> t
val open_gen : open_flag list -> int -> string -> t
val with_open_bin : string -> (t -> 'a) -> 'a
val with_open_text : string -> (t -> 'a) -> 'a
val with_open_gen : open_flag list -> int -> string -> (t -> 'a) -> 'a
val seek : t -> int64 -> unit
val pos : t -> int64
val length : t -> int64
val close : t -> unit
val close_noerr : t -> unit
val flush : t -> unit
val flush_all : unit -> unit
val output_char : t -> char -> unit
val output_byte : t -> int -> unit
val output_string : t -> string -> unit
val output_bytes : t -> bytes -> unit
val output : t -> bytes -> int -> int -> unit
val output_substring : t -> string -> int -> int -> unit
val set_binary_mode : t -> bool -> unit
val set_buffered : t -> bool -> unit
val is_buffered : t -> bool
