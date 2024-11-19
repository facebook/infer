type extern_flags =
  | No_sharing 
  | Closures 
val to_channel : out_channel -> 'a -> extern_flags list -> unit
external to_string :
  'a -> extern_flags list -> string = "output_value_to_string"
val to_buffer : string -> int -> int -> 'a -> extern_flags list -> int
val from_channel : in_channel -> 'a
val from_string : string -> int -> 'a
val header_size : int
val data_size : string -> int -> int
val total_size : string -> int -> int
