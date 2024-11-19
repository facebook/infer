type spec =
  | Unit of (unit -> unit) 
  | Bool of (bool -> unit) 
  | Set of bool ref 
  | Clear of bool ref 
  | String of (string -> unit) 
  | Set_string of string ref 
  | Int of (int -> unit) 
  | Set_int of int ref 
  | Float of (float -> unit) 
  | Set_float of float ref 
  | Tuple of spec list 
  | Symbol of string list * (string -> unit) 
  | Rest of (string -> unit) 
and key = string
and doc = string
and usage_msg = string
and anon_fun = string -> unit
val parse : (key * spec * doc) list -> anon_fun -> usage_msg -> unit
val parse_argv :
  ?current:int ref ->
    string array -> (key * spec * doc) list -> anon_fun -> usage_msg -> unit
exception Help of string 
exception Bad of string 
val usage : (key * spec * doc) list -> usage_msg -> unit
val current : int ref
