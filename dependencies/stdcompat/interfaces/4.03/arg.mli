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
type key = string
type doc = string
type usage_msg = string
type anon_fun = string -> unit
val parse : (key * spec * doc) list -> anon_fun -> usage_msg -> unit
val parse_dynamic :
  (key * spec * doc) list ref -> anon_fun -> usage_msg -> unit
val parse_argv :
  ?current:int ref ->
    string array -> (key * spec * doc) list -> anon_fun -> usage_msg -> unit
val parse_argv_dynamic :
  ?current:int ref ->
    string array -> (key * spec * doc) list ref -> anon_fun -> string -> unit
exception Help of string 
exception Bad of string 
val usage : (key * spec * doc) list -> usage_msg -> unit
val usage_string : (key * spec * doc) list -> usage_msg -> string
val align : ?limit:int -> (key * spec * doc) list -> (key * spec * doc) list
val current : int ref
