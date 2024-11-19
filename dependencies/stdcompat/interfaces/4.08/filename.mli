val current_dir_name : string
val parent_dir_name : string
val dir_sep : string
val concat : string -> string -> string
val is_relative : string -> bool
val is_implicit : string -> bool
val check_suffix : string -> string -> bool
val chop_suffix : string -> string -> string
val chop_suffix_opt : suffix:string -> string -> string option
val extension : string -> string
val remove_extension : string -> string
val chop_extension : string -> string
val basename : string -> string
val dirname : string -> string
val temp_file : ?temp_dir:string -> string -> string -> string
val open_temp_file :
  ?mode:open_flag list ->
    ?perms:int ->
      ?temp_dir:string -> string -> string -> (string * out_channel)
val get_temp_dir_name : unit -> string
val set_temp_dir_name : string -> unit
val temp_dir_name : string
val quote : string -> string
