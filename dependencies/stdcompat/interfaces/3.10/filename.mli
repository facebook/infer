val current_dir_name : string
val parent_dir_name : string
val concat : string -> string -> string
val is_relative : string -> bool
val is_implicit : string -> bool
val check_suffix : string -> string -> bool
val chop_suffix : string -> string -> string
val chop_extension : string -> string
val basename : string -> string
val dirname : string -> string
val temp_file : string -> string -> string
val open_temp_file :
  ?mode:open_flag list -> string -> string -> (string * out_channel)
val temp_dir_name : string
val quote : string -> string
