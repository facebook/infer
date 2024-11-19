module type S = sig
val temp_dir : ?temp_dir:string -> ?perms:int -> string -> string -> string
(** @since 5.1.0:
    val temp_dir :
      ?temp_dir:string -> ?perms:int -> string -> string -> string *)

val null : string
(** @since 4.10.0: val null : string *)

val quote_command :
  string ->
    ?stdin:string ->
      ?stdout:string -> ?stderr:string -> string list -> string
(** @since 4.10.0:
    val quote_command :
      string ->
        ?stdin:string ->
          ?stdout:string -> ?stderr:string -> string list -> string *)

val chop_suffix_opt : suffix:string -> string -> string option
(** @since 4.08.0:
    val chop_suffix_opt : suffix:string -> string -> string option *)

val extension : string -> string
(** @since 4.04.0: val extension : string -> string *)

val remove_extension : string -> string
(** @since 4.04.0: val remove_extension : string -> string *)

val open_temp_file :
  ?mode:open_flag list ->
    ?perms:int ->
      ?temp_dir:string -> string -> string -> (string * out_channel)
(** @since 4.03.0:
    val open_temp_file :
      ?mode:open_flag list ->
        ?perms:int ->
          ?temp_dir:string -> string -> string -> (string * out_channel) *)

val get_temp_dir_name : unit -> string
(** @since 4.00.0: val get_temp_dir_name : unit -> string *)

val set_temp_dir_name : string -> unit
(** @since 4.00.0: val set_temp_dir_name : string -> unit *)

val dir_sep : string
(** @since 3.11.0: val dir_sep : string *)

val temp_file : ?temp_dir:string -> string -> string -> string
(** @since 3.11.0:
    val temp_file : ?temp_dir:string -> string -> string -> string *)

val current_dir_name : string
(** Alias for {!Filename.current_dir_name} *)

val parent_dir_name : string
(** Alias for {!Filename.parent_dir_name} *)

val concat : string -> string -> string
(** Alias for {!Filename.concat} *)

val is_relative : string -> bool
(** Alias for {!Filename.is_relative} *)

val is_implicit : string -> bool
(** Alias for {!Filename.is_implicit} *)

val check_suffix : string -> string -> bool
(** Alias for {!Filename.check_suffix} *)

val chop_suffix : string -> string -> string
(** Alias for {!Filename.chop_suffix} *)

val chop_extension : string -> string
(** Alias for {!Filename.chop_extension} *)

val basename : string -> string
(** Alias for {!Filename.basename} *)

val dirname : string -> string
(** Alias for {!Filename.dirname} *)

val quote : string -> string
(** Alias for {!Filename.quote} *)

end
