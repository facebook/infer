(*
* Copyright (c) 2013 - Facebook. All rights reserved.
*)

type model_table_t = (string, bool * bool list) Hashtbl.t

val annotated_table_nullable : model_table_t
val annotated_table_present : model_table_t
val annotated_table_strict : model_table_t
val check_not_null_table : model_table_t
val check_not_null_parameter_table : (string, int) Hashtbl.t
val check_state_table : model_table_t
val check_argument_table : model_table_t
val optional_get_table : model_table_t
val optional_isPresent_table : model_table_t
val containsKey_table : model_table_t
