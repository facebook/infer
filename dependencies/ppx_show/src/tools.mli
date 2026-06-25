open Ppxlib

val flatten_map : ('a -> 'b list) -> 'a list -> 'b list
(** [flatten_map f list] is equal to [List.flatten (List.map f list)]. *)

val map_loc : ('a -> 'b) -> 'a Location.loc -> 'b Location.loc

type affix =
  | Prefix of string
  | Suffix of string
  | PrefixSuffix of string * string

val mangle : ?fixpoint : string -> affix -> string -> string

val mangle_lid : ?fixpoint : string -> affix -> Longident.t -> Longident.t

val mangle_type_decl :
    ?fixpoint : string -> affix -> type_declaration -> string Location.loc

val seq : ?loc : Location.t -> expression list -> expression

val separate : 'a -> 'a list -> 'a list

val poly_var : string -> string

val poly_fun_of_type_decl : type_declaration -> expression -> expression

val poly_arrow_of_type_decl :
    (core_type -> core_type) -> type_declaration -> core_type
      -> core_type

val core_type_of_type_decl : type_declaration -> core_type

val expand_path : path : string list -> string -> string

val path_of_type_decl : path : string list -> type_declaration -> string list

val pat_var_of_string : string -> pattern

val ident_of_string : string -> expression

val ident_of_str : string Location.loc -> expression

val poly_apply_of_type_decl : type_declaration -> expression -> expression

val var_of_type : core_type -> string
