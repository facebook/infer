(** This module declares utility functions that does not require Python to
    be initialized. *)

val substring_between: string -> int -> int -> string
(** [substring_between s i j] returns the substring of [s] between the indices
    [i] (included) and [j] (excluded). *)

val int_of_octal: string -> int
(** Returns the integer represented by the argument written in base 8. *)

val int_of_hex: string -> int
(** Returns the integer represented by the argument written in base 16. *)

val split_left_on_char: ?from:int -> char -> string -> string
(** If the character occurs in the substring beginning from [from],
    returns the prefix that precedes the first occurrence (excluded),
    else returns the whole substring beginning from [from]. *)

val split_right_on_char: ?from:int -> char -> string -> string
(** If the character occurs in the substring beginning from [from],
    returns the suffix that succedes the first occurrence (excluded),
    else returns the whole substring beginning from [from]. *)

val trim_carriage_return: string -> string
(** If the string ends with ['\r'], then returns the string without this
    character, else returns the whole string. *)

val input_lines: in_channel -> string list
(** Reads and returns all the lines from an input channel to the end of file.
    Carriage return characters are removed from the end of lines if any. *)

val option_find: ('a -> 'b) -> 'a -> 'b option
(** [option_find f x] returns [Some (f x)], or [None] if [f x] raises
    [Not_found]. *)

val write_and_close: out_channel -> ('a -> 'b) -> 'a -> 'b
(** [write_and_close channel f arg] calls [f arg], and returns the result of
    [f].
    [channel] is always closed after [f] has been called, even if [f] raises
    an exception. *)

val with_temp_file: string -> (string -> in_channel -> 'a) -> 'a
(** [with_temp_file s f] creates a temporary file with [s] as contents and
    calls [f filename in_channel] where [filename] is the name of the
    temporary file and [in_channel] is an input channel opened to read the
    file. The file is deleted after the execution of [f] (even if [f]
    raised an exception. *)

val with_pipe: (in_channel -> out_channel -> 'a) -> 'a
(** [with_pipe f] creates a pipe and calls [f] with the two ends of the
    pipe. *)

val with_stdin_from: in_channel -> ('a -> 'b) -> 'a -> 'b
(** [with_stdin_from chan f arg] calls [f arg] with the standard input
    redirected for reading from [chan]. *)

val with_channel_from_string: string -> (in_channel -> 'a) -> 'a
(** [with_channel_from_string s f] calls [f in_channel] where [in_channel]
    is an input channel returning the contents of [s]. *)

val with_stdin_from_string: string -> ('a -> 'b) -> 'a -> 'b
(** [with_stdin_from_string s f arg] calls [f arg] with the standard input
    redirected for reading from the contents of [s]. *)
