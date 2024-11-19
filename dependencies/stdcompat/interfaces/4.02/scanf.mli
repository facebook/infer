module Scanning :
sig
  type in_channel
  type scanbuf = in_channel
  val stdin : in_channel
  type file_name = string
  val open_in : file_name -> in_channel
  val open_in_bin : file_name -> in_channel
  val close_in : in_channel -> unit
  val from_file : file_name -> in_channel
  val from_file_bin : string -> in_channel
  val from_string : string -> in_channel
  val from_function : (unit -> char) -> in_channel
  val from_channel : in_channel -> in_channel
  val end_of_input : in_channel -> bool
  val beginning_of_input : in_channel -> bool
  val name_of_input : in_channel -> string
  val stdib : in_channel
end
type ('a, 'b, 'c, 'd) scanner =
  ('a, Scanning.in_channel, 'b, 'c, 'a -> 'd, 'd) format6 -> 'c
exception Scan_failure of string 
val bscanf : Scanning.in_channel -> ('a, 'b, 'c, 'd) scanner
val fscanf : in_channel -> ('a, 'b, 'c, 'd) scanner
val sscanf : string -> ('a, 'b, 'c, 'd) scanner
val scanf : ('a, 'b, 'c, 'd) scanner
val kscanf :
  Scanning.in_channel ->
    (Scanning.in_channel -> exn -> 'd) -> ('a, 'b, 'c, 'd) scanner
val ksscanf :
  string -> (Scanning.in_channel -> exn -> 'd) -> ('a, 'b, 'c, 'd) scanner
val kfscanf :
  in_channel ->
    (Scanning.in_channel -> exn -> 'd) -> ('a, 'b, 'c, 'd) scanner
val bscanf_format :
  Scanning.in_channel ->
    ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
      (('a, 'b, 'c, 'd, 'e, 'f) format6 -> 'g) -> 'g
val sscanf_format :
  string ->
    ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
      (('a, 'b, 'c, 'd, 'e, 'f) format6 -> 'g) -> 'g
val format_from_string :
  string ->
    ('a, 'b, 'c, 'd, 'e, 'f) format6 -> ('a, 'b, 'c, 'd, 'e, 'f) format6
val unescaped : string -> string
