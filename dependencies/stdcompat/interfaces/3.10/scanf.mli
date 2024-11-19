module Scanning :
sig
  type scanbuf
  val stdib : scanbuf
  val from_string : string -> scanbuf
  val from_file : string -> scanbuf
  val from_file_bin : string -> scanbuf
  val from_function : (unit -> char) -> scanbuf
  val from_channel : in_channel -> scanbuf
  val end_of_input : scanbuf -> bool
  val beginning_of_input : scanbuf -> bool
  val name_of_input : scanbuf -> string
end
exception Scan_failure of string 
type ('a, 'b, 'c, 'd) scanner =
  ('a, Scanning.scanbuf, 'b, 'c, 'a -> 'd, 'd) format6 -> 'c
val bscanf : Scanning.scanbuf -> ('a, 'b, 'c, 'd) scanner
val fscanf : in_channel -> ('a, 'b, 'c, 'd) scanner
val sscanf : string -> ('a, 'b, 'c, 'd) scanner
val scanf : ('a, 'b, 'c, 'd) scanner
val kscanf :
  Scanning.scanbuf ->
    (Scanning.scanbuf -> exn -> 'a) -> ('b, 'c, 'd, 'a) scanner
val bscanf_format :
  Scanning.scanbuf ->
    ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
      (('a, 'b, 'c, 'd, 'e, 'f) format6 -> 'g) -> 'g
val sscanf_format :
  string ->
    ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
      (('a, 'b, 'c, 'd, 'e, 'f) format6 -> 'g) -> 'g
val format_from_string :
  string ->
    ('a, 'b, 'c, 'd, 'e, 'f) format6 -> ('a, 'b, 'c, 'd, 'e, 'f) format6
