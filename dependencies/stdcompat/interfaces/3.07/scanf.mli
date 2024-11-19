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
end
exception Scan_failure of string 
val bscanf :
  Scanning.scanbuf -> ('a, Scanning.scanbuf, 'b) format -> 'a -> 'b
val fscanf : in_channel -> ('a, Scanning.scanbuf, 'b) format -> 'a -> 'b
val sscanf : string -> ('a, Scanning.scanbuf, 'b) format -> 'a -> 'b
val scanf : ('a, Scanning.scanbuf, 'b) format -> 'a -> 'b
val kscanf :
  Scanning.scanbuf ->
    (Scanning.scanbuf -> exn -> 'a) ->
      ('b, Scanning.scanbuf, 'a) format -> 'b -> 'a
