(* simple example to show how to use Parmap on the native toplevel *)
(* just run ocamlnat and then #use this file *)
#load "unix.cmxs";;
#load "bigarray.cmxs";;
#load "parmap.cmxs";;
let mkn n = let rec aux acc = function 0 -> acc | n -> aux (n::acc) (n-1) in aux [] n;;
Printf.printf "%d\n" (List.length (Parmap.parmap (fun x -> x+1) (Parmap.L (mkn 10000)) ~ncores:2));;
