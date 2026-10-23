(**************************************************************************)
(* Sample use of Parmap, a simple library to perform Map computations on  *)
(* a multi-core                                                           *)
(*                                                                        *)
(*  Author(s):  Roberto Di Cosmo                                          *)
(*                                                                        *)
(*  This program is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as               *)
(*  published by the Free Software Foundation, either version 2 of the    *)
(*  License, or (at your option) any later version.                       *)
(**************************************************************************)

open Parmap
open Utils

(* from Paul Vernaza's code *)

let env_param of_string name default = 
  try of_string (Unix.getenv name) with Not_found -> default 

let env_num = env_param float_of_string 

let env_float = env_num

let env_int = env_param int_of_string 

let env_bool = env_param bool_of_string 

let env_string = env_param (fun x -> x)

let nIters = env_int "nIters" 1

let nData = env_int "nData" 10000000

let nProcs = env_int "nProcs" 8

let inOrder = env_bool "inOrder" false

let chunksize = env_int "chunksize" (nData/nProcs)

let xs, func = 
  let xs = Array.make nData 0. in 
  let pi = 4. *. atan 1. in 
  for i = 0 to nData - 1 do 
    xs.(i) <- pi *. (float_of_int i /. (float_of_int nData))
  done;
  xs, (fun x -> cos (sqrt x))

let checksum_array arr () = 
  Array.fold_left (fun acc el -> acc +. el) 0. arr;;

let checksum_list list () = 
  List.fold_left (fun acc el -> acc +. el) 0. list;;

Printf.printf "Test: normal parmap\n%!";;
scale_test ~chunksize:chunksize ~inorder:inOrder func (A xs) nIters nProcs nProcs;;

Printf.printf "Test: specialised array parmap\n%!";;
array_scale_test ~chunksize:chunksize ~inorder:inOrder func xs nIters nProcs nProcs;;

Printf.printf "Test: specialised float array parmap\n%!";;
array_float_scale_test ~chunksize:chunksize ~inorder:inOrder func xs nIters nProcs nProcs;;

