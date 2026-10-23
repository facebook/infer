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

let compute p = 
  let r=ref 1 in 
  for i = 1 to 80000 do 
    r:= !r+(p*p)-(p*(p-1))
  done;
  !r
;;

let fcompute p = 
  let r=ref 1. in 
  for i = 1 to 80000 do 
    r:= !r+.(p*.p)-.(p*.(p-.1.))
  done;
  !r
;;

array_scale_test fcompute (Array.init 20000 (fun i -> float_of_int i)) 2 1 10;;
array_scale_test ~chunksize:100 ~inorder:false fcompute (Array.init 20000 (fun i -> float_of_int i)) 2 1 10;;
array_scale_test ~chunksize:100 ~keeporder:true fcompute (Array.init 20000 (fun i -> float_of_int i)) 2 1 10;;

array_float_scale_test fcompute (Array.init 20000 (fun i -> float_of_int i)) 2 1 10;;

scale_test ~chunksize:100 ~inorder:false compute (A (Array.init 20000 (fun i -> i))) 2 1 10;;
scale_test ~chunksize:100 ~keeporder:true compute (A (Array.init 20000 (fun i -> i))) 2 1 10;;

scale_test compute (A (Array.init 20000 (fun i -> i))) 2 1 10;;
