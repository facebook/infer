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

Printf.eprintf "Testing capture of exception: this code should exit normally, without segfault.\n%!";;

Parmap.debugging true;;

let _ =
  try
    Parmap.parmap
      (function x ->
	failwith "should not crash")
      (Parmap.L [1;2;3;4;5])
  with _ -> (Printf.printf "Exceptions are properly catched, no SIGSEV"; exit 0)

  

