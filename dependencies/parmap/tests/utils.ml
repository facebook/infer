(**************************************************************************)
(* Generic testing framework for evaluating speedup on Parmap             *)
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

let scale_test ?(init=(fun _ -> ())) ?(inorder=true) ?(step=1) ?chunksize ?(keeporder=false) compute sequence iter nprocmin nprocmax =
  Printf.eprintf "Testing scalability with %d iterations on %d to %d cores, step %d\n%!" iter nprocmin nprocmax step;
  let rseq,tseq =  
    let d=Unix.gettimeofday() in
    match sequence with
      L l -> let l'=List.rev_map compute l in List.rev l',(Unix.gettimeofday() -. d)
    | A a -> let l'=Array.to_list(Array.map compute a) in l',(Unix.gettimeofday() -. d)
  in
  Printf.eprintf "Sequential execution takes %f seconds\n%!" tseq;
  for incr = 0 to (nprocmax-nprocmin)/step do
    let i = nprocmin + incr in
    let tot=ref 0.0 in
    for j=1 to iter do
      let d=Unix.gettimeofday() in
      let rpar=parmap ~init ~ncores:i ?chunksize ~keeporder compute sequence in
      tot:=!tot+.(Unix.gettimeofday()-.d);
      if rseq<>rpar then 
	begin
	  if (List.sort compare rseq) <> (List.sort compare rpar) then 
	    Printf.eprintf "Parmap failure: result mismatch!\n%!"
	  else
	    if inorder || keeporder then Printf.eprintf "Parmap failure: result order was expected to be preserved, and is not.\n%!"
	    else Printf.eprintf "Parmap warning: result order is not preserved (it was not expected to be).\n%!"
	end
    done;
    let speedup=tseq /. (!tot /. (float iter)) in 
    Printf.eprintf "Speedup with %d cores (average on %d iterations): %f (tseq=%f, tpar=%f)\n%!" i iter speedup tseq (!tot /. (float iter))
  done;
  rseq
;;

let array_scale_test ?(init= (fun _ -> ())) ?(inorder=true) ?(step=1) ?chunksize ?(keeporder=false) compute a iter nprocmin nprocmax =
  Printf.eprintf "Testing scalability with %d iterations on %d to %d cores, step %d\n" iter nprocmin nprocmax step;
  let rseq,tseq =  
    let d=Unix.gettimeofday() in
    let a'= Array.map compute a in
    a',(Unix.gettimeofday() -. d)
  in
  Printf.eprintf "Sequential execution takes %f seconds\n" tseq;
  for incr = 0 to (nprocmax-nprocmin)/step do
    let i = nprocmin + incr in
    let tot=ref 0.0 in
    for j=1 to iter do
      let d=Unix.gettimeofday() in
      let rpar=array_parmap ~init ~ncores:i ?chunksize ~keeporder compute a in
      tot:=!tot+.(Unix.gettimeofday()-.d);
      if rseq<>rpar then 
	begin
	  if (Array.sort compare rseq) <> (Array.sort compare rpar) then 
	    Printf.eprintf "Parmap failure: result mismatch!\n"
	  else
	    if inorder || keeporder then Printf.eprintf "Parmap failure: result order was expected to be preserved, and is not.\n"
	    else Printf.eprintf "Parmap warning: result order is not preserved (it was not expected to be).\n"
	end
    done;
    let speedup=tseq /. (!tot /. (float iter)) in 
    Printf.eprintf "Speedup with %d cores (average on %d iterations): %f (tseq=%f, tpar=%f)\n%!" i iter speedup tseq (!tot /. (float iter))
  done;
  rseq
;;

let array_float_scale_test ?(init= (fun _ -> ())) ?(inorder=true) ?(step=1) ?chunksize compute a iter nprocmin nprocmax =
  Printf.eprintf "Testing scalability with %d iterations on %d to %d cores, step %d\n" iter nprocmin nprocmax step;
  let rseq,tseq =  
    let d=Unix.gettimeofday() in
    let a'= Array.map compute a in
    a',(Unix.gettimeofday() -. d)
  in
  Printf.eprintf "Sequential execution takes %f seconds\n" tseq;
  for incr = 0 to (nprocmax-nprocmin)/step do
    let i = nprocmin + incr in
    let tot=ref 0.0 in
    for j=1 to iter do
      let d=Unix.gettimeofday() in
      let rpar=array_float_parmap ~init ~ncores:i compute a in
      tot:=!tot+.(Unix.gettimeofday()-.d);
      if rseq<>rpar then 
	begin
	  if (Array.sort compare rseq) <> (Array.sort compare rpar) then 
	    Printf.eprintf "Parmap failure: result mismatch!\n"
	  else
	    if inorder then Printf.eprintf "Parmap failure: result order was expected to be preserved, and is not.\n"
	    else Printf.eprintf "Parmap warning: result order is not preserved (it was not expected to be).\n"
	end
    done;
    let speedup=tseq /. (!tot /. (float iter)) in 
    Printf.eprintf "Speedup with %d cores (average on %d iterations): %f (tseq=%f, tpar=%f)\n%!" i iter speedup tseq (!tot /. (float iter))
  done;
  rseq
;;
