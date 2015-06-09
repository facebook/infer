(*
* Copyright (c) 2009-2013 Monoidics ltd.
* Copyright (c) 2013- Facebook.
* All rights reserved.
*)

(** Preanalysis for eliminating dead local variables *)

(** Perform liveness analysis *)
val doit : Cfg.cfg -> Sil.tenv -> unit

(** Return the time for the last execution of the analysis *)
val gettime : unit -> float
