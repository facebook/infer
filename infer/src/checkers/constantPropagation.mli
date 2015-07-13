(*
* Copyright (c) 2014 - Facebook. All rights reserved.
*)

type const_map = Cfg.Node.t -> Sil.exp -> Sil.const option

(** Build a const map lazily. *)
val build_const_map : Cfg.Procdesc.t -> const_map
