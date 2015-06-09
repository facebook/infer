(*
* Copyright (c) 2013 - Facebook. All rights reserved.
*)

(** Environment for temporary identifiers used in instructions.
Lazy implementation: only created when actually used. *)


type t

val create : Cfg.cfg -> Cfg.Procdesc.t -> t
val create_from_idenv : t -> Cfg.Procdesc.t -> t
val lookup : t -> Ident.t -> Sil.exp option
val expand_expr : t -> Sil.exp -> Sil.exp

val exp_is_temp : t -> Sil.exp -> bool

(** Stronger version of expand_expr which also expands a temporary variable. *)
val expand_expr_temps : t -> Cfg.Node.t -> Sil.exp -> Sil.exp
