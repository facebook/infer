(*
* Copyright (c) 2009-2013 Monoidics ltd.
* Copyright (c) 2013- Facebook.
* All rights reserved.
*)

(** Abstraction for Arrays *)

(** Apply array abstraction and check the result *)
val abstract_array_check : Prop.normal Prop.t -> Prop.normal Prop.t

(** Remember whether array abstraction was performed (to be reset before calling Abs.abstract) *)
val array_abstraction_performed : bool ref

(** remove redundant elements in an array *)
val remove_redundant_elements : Prop.normal Prop.t -> Prop.normal Prop.t