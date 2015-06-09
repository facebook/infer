(*
* Copyright (c) 2009-2013 Monoidics ltd.
* Copyright (c) 2013- Facebook.
* All rights reserved.
*)

(** Classify bugs into buckets *)

open Utils

(** Classify the bucket of an error desc using Location.access information *)
val classify_access : Localise.error_desc -> Localise.access option -> Localise.error_desc
