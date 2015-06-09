(*
* Copyright (c) 2009 -2013 Monoidics ltd.
* Copyright (c) 2013 - Facebook.
* All rights reserved.
*)
(** This module handles buckets of memory leaks in Objective-C *)

open Utils

let objc_arc_flag = "objc_arc"

let bucket_delimiter = ","

type mleak_bucket =
  | MLeak_cf
  | MLeak_arc
  | MLeak_no_arc

let objc_ml_buckets = ref []

let bucket_from_string bucket_s =
  match bucket_s with
  | "cf" -> MLeak_cf
  | "arc" -> MLeak_arc
  | "narc" -> MLeak_no_arc
  | _ -> assert false

let bucket_to_string bucket =
  match bucket with
  | MLeak_cf -> "Core Foundation"
  | MLeak_arc -> "Arc"
  | MLeak_no_arc -> "No arc"

let bucket_to_message bucket =
  match bucket with
  | MLeak_cf -> "[CF]"
  | MLeak_arc -> "[ARC]"
  | MLeak_no_arc -> "[NO ARC]"

let mleak_bucket_compare b1 b2 =
  match b1, b2 with
  | MLeak_cf, MLeak_cf -> 0
  | MLeak_cf, _ -> -1
  | _, MLeak_cf -> 1
  | MLeak_arc, MLeak_arc -> 0
  | MLeak_arc, _ -> -1
  | _, MLeak_arc -> 1
  | MLeak_no_arc, MLeak_no_arc -> 0

let mleak_bucket_eq b1 b2 =
  mleak_bucket_compare b1 b2 = 0

let init_buckets objc_ml_buckets_arg =
  let buckets =
    Str.split (Str.regexp bucket_delimiter) objc_ml_buckets_arg in
  let buckets =
    match buckets with
    | ["all"] -> []
    | _ -> list_map bucket_from_string buckets in
  objc_ml_buckets := buckets

let contains_cf ml_buckets =
  list_mem mleak_bucket_eq MLeak_cf ml_buckets

let contains_arc ml_buckets =
  list_mem mleak_bucket_eq MLeak_arc ml_buckets

let contains_narc ml_buckets =
  list_mem mleak_bucket_eq MLeak_no_arc ml_buckets

let should_raise_leak_cf typ =
  if contains_cf !objc_ml_buckets then
    Objc_models.is_core_lib_type typ
  else false

let should_raise_leak_arc () =
  if contains_arc !objc_ml_buckets then
    !Config.arc_mode
  else false

let should_raise_leak_no_arc () =
  if contains_narc !objc_ml_buckets then
    not (!Config.arc_mode)
  else false

(* Returns whether a memory leak should be raised. If objc_ml_buckets is not there, *)
(* then raise all memory leaks. *)
(* If cf is passed, then check leaks from Core Foundation. *)
(* If arc is passed, check leaks from code that compiles with arc*)
(* If no arc is passed check the leaks from code that compiles without arc *)
let should_raise_leak typ =
  if list_length !objc_ml_buckets = 0 then Some ""
  else
  if should_raise_leak_cf typ then Some (bucket_to_message MLeak_cf)
  else if should_raise_leak_arc () then Some (bucket_to_message MLeak_arc)
  else if should_raise_leak_no_arc () then Some (bucket_to_message MLeak_no_arc)
  else None
