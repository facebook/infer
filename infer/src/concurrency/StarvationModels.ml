(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let is_synchronized_library_call =
  let targets = ["java.lang.StringBuffer"; "java.util.Hashtable"; "java.util.Vector"] in
  fun tenv pn ->
    (not (Typ.Procname.is_constructor pn))
    &&
    match pn with
    | Typ.Procname.Java java_pname ->
        let classname = Typ.Procname.Java.get_class_type_name java_pname in
        List.exists targets ~f:(PatternMatch.is_subtype_of_str tenv classname)
    | _ ->
        false


let should_skip_analysis = MethodMatcher.of_json Config.starvation_skip_analysis

(** magical value from https://developer.android.com/topic/performance/vitals/anr *)
let android_anr_time_limit = 5.0

(* get time unit in seconds *)
let timeunit_of_exp =
  let time_units =
    String.Map.of_alist_exn
      [ ("NANOSECONDS", 0.000_000_001)
      ; ("MICROSECONDS", 0.000_001)
      ; ("MILLISECONDS", 0.001)
      ; ("SECONDS", 1.0)
      ; ("MINUTES", 60.0)
      ; ("HOURS", 3_600.0)
      ; ("DAYS", 86_400.0) ]
  in
  let str_of_access_path = function
    | _, [AccessPath.FieldAccess field]
      when String.equal "java.util.concurrent.TimeUnit" (Typ.Fieldname.Java.get_class field) ->
        Some (Typ.Fieldname.Java.get_field field)
    | _ ->
        None
  in
  let str_of_exp = function
    | HilExp.AccessExpression timeunit_acc_exp ->
        HilExp.AccessExpression.to_access_path timeunit_acc_exp |> str_of_access_path
    | _ ->
        None
  in
  fun timeunit_exp -> str_of_exp timeunit_exp |> Option.bind ~f:(String.Map.find time_units)


(** check whether actuals of a method call either empty, denoting indefinite timeout,
      or evaluate to a finite timeout greater than the android anr limit *)
let empty_or_excessive_timeout actuals =
  let duration_of_exp = function
    | HilExp.Constant (Const.Cint duration_lit) ->
        Some (IntLit.to_float duration_lit)
    | _ ->
        None
  in
  (* all arguments in seconds *)
  let is_excessive_secs duration = duration >. android_anr_time_limit in
  match actuals with
  | [_] ->
      (* this is a wait or lock call without timeout, thus it can block indefinitely *)
      true
  | [_; snd_arg] ->
      (* this is an Object.wait(_) call, second argument should be a duration in milliseconds *)
      duration_of_exp snd_arg
      |> Option.value_map ~default:false ~f:(fun duration -> is_excessive_secs (0.001 *. duration))
  | [_; snd_arg; third_arg] ->
      (* this is either a call to Object.wait(_, _) or to a java.util.concurent.lock(_, _) method.
           In the first case the arguments are a duration in milliseconds and an extra duration in
           nanoseconds; in the second case, the arguments are a duration and a time unit. *)
      duration_of_exp snd_arg
      |> Option.value_map ~default:false ~f:(fun duration ->
             match timeunit_of_exp third_arg with
             | Some timeunit ->
                 is_excessive_secs (timeunit *. duration)
             | None ->
                 duration_of_exp third_arg
                 |> Option.value_map ~default:false ~f:(fun extra ->
                        is_excessive_secs (0.001 *. (duration +. (0.000_001 *. extra))) ) )
  | _ ->
      false


let standard_matchers =
  let open MethodMatcher in
  let open StarvationDomain.Event in
  let high_sev =
    [ {default with classname= "java.lang.Thread"; methods= ["sleep"]}
    ; { default with
        classname= "java.lang.Object"; methods= ["wait"]; actuals_pred= empty_or_excessive_timeout
      }
    ; { default with
        classname= "java.util.concurrent.CountDownLatch"
      ; methods= ["await"]
      ; actuals_pred= empty_or_excessive_timeout }
      (* an IBinder.transact call is an RPC.  If the 4th argument (5th counting `this` as the first)
         is int-zero then a reply is expected and returned from the remote process, thus potentially
         blocking.  If the 4th argument is anything else, we assume a one-way call which doesn't block. *)
    ; { default with
        classname= "android.os.IBinder"
      ; methods= ["transact"]
      ; actuals_pred=
          (fun actuals ->
            List.nth actuals 4 |> Option.value_map ~default:false ~f:HilExp.is_int_zero ) } ]
  in
  let low_sev =
    [ { default with
        classname= "java.util.concurrent.Future"
      ; methods= ["get"]
      ; actuals_pred= empty_or_excessive_timeout }
    ; { default with
        classname= "android.os.AsyncTask"
      ; methods= ["get"]
      ; actuals_pred= empty_or_excessive_timeout } ]
  in
  let high_sev_matcher = List.map high_sev ~f:of_record |> of_list in
  let low_sev_matcher = List.map low_sev ~f:of_record |> of_list in
  [(high_sev_matcher, High); (low_sev_matcher, Low)]


(* sort from High to Low *)
let may_block tenv pn actuals =
  List.find_map standard_matchers ~f:(fun (matcher, sev) ->
      Option.some_if (matcher tenv pn actuals) sev )


(* selection is a bit arbitrary as some would be generated anyway if not here; no harm though *)
(* some, like [file], need manual addition due to our lack of handling dynamic dispatch *)
let strict_mode_matcher =
  let open MethodMatcher in
  (* NB [default] searches superclasses too.  Most of the classes below are final and we don't
     really want to search superclasses for those that aren't, so for performance, disable that *)
  let dont_search_superclasses = {default with search_superclasses= false} in
  let matcher_records =
    [ { dont_search_superclasses with
        classname= "dalvik.system.BlockGuard$Policy"; methods= ["on"]; method_prefix= true }
    ; { dont_search_superclasses with
        classname= "java.lang.System"; methods= ["gc"; "runFinalization"] }
    ; {dont_search_superclasses with classname= "java.lang.Runtime"; methods= ["gc"]}
    ; {dont_search_superclasses with classname= "java.net.Socket"; methods= ["connect"]}
      (* all public constructors of Socket with two or more arguments call connect *)
    ; { dont_search_superclasses with
        classname= "java.net.Socket"
      ; methods= [Typ.Procname.Java.constructor_method_name]
      ; actuals_pred= (function [] | [_] -> false | _ -> true) }
    ; {dont_search_superclasses with classname= "java.net.DatagramSocket"; methods= ["connect"]}
    ; { dont_search_superclasses with
        classname= "java.io.File"
      ; methods=
          [ "canRead"
          ; "canWrite"
          ; "createNewFile"
          ; "createTempFile"
          ; "delete"
          ; "getFreeSpace"
          ; "getTotalSpace"
          ; "getUsableSpace"
          ; "lastModified"
          ; "list"
          ; "listFiles"
          ; "mkdir"
          ; "renameTo"
          ; "setExecutable"
          ; "setLastModified"
          ; "setReadable"
          ; "setReadOnly"
          ; "setWritable" ] } ]
  in
  of_records matcher_records


let is_strict_mode_violation tenv pn actuals =
  Config.starvation_strict_mode && strict_mode_matcher tenv pn actuals
