(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open ConcurrencyModels

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


let is_futures_getdone =
  is_call_of_class "com.google.common.util.concurrent.Futures" ["getDone"] |> Staged.unstage


let should_skip_analysis =
  let matchers = [is_futures_getdone] in
  fun tenv pn actuals -> List.exists matchers ~f:(fun matcher -> matcher tenv pn actuals)


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
        AccessExpression.to_access_path timeunit_acc_exp |> str_of_access_path
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


(** is the method called Object.wait or on subclass, without timeout or with excessive timeout ? *)
let is_object_wait =
  is_call_of_class ~actuals_pred:empty_or_excessive_timeout "java.lang.Object" ["wait"]
  |> Staged.unstage


(** is the method called CountDownLath.await or on subclass? *)
let is_countdownlatch_await =
  is_call_of_class ~actuals_pred:empty_or_excessive_timeout "java.util.concurrent.CountDownLatch"
    ["await"]
  |> Staged.unstage


(** an IBinder.transact call is an RPC.  If the 4th argument (5th counting `this` as the first)
           is int-zero then a reply is expected and returned from the remote process, thus potentially
           blocking.  If the 4th argument is anything else, we assume a one-way call which doesn't block.
        *)
let is_two_way_binder_transact =
  let actuals_pred actuals =
    List.nth actuals 4 |> Option.value_map ~default:false ~f:HilExp.is_int_zero
  in
  is_call_of_class ~actuals_pred "android.os.IBinder" ["transact"] |> Staged.unstage


(** is it a call to Future.get()? *)
let is_future_get =
  is_call_of_class ~search_superclasses:false ~actuals_pred:empty_or_excessive_timeout
    "java.util.concurrent.Future" ["get"]
  |> Staged.unstage


let is_accountManager_setUserData =
  is_call_of_class ~search_superclasses:false "android.accounts.AccountManager" ["setUserData"]
  |> Staged.unstage


let is_asyncTask_get =
  is_call_of_class ~actuals_pred:empty_or_excessive_timeout "android.os.AsyncTask" ["get"]
  |> Staged.unstage


(* consider any call to sleep as bad, even with timeouts lower than the anr limit *)
let is_thread_sleep = is_call_of_class "java.lang.Thread" ["sleep"] |> Staged.unstage

(* matcher for strict mode throws in Android libcore implementation,
   used with --dev-android strict mode *)
let is_blockguard_on =
  is_call_of_class ~method_prefix:true "dalvik.system.BlockGuard$Policy" ["on"] |> Staged.unstage


let is_system_gc = is_call_of_class "java.lang.System" ["gc"; "runFinalization"] |> Staged.unstage

let is_runtime_gc = is_call_of_class "java.lang.Runtime" ["gc"] |> Staged.unstage

let is_file_io =
  is_call_of_class "java.io.File"
    [ "canRead"
    ; "canWrite"
    ; "createNewFile"
    ; "createTempFile"
    ; "delete"
    ; "getCanonicalPath"
    ; "getFreeSpace"
    ; "getTotalSpace"
    ; "getUsableSpace"
    ; "isDirectory"
    ; "isFile"
    ; "isHidden"
    ; "lastModified"
    ; "length"
    ; "list"
    ; "listFiles"
    ; "mkdir"
    ; "renameTo"
    ; "setExecutable"
    ; "setLastModified"
    ; "setReadable"
    ; "setReadOnly"
    ; "setWritable" ]
  |> Staged.unstage


let is_socket_connect = is_call_of_class "java.net.Socket" ["connect"] |> Staged.unstage

let is_connected_socket_constructor =
  (* all public constructors of Socket with two or more arguments call connect *)
  let actuals_pred = function [] | [_] -> false | _ -> true in
  is_call_of_class ~actuals_pred "java.net.Socket" [Typ.Procname.Java.constructor_method_name]
  |> Staged.unstage


let is_datagram_socket_connect =
  is_call_of_class "java.net.DatagramSocket" ["connect"] |> Staged.unstage


(* matchers used for normal analysis as well as in --dev-android-strict-mode *)
(* selection is a bit arbitrary as some would be generated anyway if not here; no harm though *)
(* some, like [file], need manual addition due to our lack of handling dynamic dispatch *)
let strict_mode_common_matchers =
  let open StarvationDomain.Event in
  [ (is_connected_socket_constructor, High)
  ; (is_datagram_socket_connect, High)
  ; (is_file_io, High)
  ; (is_runtime_gc, High)
  ; (is_socket_connect, High)
  ; (is_system_gc, High) ]


let strict_mode_seed_matchers =
  (is_blockguard_on, StarvationDomain.Event.High) :: strict_mode_common_matchers


let strict_mode_matchers =
  let open StarvationDomain.Event in
  (StrictModeModels.is_strict_mode_violation, High) :: strict_mode_common_matchers


(* at most one function is allowed to be true, sort from High to Low *)
let may_block =
  let open StarvationDomain.Event in
  let matchers =
    if Config.dev_android_strict_mode then strict_mode_seed_matchers
    else
      strict_mode_matchers
      @ [ (is_accountManager_setUserData, High)
        ; (is_two_way_binder_transact, High)
        ; (is_countdownlatch_await, High)
        ; (is_thread_sleep, High)
        ; (is_object_wait, High)
        ; (is_asyncTask_get, Low)
        ; (is_future_get, Low) ]
  in
  fun tenv pn actuals ->
    List.find_map matchers ~f:(fun (matcher, sev) -> Option.some_if (matcher tenv pn actuals) sev)
