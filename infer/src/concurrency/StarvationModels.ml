(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let is_synchronized_library_call =
  let targets = ["java.lang.StringBuffer"; "java.util.Hashtable"; "java.util.Vector"] in
  fun tenv pn ->
    (not (Procname.is_constructor pn))
    &&
    match pn with
    | Procname.Java java_pname ->
        let classname = Procname.Java.get_class_type_name java_pname in
        List.exists targets ~f:(PatternMatch.is_subtype_of_str tenv classname)
    | _ ->
        false


let should_skip_analysis =
  let matcher = MethodMatcher.of_json Config.starvation_skip_analysis in
  fun tenv pname actuals ->
    match pname with
    | Procname.Java java_pname
      when Procname.Java.is_static java_pname
           && String.equal "getInstance" (Procname.get_method pname) ->
        true
    | _ ->
        matcher tenv pname actuals


(** magical value from https://developer.android.com/topic/performance/vitals/anr *)
let android_anr_time_limit = 5.0

(* get time unit in seconds *)
let secs_of_timeunit =
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
      when String.equal "java.util.concurrent.TimeUnit"
             (Typ.Name.name (Fieldname.get_class_name field)) ->
        Some (Fieldname.get_field_name field)
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


let float_of_const_int = function
  | HilExp.Constant (Const.Cint duration_lit) ->
      Some (IntLit.to_float duration_lit)
  | _ ->
      None


let is_excessive_secs duration = Float.(duration > android_anr_time_limit)

let no_args_or_excessive_timeout_and_timeunit = function
  | [_] ->
      (* no arguments, unconditionally blocks *)
      true
  | [_; timeout; timeunit] ->
      (* two arguments, a timeout and a time unit *)
      Option.both (float_of_const_int timeout) (secs_of_timeunit timeunit)
      |> Option.map ~f:(fun (duration, timeunit_secs) -> duration *. timeunit_secs)
      |> Option.exists ~f:is_excessive_secs
  | _ ->
      false


let no_args_or_excessive_millis_and_nanos = function
  | [_] ->
      (* this is a wait without timeout, it can block indefinitely *)
      true
  | [_; snd_arg] ->
      (* 2nd argument is a duration in milliseconds *)
      float_of_const_int snd_arg
      |> Option.exists ~f:(fun duration -> is_excessive_secs (0.001 *. duration))
  | [_; snd_arg; third_arg] ->
      (* 2nd argument is a duration in milliseconds, 3rd is extra duration in nanoseconds. *)
      Option.both (float_of_const_int snd_arg) (float_of_const_int third_arg)
      |> Option.map ~f:(fun (duration, extra) -> 0.001 *. (duration +. (0.000_001 *. extra)))
      |> Option.exists ~f:is_excessive_secs
  | _ ->
      false


let is_future_get =
  MethodMatcher.(
    of_record
      { default with
        classname= "java.util.concurrent.Future"
      ; methods= ["get"]
      ; actuals_pred= no_args_or_excessive_timeout_and_timeunit } )


let is_future_is_done =
  MethodMatcher.(
    of_record {default with classname= "java.util.concurrent.Future"; methods= ["isDone"]} )


let may_block =
  MethodMatcher.(
    of_records
      [ {default with classname= "java.lang.Thread"; methods= ["sleep"]}
      ; { default with
          classname= "java.lang.Thread"
        ; methods= ["join"]
        ; actuals_pred= no_args_or_excessive_millis_and_nanos }
      ; { default with
          classname= "java.util.concurrent.CountDownLatch"
        ; methods= ["await"]
        ; actuals_pred= no_args_or_excessive_timeout_and_timeunit }
      ; { default with
          classname= "android.os.AsyncTask"
        ; methods= ["get"]
        ; actuals_pred= no_args_or_excessive_timeout_and_timeunit } ] )


let may_do_ipc =
  MethodMatcher.(
    of_records
      [ (* an IBinder.transact call is an RPC.  If the 4th argument (5th counting `this` as the first)
           is int-zero then a reply is expected and returned from the remote process, thus potentially
           blocking.  If the 4th argument is anything else, we assume a one-way call which doesn't block. *)
        { default with
          classname= "android.os.IBinder"
        ; methods= ["transact"]
        ; actuals_pred= (fun actuals -> List.nth actuals 4 |> Option.exists ~f:HilExp.is_int_zero)
        }
      ; (* indirectly make a transact call *)
        { default with
          classname= "android.net.ConnectivityManager"
        ; methods= ["getActiveNetworkInfo"] }
      ; { default with
          classname= "android.media.AudioManager"
        ; methods= ["getStreamVolume"; "getRingerMode"] }
      ; { default with
          classname= "android.content.Context"
        ; methods= ["checkPermission"; "checkSelfPermission"] }
      ; {default with classname= "android.net.wifi.WifiManager"; methods= ["getConnectionInfo"]}
      ; {default with classname= "android.view.Display"; methods= ["getRealSize"]} ] )


let is_regex_op =
  MethodMatcher.(
    of_records
      [ (* Potentially costly regex operations, after https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html *)
        {default with classname= "java.util.regex.Pattern"; methods= ["compile"; "matches"]} ] )


let is_monitor_wait =
  MethodMatcher.(
    of_record
      { default with
        classname= "java.lang.Object"
      ; methods= ["wait"]
      ; actuals_pred= no_args_or_excessive_millis_and_nanos } )


(* selection is a bit arbitrary as some would be generated anyway if not here; no harm though *)
(* some, like [file], need manual addition due to our lack of handling dynamic dispatch *)
let strict_mode_matcher =
  let open MethodMatcher in
  (* NB [default] searches superclasses too.  Most of the classes below are final and we don't
     really want to search superclasses for those that aren't, so for performance, disable that *)
  let dont_search_superclasses = {default with search_superclasses= false} in
  let matcher_records =
    [ { dont_search_superclasses with
        classname= "dalvik.system.BlockGuard$Policy"
      ; methods= ["on"]
      ; method_prefix= true }
    ; { dont_search_superclasses with
        classname= "java.lang.System"
      ; methods= ["gc"; "runFinalization"] }
    ; {dont_search_superclasses with classname= "java.lang.Runtime"; methods= ["gc"]}
    ; {dont_search_superclasses with classname= "java.net.Socket"; methods= ["connect"]}
      (* all public constructors of Socket with two or more arguments call connect *)
    ; { dont_search_superclasses with
        classname= "java.net.Socket"
      ; methods= [Procname.Java.constructor_method_name]
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


let is_annotated_nonblocking tenv pname =
  ConcurrencyModels.find_override_or_superclass_annotated Annotations.ia_is_nonblocking tenv pname
  |> Option.is_some


let is_annotated_lockless tenv pname =
  let check annot = Annotations.(ia_ends_with annot lockless) in
  ConcurrencyModels.find_override_or_superclass_annotated check tenv pname |> Option.is_some


let executor_type_str = "java.util.concurrent.Executor"

let schedules_work =
  let open MethodMatcher in
  (* Some methods below belong to subclasses of [Executor]; we do check for an [Executor] base class. *)
  let matcher =
    [ { default with
        classname= executor_type_str
      ; methods= ["execute"; "schedule"; "scheduleAtFixedRate"; "scheduleWithFixedDelay"; "submit"]
      }
    ; { default with
        classname= "android.os.Handler"
      ; methods= ["post"; "postAtFrontOfQueue"; "postAtTime"; "postDelayed"] } ]
    |> of_records
  in
  fun tenv pname -> matcher tenv pname []


let schedules_first_arg_on_ui_thread =
  let open MethodMatcher in
  let matcher =
    { default with
      classname= "java.lang.Object"
    ; methods=
        [ "postOnUiThread"
        ; "postOnUiThreadDelayed"
        ; "postToUiThread"
        ; "runOnUiThread"
        ; "runOnUiThreadAsync"
        ; "runOnUiThreadAsyncWithDelay" ] }
    |> of_record
  in
  fun tenv pname -> matcher tenv pname []


let schedules_second_arg_on_ui_thread =
  let open MethodMatcher in
  let matcher =
    { default with
      classname= "android.view.View"
    ; methods= ["post"; "postDelayed"; "postOnAnimation"] }
    |> of_record
  in
  fun tenv pname -> matcher tenv pname []


let schedules_first_arg_on_bg_thread =
  let open MethodMatcher in
  let matcher =
    [ {default with classname= "java.lang.Object"; methods= ["scheduleGuaranteedDelayed"]}
    ; {default with classname= "java.lang.Thread"; methods= ["start"]} ]
    |> of_records
  in
  fun tenv pname -> matcher tenv pname []


type scheduler_thread_constraint = ForUIThread | ForNonUIThread | ForUnknownThread
[@@deriving equal]

(* Executors are sometimes stored in fields and annotated according to what type of thread
   they schedule work on.  Given an expression representing such a field, try to find the kind of
   annotation constraint, if any. *)
let rec get_executor_thread_annotation_constraint tenv (receiver : HilExp.AccessExpression.t) =
  match receiver with
  | FieldOffset (_, field_name) when Fieldname.is_java field_name ->
      Fieldname.get_class_name field_name
      |> Tenv.lookup tenv
      |> Option.map ~f:(fun (tstruct : Struct.t) -> tstruct.fields @ tstruct.statics)
      |> Option.bind ~f:(List.find ~f:(fun {Struct.name= fld} -> Fieldname.equal fld field_name))
      |> Option.bind ~f:(fun {Struct.annot} ->
             if Annotations.(ia_ends_with annot for_ui_thread) then Some ForUIThread
             else if Annotations.(ia_ends_with annot for_non_ui_thread) then Some ForNonUIThread
             else None )
  | Dereference prefix ->
      get_executor_thread_annotation_constraint tenv prefix
  | _ ->
      None


(* Given an object, find the [run] method in its class and return the procname, if any *)
let get_run_method_from_runnable tenv runnable =
  let run_like_methods = ["run"; "call"] in
  let is_run_method = function
    | Procname.Java pname when Procname.Java.(not (is_static pname)) ->
        (* confusingly, the parameter list in (non-static?) Java procnames does not contain [this] *)
        Procname.Java.(
          List.is_empty (get_parameters pname)
          &&
          let methodname = get_method pname in
          List.exists run_like_methods ~f:(String.equal methodname) )
    | _ ->
        false
  in
  HilExp.AccessExpression.get_typ runnable tenv
  |> Option.map ~f:(function Typ.{desc= Tptr (typ, _)} -> typ | typ -> typ)
  |> Option.bind ~f:Typ.name
  |> Option.bind ~f:(Tenv.lookup tenv)
  |> Option.map ~f:(fun (tstruct : Struct.t) -> tstruct.methods)
  |> Option.bind ~f:(List.find ~f:is_run_method)


(* Syntactically match for certain methods known to return executors. *)
let get_returned_executor tenv callee actuals =
  let type_check =
    lazy
      ( Attributes.load callee
      |> Option.exists ~f:(fun (attrs : ProcAttributes.t) ->
             match attrs.ret_type.Typ.desc with
             | Tstruct tname | Typ.Tptr ({desc= Tstruct tname}, _) ->
                 PatternMatch.is_subtype_of_str tenv tname executor_type_str
             | _ ->
                 false ) )
  in
  match (callee, actuals) with
  | Procname.Java java_pname, [] -> (
    match Procname.Java.get_method java_pname with
    | ("getForegroundExecutor" | "getBackgroundExecutor") when Lazy.force type_check ->
        Some ForNonUIThread
    | "getUiThreadExecutorService" when Lazy.force type_check ->
        Some ForUIThread
    | _ ->
        None )
  | _ ->
      None


let is_getMainLooper =
  let open MethodMatcher in
  of_record
    { default with
      classname= "android.os.Looper"
    ; methods= ["getMainLooper"]
    ; actuals_pred= List.is_empty }


let is_handler_constructor =
  let open MethodMatcher in
  of_record
    { default with
      classname= "android.os.Handler"
    ; methods= [Procname.Java.constructor_method_name]
    ; actuals_pred= (fun actuals -> not (List.is_empty actuals)) }


let is_thread_constructor =
  let open MethodMatcher in
  of_record
    { default with
      classname= "java.lang.Thread"
    ; search_superclasses= false
    ; methods= [Procname.Java.constructor_method_name] }


let is_assume_true =
  let open MethodMatcher in
  of_records
    [ { default with
        classname= "com.facebook.common.internal.Preconditions"
      ; methods= ["checkArgument"; "checkState"] }
    ; { default with
        classname= "com.facebook.infer.annotation.Assertions"
      ; methods= ["assertCondition"; "assumeCondition"] }
    ; { default with
        classname= "com.google.common.base.Preconditions"
      ; methods= ["checkArgument"; "checkState"] } ]


let is_java_main_method (pname : Procname.t) =
  let pointer_to_array_of_java_lang_string =
    Typ.(mk_ptr (mk_array StdTyp.Java.pointer_to_java_lang_string))
  in
  let check_main_args args =
    match args with [arg] -> Typ.equal pointer_to_array_of_java_lang_string arg | _ -> false
  in
  let test_pname pname =
    match (pname : Procname.t) with
    | C _ | Erlang _ | Hack _ | Block _ | ObjC_Cpp _ | CSharp _ | Python _ ->
        false
    | Java java_pname ->
        Procname.Java.is_static java_pname
        && String.equal "main" (Procname.get_method pname)
        && Typ.equal StdTyp.void (Procname.Java.get_return_typ java_pname)
        && check_main_args (Procname.Java.get_parameters java_pname)
  in
  test_pname pname


let may_execute_arbitrary_code =
  let open MethodMatcher in
  of_record
    {default with classname= "com.google.common.util.concurrent.SettableFuture"; methods= ["set"]}
