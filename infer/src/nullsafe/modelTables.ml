(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Hashtbl = Caml.Hashtbl

(*
 * This file is a big bunch of tables; they read better with really long lines.
 * @nolint
 *)

(* the type should be treated as Nonnull *)
let o = false

(* the type should be treated as Nullable *)
and n = true

(* create signature where both return type and params are nonnull *)
let all_nonnull num_params = (o, List.init num_params ~f:(fun _ -> o))

(* o<N> means signature with N nonnull params and nonnull return type *)

let o1 = all_nonnull 1

let o2 = all_nonnull 2

let o3 = all_nonnull 3

let o4 = all_nonnull 4

let o5 = all_nonnull 5

let o6 = all_nonnull 6

let o7 = all_nonnull 7

let o8 = all_nonnull 8

let o9 = all_nonnull 9

let o10 = all_nonnull 10

let o11 = all_nonnull 11

let o12 = all_nonnull 12

(* n<N> stands for signature with nonnull return type and N nullable params *)

let n1 = (o, [n])

let n2 = (o, [n; n])

let n3 = (o, [n; n; n])

(* the second argument is nullable, everything else is nonnull *)
let on = (o, [o; n])

(* container add *)
let ca = if Config.nullsafe_strict_containers then (o, [o]) else (o, [n])

(* container get *)
let cg = if Config.nullsafe_strict_containers then (n, [o]) else (n, [n])

(* container put *)
let cp = if Config.nullsafe_strict_containers then (n, [o; o]) else (n, [n; n])

(* container remove *)
let cr = if Config.nullsafe_strict_containers then (n, [o]) else (n, [n])

(* nullable getter *)
let ng = (n, [])

let check_not_null_parameter_list, check_not_null_list =
  (* The first integer in the tuple is the index (counting from 1) of the argument to be asserted.
     Commonly it is 1.
  *)
  let list =
    [ ( 1
      , (o, [n; n])
      , "com.facebook.common.internal.Preconditions.checkNotNull(java.lang.Object,java.lang.Object):java.lang.Object"
      )
    ; ( 1
      , (o, [n; n; n])
      , "com.facebook.common.internal.Preconditions.checkNotNull(java.lang.Object,java.lang.String,java.lang.Object[]):java.lang.Object"
      )
    ; ( 1
      , (o, [n])
      , "com.facebook.common.internal.Preconditions.checkNotNull(java.lang.Object):java.lang.Object"
      )
    ; ( 1
      , (o, [n; n])
      , "com.facebook.common.preconditions.Preconditions.checkNotNull(java.lang.Object,java.lang.String):java.lang.Object"
      )
    ; ( 1
      , (o, [n; n; n])
      , "com.facebook.common.preconditions.Preconditions.checkNotNull(java.lang.Object,java.lang.String,java.lang.Object[]):java.lang.Object"
      )
    ; ( 1
      , (o, [n])
      , "com.facebook.common.preconditions.Preconditions.checkNotNull(java.lang.Object):java.lang.Object"
      )
    ; ( 1
      , (o, [n; n])
      , "com.google.common.base.Preconditions.checkNotNull(java.lang.Object,java.lang.Object):java.lang.Object"
      )
    ; ( 1
      , (o, [n; n; n])
      , "com.google.common.base.Preconditions.checkNotNull(java.lang.Object,java.lang.String,java.lang.Object[]):java.lang.Object"
      )
    ; ( 1
      , (o, [n])
      , "com.google.common.base.Preconditions.checkNotNull(java.lang.Object):java.lang.Object" )
    ; (1, (o, [n]), "com.google.common.base.Verify.verifyNotNull(java.lang.Object):java.lang.Object")
    ; ( 1
      , (o, [n; n; n])
      , "com.google.common.base.Verify.verifyNotNull(java.lang.Object,java.lang.String,java.lang.Object[]):java.lang.Object"
      )
    ; (1, (o, [n]), "org.junit.Assert.assertNotNull(java.lang.Object):void")
    ; ( 2
      , (* a non-traditional method - the second parameter is the object to be asserted, the first is the description *)
        (o, [n; n])
      , "org.junit.Assert.assertNotNull(java.lang.String,java.lang.Object):void" )
    ; ( 1
      , (o, [n])
      , "com.facebook.infer.annotation.Assertions.assertNotNull(java.lang.Object):java.lang.Object"
      )
    ; ( 1
      , (o, [n; o])
      , "com.facebook.infer.annotation.Assertions.assertNotNull(java.lang.Object,java.lang.String):java.lang.Object"
      )
    ; ( 1
      , (o, [n])
      , "com.facebook.infer.annotation.Assertions.assumeNotNull(java.lang.Object):java.lang.Object"
      )
    ; ( 1
      , (o, [n; o])
      , "com.facebook.infer.annotation.Assertions.assumeNotNull(java.lang.Object,java.lang.String):java.lang.Object"
      )
    ; ( 1
      , (o, [n; o])
      , "com.facebook.infer.annotation.Assertions.nullsafeFIXME(java.lang.Object,java.lang.String):java.lang.Object"
      )
    ; ( 1
      , (o, [n])
      , "androidx.core.util.Preconditions.checkNotNull(java.lang.Object):java.lang.Object" )
    ; ( 1
      , (o, [n; n])
      , "androidx.core.util.Preconditions.checkNotNull(java.lang.Object,java.lang.Object):java.lang.Object"
      ) ]
  in
  (List.map ~f:(fun (x, _, z) -> (x, z)) list, List.map ~f:(fun (_, y, z) -> (y, z)) list)


let check_state_list =
  [ ((o, [o]), "Preconditions.checkState(boolean):void")
  ; ((o, [o]), "com.facebook.common.internal.Preconditions.checkState(boolean):void")
  ; ( (o, [o; n])
    , "com.facebook.common.internal.Preconditions.checkState(boolean,java.lang.Object):void" )
  ; ( (o, [o; n; n])
    , "com.facebook.common.internal.Preconditions.checkState(boolean,java.lang.String,java.lang.Object[]):void"
    )
  ; ((o, [o]), "com.facebook.common.preconditions.Preconditions.checkState(boolean):void")
  ; ( (o, [o; n])
    , "com.facebook.common.preconditions.Preconditions.checkState(boolean,java.lang.Object):void" )
  ; ( (o, [o; n; n])
    , "com.facebook.common.preconditions.Preconditions.checkState(boolean,java.lang.String,java.lang.Object[]):void"
    )
  ; ((o, [o]), "com.google.common.base.Preconditions.checkState(boolean):void")
  ; ((o, [o; n]), "com.google.common.base.Preconditions.checkState(boolean,java.lang.Object):void")
  ; ( (o, [o; n; n])
    , "com.google.common.base.Preconditions.checkState(boolean,java.lang.String,java.lang.Object[]):void"
    )
  ; ((o, [o]), "com.facebook.infer.annotation.Assertions.assertCondition(boolean):void")
  ; ( (o, [o; o])
    , "com.facebook.infer.annotation.Assertions.assertCondition(boolean,java.lang.String):void" )
  ; ((o, [o]), "com.facebook.infer.annotation.Assertions.assumeCondition(boolean):void")
  ; ( (o, [o; o])
    , "com.facebook.infer.annotation.Assertions.assumeCondition(boolean,java.lang.String):void" ) ]


let check_argument_list =
  [ ((o, [o]), "com.facebook.common.internal.Preconditions.checkArgument(boolean):void")
  ; ( (o, [o; n])
    , "com.facebook.common.internal.Preconditions.checkArgument(boolean,java.lang.Object):void" )
  ; ( (o, [o; n; n])
    , "com.facebook.common.internal.Preconditions.checkArgument(boolean,java.lang.String,java.lang.Object[]):void"
    )
  ; ((o, [o]), "com.facebook.common.preconditions.Preconditions.checkArgument(boolean):void")
  ; ( (o, [o; n])
    , "com.facebook.common.preconditions.Preconditions.checkArgument(boolean,java.lang.Object):void"
    )
  ; ( (o, [o; n; n])
    , "com.facebook.common.preconditions.Preconditions.checkArgument(boolean,java.lang.String,java.lang.Object[]):void"
    )
  ; ((o, [o]), "com.google.common.base.Preconditions.checkArgument(boolean):void")
  ; ( (o, [o; n])
    , "com.google.common.base.Preconditions.checkArgument(boolean,java.lang.Object):void" )
  ; ( (o, [o; n; n])
    , "com.google.common.base.Preconditions.checkArgument(boolean,java.lang.String,java.lang.Object[]):void"
    ) ]


(** Models for boolean functions that return true on null. *)
let true_on_null_list =
  [ (n1, "android.text.TextUtils.isEmpty(java.lang.CharSequence):boolean")
  ; (n1, "com.google.common.base.Strings.isNullOrEmpty(java.lang.String):boolean") ]


(** Models for Map.containsKey *)
let containsKey_list =
  [ (n1, "com.google.common.collect.ImmutableMap.containsKey(java.lang.Object):boolean")
  ; (n1, "java.util.Map.containsKey(java.lang.Object):boolean") ]


(** Models for Map.put *)
let mapPut_list =
  [ ( cp
    , "com.google.common.collect.ImmutableMap.put(java.lang.Object,java.lang.Object):java.lang.Object"
    )
  ; (cp, "java.util.Map.put(java.lang.Object,java.lang.Object):java.lang.Object") ]


type nonnull_alternative_method = {package_name: string; class_name: string; method_name: string}

(* Nullable methods that have non-nullable alternatives.
   Format is a triple: (<nullability>, <method>, <alternative>), *)
let nullable_methods_with_nonnull_alternatives_list =
  [ ( (n, [o])
    , "android.view.View.findViewById(int):android.view.View"
      (* View.requireViewById() is inaccessible from older APIs and will lead to a runtime crash *)
    , {package_name= "androidx.core.view"; class_name= "ViewCompat"; method_name= "requireViewById"}
    )
  ; ( (n, [o])
    , "android.app.Activity.findViewById(int):android.view.View"
    , { package_name= "androidx.core.app"
      ; class_name= "ActivityCompat"
      ; method_name= "requireViewById" } )
  ; ( (n, [])
    , "androidx.fragment.app.Fragment.getArguments():android.os.Bundle"
    , { package_name= "androidx.fragment.app"
      ; class_name= "Fragment"
      ; method_name= "requireArguments" } )
  ; ( (n, [])
    , "androidx.fragment.app.Fragment.getContext():android.content.Context"
    , {package_name= "androidx.fragment.app"; class_name= "Fragment"; method_name= "requireContext"}
    )
  ; ( (n, [])
    , "androidx.fragment.app.Fragment.getActivity():androidx.fragment.app.FragmentActivity"
    , {package_name= "androidx.fragment.app"; class_name= "Fragment"; method_name= "requireActivity"}
    )
  ; ( (n, [])
    , "androidx.fragment.app.Fragment.getHost():java.lang.Object"
    , {package_name= "androidx.fragment.app"; class_name= "Fragment"; method_name= "requireHost"} )
  ; ( (n, [])
    , "androidx.fragment.app.Fragment.getParentFragment():androidx.fragment.app.Fragment"
    , { package_name= "androidx.fragment.app"
      ; class_name= "Fragment"
      ; method_name= "requireParentFragment" } )
  ; ( (n, [])
    , "androidx.fragment.app.Fragment.getView():android.view.View"
    , {package_name= "androidx.fragment.app"; class_name= "Fragment"; method_name= "requireView"} )
  ]


let nullable_method_with_nonnull_alternatives_nullability_list =
  let result =
    List.map nullable_methods_with_nonnull_alternatives_list
      ~f:(fun (nullability, method_descr, _) -> (nullability, method_descr))
  in
  List.iter result ~f:(fun ((ret_nullability, _param_nullability), _) ->
      if not (Bool.equal ret_nullability n) then
        Logging.die Logging.InternalError "Function is expected to be nullable" ) ;
  result


(* Methods with signatures that are not special enough to be described in one of lists above *)
let annotated_list_nullability_other =
  [ (o1, "android.text.SpannableString.valueOf(java.lang.CharSequence):android.text.SpannableString")
  ; (o1, "android.app.AlarmManager.cancel(android.app.PendingIntent):void")
  ; (o1, "android.net.Uri.parse(java.lang.String):android.net.Uri")
  ; (n1, "android.os.Parcel.writeList(java.util.List):void")
  ; (n2, "android.os.Parcel.writeParcelable(android.os.Parcelable,int):void")
  ; (n1, "android.os.Parcel.writeString(java.lang.String):void")
  ; ( (o, [o; o])
    , "androidx.core.view.ViewCompat.requireViewById(android.view.View,int):android.view.View" )
  ; ( (o, [o; o])
    , "androidx.core.app.ActivityCompat.requireViewById(android.app.Activity,int):android.view.View"
    )
  ; ((o, []), "androidx.fragment.app.Fragment.requireArguments():android.os.Bundle")
  ; ((o, []), "androidx.fragment.app.Fragment.requireContext():android.content.Context")
  ; ( (o, [])
    , "androidx.fragment.app.Fragment.requireActivity():androidx.fragment.app.FragmentActivity" )
  ; ((o, []), "androidx.fragment.app.Fragment.requireHost():java.lang.Object")
  ; ( (o, [])
    , "androidx.fragment.app.Fragment.requireParentFragment():androidx.fragment.app.Fragment" )
  ; ((o, []), "androidx.fragment.app.Fragment.requireView():android.view.View")
  ; ( (o, [o; o; n; n; n])
    , "com.android.sdklib.build.ApkBuilder.<init>(java.io.File,java.io.File,java.io.File,java.lang.String,java.io.PrintStream)"
    )
  ; ( (o, [n])
    , "com.android.manifmerger.ManifestMerger.xmlFileAndLine(org.w3c.dom.Node):com.android.manifmerger.IMergerLog$FileAndLine"
    )
  ; ( on
    , "com.android.util.CommandLineParser$Mode.process(com.android.util.CommandLineParser$Arg,java.lang.String):java.lang.Object"
    )
    (* The following FbInjector functions are not being called explicitly, they are artifacts of DI framework.
       They are technically @Nullable, but the case when they are returning null is rather exceptional,
       so the right tradeoff here is to treat them as non-nullables.
    *)
  ; ( (o, [o; o; n])
    , "com.facebook.inject.FbInjector.lazyInstance(int,int,com.facebook.inject.InjectionContext):java.lang.Object"
    )
  ; ( (o, [o; o; n])
    , "com.facebook.inject.FbInjector.lazyApplicationInstance(int,int,com.facebook.inject.InjectionContext):java.lang.Object"
    )
  ; ( (o, [o; n])
    , "com.facebook.inject.FbInjector.localInstance(int,com.facebook.inject.InjectionContext):java.lang.Object"
    )
  ; ( on
    , "com.google.common.base.Objects$ToStringHelper.add(java.lang.String,java.lang.Object):com.google.common.base.Objects$ToStringHelper"
    )
  ; (n2, "com.google.common.base.Objects.equal(java.lang.Object,java.lang.Object):boolean")
  ; ( n1
    , "com.google.common.base.Optional.fromNullable(java.lang.Object):com.google.common.base.Optional"
    )
  ; ((n, []), "com.google.common.base.Optional.orNull():java.lang.Object")
  ; (n1, "com.google.common.base.Strings.nullToEmpty(java.lang.String):java.lang.String")
  ; (cg, "com.google.common.collect.ImmutableMap.get(java.lang.Object):java.lang.Object")
  ; (* container get *)
    ( o1
    , "com.google.common.collect.ImmutableList$Builder.add(java.lang.Object):com.google.common.collect.ImmutableList$Builder"
    )
  ; ( o1
    , "com.google.common.collect.ImmutableList$Builder.addAll(java.lang.Iterable):com.google.common.collect.ImmutableList$Builder"
    )
  ; ( o1
    , "com.google.common.collect.ImmutableList.of(java.lang.Object):com.google.common.collect.ImmutableList"
    )
  ; ( o2
    , "com.google.common.collect.ImmutableList.of(java.lang.Object,java.lang.Object):com.google.common.collect.ImmutableList"
    )
  ; ( o3
    , "com.google.common.collect.ImmutableList.of(java.lang.Object,java.lang.Object,java.lang.Object):com.google.common.collect.ImmutableList"
    )
  ; ( o4
    , "com.google.common.collect.ImmutableList.of(java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object):com.google.common.collect.ImmutableList"
    )
  ; ( o5
    , "com.google.common.collect.ImmutableList.of(java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object):com.google.common.collect.ImmutableList"
    )
  ; ( o6
    , "com.google.common.collect.ImmutableList.of(java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object):com.google.common.collect.ImmutableList"
    )
  ; ( o7
    , "com.google.common.collect.ImmutableList.of(java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object):com.google.common.collect.ImmutableList"
    )
  ; ( o8
    , "com.google.common.collect.ImmutableList.of(java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object):com.google.common.collect.ImmutableList"
    )
  ; ( o9
    , "com.google.common.collect.ImmutableList.of(java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object):com.google.common.collect.ImmutableList"
    )
  ; ( o10
    , "com.google.common.collect.ImmutableList.of(java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object):com.google.common.collect.ImmutableList"
    )
  ; ( o11
    , "com.google.common.collect.ImmutableList.of(java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object):com.google.common.collect.ImmutableList"
    )
  ; ( o12
    , "com.google.common.collect.ImmutableList.of(java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object):com.google.common.collect.ImmutableList"
    )
  ; ( o1
    , "com.google.common.collect.ImmutableList.copyOf(java.lang.Iterable):com.google.common.collect.ImmutableList"
    )
  ; ( o1
    , "com.google.common.collect.ImmutableList.copyOf(java.util.Collection):com.google.common.collect.ImmutableList"
    )
  ; ( o1
    , "com.google.common.collect.ImmutableList.copyOf(java.util.Iterator):com.google.common.collect.ImmutableList"
    )
  ; ( o1
    , "com.google.common.collect.ImmutableList.copyOf(java.lang.Object):com.google.common.collect.ImmutableList"
    )
  ; ( o2
    , "com.google.common.collect.ImmutableList.sortedCopyOf(java.util.Comparator,java.lang.Iterable):com.google.common.collect.ImmutableList"
    )
  ; ( o1
    , "com.google.common.collect.ImmutableSet.of(java.lang.Object):com.google.common.collect.ImmutableSet"
    )
  ; ( o2
    , "com.google.common.collect.ImmutableSet.of(java.lang.Object,java.lang.Object):com.google.common.collect.ImmutableSet"
    )
  ; ( o3
    , "com.google.common.collect.ImmutableSet.of(java.lang.Object,java.lang.Object,java.lang.Object):com.google.common.collect.ImmutableSet"
    )
  ; ( o4
    , "com.google.common.collect.ImmutableSet.of(java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object):com.google.common.collect.ImmutableSet"
    )
  ; ( o5
    , "com.google.common.collect.ImmutableSet.of(java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object):com.google.common.collect.ImmutableSet"
    )
  ; ( o1
    , "com.google.common.collect.ImmutableSet.copyOf(java.lang.Iterable):com.google.common.collect.ImmutableSet"
    )
  ; ( o1
    , "com.google.common.collect.ImmutableSet.copyOf(java.util.Collection):com.google.common.collect.ImmutableSet"
    )
  ; ( o1
    , "com.google.common.collect.ImmutableSet.copyOf(java.util.Iterator):com.google.common.collect.ImmutableSet"
    )
  ; ( o1
    , "com.google.common.collect.ImmutableSet.copyOf(java.lang.Object):com.google.common.collect.ImmutableSet"
    )
  ; ( o1
    , "com.google.common.collect.ImmutableSortedSet$Builder.add(java.lang.Object):com.google.common.collect.ImmutableSortedSet$Builder"
    )
  ; ( o2
    , "com.google.common.collect.ImmutableMap.of(java.lang.Object,java.lang.Object):com.google.common.collect.ImmutableMap"
    )
  ; ( o4
    , "com.google.common.collect.ImmutableMap.of(java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object):com.google.common.collect.ImmutableMap"
    )
  ; ( o6
    , "com.google.common.collect.ImmutableMap.of(java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object):com.google.common.collect.ImmutableMap"
    )
  ; ( o8
    , "com.google.common.collect.ImmutableMap.of(java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object):com.google.common.collect.ImmutableMap"
    )
  ; ( o10
    , "com.google.common.collect.ImmutableMap.of(java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object,java.lang.Object):com.google.common.collect.ImmutableMap"
    )
  ; ( o1
    , "com.google.common.collect.ImmutableMap.copyOf(java.util.Map):com.google.common.collect.ImmutableMap"
    )
  ; ( o1
    , "com.google.common.collect.ImmutableMap.copyOf(java.lang.Iterable):com.google.common.collect.ImmutableMap"
    )
  ; ( o1
    , "com.google.common.util.concurrent.SettableFuture.setException(java.lang.Throwable):boolean"
    )
  ; (o1, "java.io.File.<init>(java.lang.String)")
  ; (n1, "java.io.PrintStream.print(java.lang.String):void")
  ; ((n, [o]), "java.lang.Class.getResource(java.lang.String):java.net.URL")
  ; (o1, "java.lang.Class.isAssignableFrom(java.lang.Class):boolean")
  ; (n1, "java.lang.Integer.equals(java.lang.Object):boolean")
  ; (o1, "java.lang.Integer.parseInt(java.lang.String):int")
  ; (o1, "java.lang.Long.parseLong(java.lang.String):long")
  ; (n1, "java.lang.Object.equals(java.lang.Object):boolean")
  ; (n2, "java.lang.RuntimeException.<init>(java.lang.String,java.lang.Throwable)")
  ; (n1, "java.lang.String.equals(java.lang.Object):boolean")
  ; (n1, "java.lang.StringBuilder.append(java.lang.String):java.lang.StringBuilder")
  ; ((n, [o]), "java.lang.System.getProperty(java.lang.String):java.lang.String")
  ; ((n, [o]), "java.lang.System.getenv(java.lang.String):java.lang.String")
  ; ( on
    , "java.net.URLClassLoader.newInstance(java.net.URL[],java.lang.ClassLoader):java.net.URLClassLoader"
    )
  ; (ng, "java.nio.file.Path.getParent():java.nio.file.Path")
  ; (n1, "java.util.AbstractList.equals(java.lang.Object):boolean")
  ; (ca, "java.util.ArrayList.add(java.lang.Object):boolean")
  ; (ca, "java.util.List.add(java.lang.Object):boolean")
  ; (cg, "java.util.Map.get(java.lang.Object):java.lang.Object")
  ; (cr, "java.util.Map.remove(java.lang.Object):java.lang.Object")
  ; (cp, "java.util.Map.put(java.lang.Object,java.lang.Object):java.lang.Object")
  ; (cg, "java.util.HashMap.get(java.lang.Object):java.lang.Object")
  ; (cr, "java.util.HashMap.remove(java.lang.Object):java.lang.Object")
  ; (cp, "java.util.HashMap.put(java.lang.Object,java.lang.Object):java.lang.Object")
  ; (cg, "java.util.concurrent.ConcurrentHashMap.get(java.lang.Object):java.lang.Object")
  ; (cr, "java.util.concurrent.ConcurrentHashMap.remove(java.lang.Object):java.lang.Object")
  ; (cg, "java.util.AbstractMap.get(java.lang.Object):java.lang.Object")
  ; (cr, "java.util.AbstractMap.remove(java.lang.Object):java.lang.Object")
  ; (cg, "java.util.concurrent.ConcurrentSkipListMap.get(java.lang.Object):java.lang.Object")
  ; (cr, "java.util.concurrent.ConcurrentSkipListMap.remove(java.lang.Object):java.lang.Object")
  ; (cg, "java.util.EnumMap.get(java.lang.Object):java.lang.Object")
  ; (cr, "java.util.EnumMap.remove(java.lang.Object):java.lang.Object")
  ; (cg, "java.util.Hashtable.get(java.lang.Object):java.lang.Object")
  ; (cr, "java.util.Hashtable.remove(java.lang.Object):java.lang.Object")
  ; (cg, "java.util.IdentityHashMap.get(java.lang.Object):java.lang.Object")
  ; (cr, "java.util.IdentityHashMap.remove(java.lang.Object):java.lang.Object")
  ; (cg, "java.util.LinkedHashMap.get(java.lang.Object):java.lang.Object")
  ; (cr, "java.util.LinkedHashMap.remove(java.lang.Object):java.lang.Object")
  ; (cg, "java.util.TreeMap.get(java.lang.Object):java.lang.Object")
  ; (cr, "java.util.TreeMap.remove(java.lang.Object):java.lang.Object")
  ; (cg, "java.util.WeakHashMap.get(java.lang.Object):java.lang.Object")
  ; (cr, "java.util.WeakHashMap.remove(java.lang.Object):java.lang.Object")
  ; ( (n, [o])
    , "javax.lang.model.element.Element.getAnnotation(java.lang.Class):java.lang.annotation.Annotation"
    )
  ; (ng, "javax.lang.model.element.Element.getEnclosingElement():javax.lang.model.element.Element")
  ; ( ng
    , "javax.lang.model.element.ExecutableElement.getDefaultValue():javax.lang.model.element.AnnotationValue"
    )
  ; ( ng
    , "javax.lang.model.element.PackageElement.getEnclosingElement():javax.lang.model.element.Element"
    )
  ; (ng, "javax.lang.model.element.VariableElement.getConstantValue():java.lang.Object")
  ; (ng, "javax.lang.model.type.WildcardType.getSuperBound():javax.lang.model.type.TypeMirror")
  ; ( (n, [o])
    , "javax.lang.model.util.Elements.getPackageElement(java.lang.CharSequence):javax.lang.model.element.PackageElement"
    )
  ; ( (n, [o])
    , "javax.lang.model.util.Elements.getTypeElement(java.lang.CharSequence):javax.lang.model.element.TypeElement"
    )
  ; ( (n, [o])
    , "javax.lang.model.util.Elements.getDocComment(javax.lang.model.element.Element):java.lang.String"
    )
  ; ( o1
    , "javax.lang.model.util.Elements.getElementValuesWithDefaults(javax.lang.model.element.AnnotationMirror):java.util.Map"
    )
  ; (o1, "javax.lang.model.util.Elements.isDeprecated(javax.lang.model.element.Element):boolean")
  ; ( o1
    , "javax.lang.model.util.Elements.getBinaryName(javax.lang.model.element.TypeElement):javax.lang.model.element.Name"
    )
  ; ( o1
    , "javax.lang.model.util.Elements.getPackageOf(javax.lang.model.element.Element):javax.lang.model.element.PackageElement"
    )
  ; ( o1
    , "javax.lang.model.util.Elements.getAllMembers(javax.lang.model.element.TypeElement):java.util.List"
    )
  ; ( o1
    , "javax.lang.model.util.Elements.getAllAnnotationMirrors(javax.lang.model.element.Element):java.util.List"
    )
  ; ( o2
    , "javax.lang.model.util.Elements.hides(javax.lang.model.element.Element, \
       javax.lang.model.element.Element):boolean" )
  ; ( o3
    , "javax.lang.model.util.Elements.overrides(javax.lang.model.element.ExecutableElement, \
       javax.lang.model.element.ExecutableElement, javax.lang.model.element.TypeElement):boolean" )
  ; ( o1
    , "javax.lang.model.util.Types.asElement(javax.lang.model.type.TypeMirror):javax.lang.model.element.Element"
    )
  ; ( o2
    , "javax.lang.model.util.Types.isSameType(javax.lang.model.type.TypeMirror, \
       javax.lang.model.type.TypeMirror):boolean" )
  ; ( o2
    , "javax.lang.model.util.Types.isSubtype(javax.lang.model.type.TypeMirror, \
       javax.lang.model.type.TypeMirror):boolean" )
  ; ( o2
    , "javax.lang.model.util.Types.isAssignable(javax.lang.model.type.TypeMirror, \
       javax.lang.model.type.TypeMirror):boolean" )
  ; ( o2
    , "javax.lang.model.util.Types.contains(javax.lang.model.type.TypeMirror, \
       javax.lang.model.type.TypeMirror):boolean" )
  ; ( o2
    , "javax.lang.model.util.Types.isSubsignature(javax.lang.model.type.ExecutableType, \
       javax.lang.model.type.ExecutableType):boolean" )
  ; ( o1
    , "javax.lang.model.util.Types.directSupertypes(javax.lang.model.type.TypeMirror):java.util.List"
    )
  ; ( o1
    , "javax.lang.model.util.Types.erasure(javax.lang.model.type.TypeMirror):javax.lang.model.type.TypeMirror"
    )
  ; ( o1
    , "javax.lang.model.util.Types.boxedClass(javax.lang.model.type.PrimitiveType):javax.lang.model.element.TypeElement"
    )
  ; ( o1
    , "javax.lang.model.util.Types.unboxedType(javax.lang.model.type.TypeMirror):javax.lang.model.type.PrimitiveType"
    )
  ; ( o1
    , "javax.lang.model.util.Types.capture(javax.lang.model.type.TypeMirror):javax.lang.model.type.TypeMirror"
    )
  ; ( o1
    , "javax.lang.model.util.Types.getArrayType(javax.lang.model.type.TypeMirror):javax.lang.model.type.ArrayType"
    )
  ; ( o2
    , "javax.lang.model.util.Types.getWildcardType(javax.lang.model.type.TypeMirror, \
       javax.lang.model.type.TypeMirror):javax.lang.model.type.WildcardType" )
  ; ( o2
    , "javax.lang.model.util.Types.getDeclaredType(javax.lang.model.element.TypeElement, \
       javax.lang.model.type.TypeMirror[]):javax.lang.model.type.DeclaredType" )
  ; ( o3
    , "javax.lang.model.util.Types.getDeclaredType(javax.lang.model.type.DeclaredType, \
       javax.lang.model.element.TypeElement, \
       javax.lang.model.type.TypeMirror[]):javax.lang.model.type.DeclaredType" )
  ; ( o2
    , "javax.lang.model.util.Types.asMemberOf(javax.lang.model.type.DeclaredType, \
       javax.lang.model.element.Element):javax.lang.model.type.TypeMirror" )
  ; ( n3
    , "javax.tools.JavaCompiler.getStandardFileManager(javax.tools.DiagnosticListener,java.util.Locale,java.nio.charset.Charset):javax.tools.StandardJavaFileManager"
    )
  ; (ng, "javax.tools.JavaFileObject.getAccessLevel():javax.lang.model.element.Modifier")
  ; (ng, "javax.tools.JavaFileObject.getNestingKind():javax.lang.model.element.NestingKind")
  ; ( o2
    , "com.sun.source.util.SourcePositions.getStartPosition(com.sun.source.tree.CompilationUnitTree, \
       com.sun.source.tree.Tree):long" )
  ; ( o2
    , "com.sun.source.util.SourcePositions.getEndPosition(com.sun.source.tree.CompilationUnitTree, \
       com.sun.source.tree.Tree):long" )
  ; ( (n, [o; o])
    , "com.sun.source.util.TreePath.getPath(com.sun.source.tree.CompilationUnitTree, \
       com.sun.source.tree.Tree):com.sun.source.util.TreePath" )
  ; ( (n, [o; o])
    , "com.sun.source.util.TreePath.getPath(com.sun.source.util.TreePath, \
       com.sun.source.tree.Tree):com.sun.source.util.TreePath" )
  ; ( (n, [o])
    , "com.sun.source.util.Trees.getTree(javax.lang.model.element.Element):com.sun.source.tree.Tree"
    )
  ; ( (n, [o])
    , "com.sun.source.util.Trees.getTree(javax.lang.model.element.TypeElement):com.sun.source.tree.ClassTree"
    )
  ; ( (n, [o])
    , "com.sun.source.util.Trees.getTree(javax.lang.model.element.ExecutableElement):com.sun.source.tree.MethodTree"
    )
  ; ( (n, [o; o])
    , "com.sun.source.util.Trees.getTree(javax.lang.model.element.Element, \
       javax.lang.model.element.AnnotationMirror):com.sun.source.tree.Tree" )
  ; ( (n, [o; o; o])
    , "com.sun.source.util.Trees.getTree(javax.lang.model.element.Element, \
       javax.lang.model.element.AnnotationMirror, \
       javax.lang.model.element.AnnotationValue):com.sun.source.tree.Tree" )
  ; ( o2
    , "com.sun.source.util.Trees.getPath(com.sun.source.tree.CompilationUnitTree, \
       com.sun.source.tree.Tree):com.sun.source.util.TreePath" )
  ; ( (n, [o])
    , "com.sun.source.util.Trees.getPath(javax.lang.model.element.Element):com.sun.source.util.TreePath"
    )
  ; ( (n, [o; o])
    , "com.sun.source.util.Trees.getPath(javax.lang.model.element.Element, \
       javax.lang.model.element.AnnotationMirror):com.sun.source.util.TreePath" )
  ; ( (n, [o; o; o])
    , "com.sun.source.util.Trees.getPath(javax.lang.model.element.Element, \
       javax.lang.model.element.AnnotationMirror, \
       javax.lang.model.element.AnnotationValue):com.sun.source.util.TreePath" )
  ; ( (n, [o])
    , "com.sun.source.util.Trees.getElement(com.sun.source.util.TreePath):javax.lang.model.element.Element"
    )
  ; ( (n, [o])
    , "com.sun.source.util.Trees.getTypeMirror(com.sun.source.util.TreePath):javax.lang.model.type.TypeMirror"
    )
  ; ( (n, [o])
    , "com.sun.source.util.Trees.getScope(com.sun.source.util.TreePath):com.sun.source.tree.Scope"
    )
  ; ( (n, [o])
    , "com.sun.source.util.Trees.getDocComment(com.sun.source.util.TreePath):java.lang.String" )
  ; ( o2
    , "com.sun.source.util.Trees.isAccessible(com.sun.source.tree.Scope, \
       javax.lang.model.element.TypeElement):boolean" )
  ; ( o3
    , "com.sun.source.util.Trees.isAccessible(com.sun.source.tree.Scope, \
       javax.lang.model.element.Element, javax.lang.model.type.DeclaredType):boolean" )
  ; ( o1
    , "com.sun.source.util.Trees.getOriginalType(javax.lang.model.type.ErrorType):javax.lang.model.type.TypeMirror"
    )
  ; ( (o, [o; o; o; o])
    , "com.sun.source.util.Trees.printMessage(javax.tools.Diagnostic.Kind, java.lang.CharSequence, \
       com.sun.source.tree.Tree, com.sun.source.tree.CompilationUnitTree):void" )
  ; ( o1
    , "com.sun.source.util.Trees.getLub(com.sun.source.tree.CatchTree):javax.lang.model.type.TypeMirror"
    )
  ; ( (n, [o; n; n])
    , "org.w3c.dom.Document.setUserData(java.lang.String,java.lang.Object,org.w3c.dom.UserDataHandler):java.lang.Object"
    )
  ; ( (n, [o; n; n])
    , "org.w3c.dom.Node.setUserData(java.lang.String,java.lang.Object,org.w3c.dom.UserDataHandler):java.lang.Object"
    )
  ; (* References *)
    (ng, "java.lang.ref.Reference.get():java.lang.Object")
  ; (ng, "java.lang.ref.PhantomReference.get():java.lang.Object")
  ; (ng, "java.lang.ref.SoftReference.get():java.lang.Object")
  ; (ng, "java.lang.ref.WeakReference.get():java.lang.Object")
  ; (ng, "java.util.concurrent.atomic.AtomicReference.get():java.lang.Object") ]


(** Models for nullability *)
let annotated_list_nullability =
  let result =
    check_not_null_list @ check_state_list @ check_argument_list @ true_on_null_list
    @ nullable_method_with_nonnull_alternatives_nullability_list @ annotated_list_nullability_other
  in
  List.find_a_dup result ~compare:(fun (_, descr1) (_, descr2) -> String.compare descr1 descr2)
  |> Option.iter ~f:(fun (_, duplicate_method_descr) ->
         Logging.die Logging.InternalError "Nullability table contains a duplicate %s"
           duplicate_method_descr ) ;
  result


(** Models for methods that do not return *)
let noreturn_list = [((o, [o]), "java.lang.System.exit(int):void")]

type model_table_t = (string, bool * bool list) Hashtbl.t

let mk_table list =
  let map = Hashtbl.create 1 in
  List.iter ~f:(function v, pn_id -> Hashtbl.replace map pn_id v) list ;
  map


let annotated_table_nullability = mk_table annotated_list_nullability

let check_not_null_table, check_not_null_parameter_table =
  (mk_table check_not_null_list, mk_table check_not_null_parameter_list)


let check_state_table = mk_table check_state_list

let check_argument_table = mk_table check_argument_list

let containsKey_table = mk_table containsKey_list

let mapPut_table = mk_table mapPut_list

let noreturn_table = mk_table noreturn_list

let true_on_null_table = mk_table true_on_null_list

let nonnull_alternatives_table =
  let method_descr_to_alternative =
    List.map nullable_methods_with_nonnull_alternatives_list
      ~f:(fun (_, method_descr, alternative) -> (alternative, method_descr))
  in
  mk_table method_descr_to_alternative


let field_nullability =
  [ ("android.content.pm.ApplicationInfo.dataDir", o)
  ; ("android.content.pm.ApplicationInfo.deviceProtectedDataDir", o)
  ; ("android.content.pm.ApplicationInfo.nativeLibraryDir", o)
  ; ("android.content.pm.ApplicationInfo.publicSourceDir", o)
  ; ("android.content.pm.ApplicationInfo.sourceDir", o)
  ; ("android.content.pm.ApplicationInfo.storageUuid", o)
  ; ( "android.content.pm.ApplicationInfo.processName"
    , o (* From the "process" attribute or, if not set, the same as packageName *) )
  ; ( "android.content.pm.PackageInfo.activities"
    , n (* Array of all <activity> tags included under <application>, or null if there were none *)
    )
  ; ( "android.content.pm.PackageInfo.applicationInfo"
    , n (* Information collected from the <application> tag, or null if there was none. *) )
  ; ("android.content.pm.PackageInfo.packageName", o)
  ; ("android.content.pm.PackageInfo.signatures", o)
  ; ( "android.content.pm.PackageInfo.services"
    , n (* Array of all <service> tags included under <application>, or null if there were none. *)
    )
  ; ("android.content.pm.PackageInfo.versionName", o)
  ; ( "android.content.pm.PackageItemInfo.metaData"
    , n
      (* This field will only be filled in if you set the PackageManager#GET_META_DATA flag when requesting the info *)
    )
  ; ("android.content.pm.PackageItemInfo.name", o)
  ; ("android.content.pm.PackageItemInfo.packageName", o)
  ; ( "android.content.pm.ResolveInfo.activityInfo"
    , n (* Exactly one of activityInfo, serviceInfo, or providerInfo will be non-null. *) )
  ; ( "android.content.pm.ResolveInfo.serviceInfo"
    , n (* Exactly one of activityInfo, serviceInfo, or providerInfo will be non-null. *) )
  ; ( "android.content.pm.ResolveInfo.providerInfo"
    , n (* Exactly one of activityInfo, serviceInfo, or providerInfo will be non-null. *) )
  ; ("android.content.res.Configuration.locale", o)
  ; ("android.graphics.Paint.Align.CENTER", o)
  ; ("android.graphics.Paint.Align.LEFT", o)
  ; ("android.graphics.Paint.Align.RIGHT", o)
  ; ("android.graphics.Paint.Cap.BUTT", o)
  ; ("android.graphics.Paint.Cap.ROUND", o)
  ; ("android.graphics.Paint.Cap.SQUARE", o)
  ; ("android.graphics.Paint.Join.BEVEL", o)
  ; ("android.graphics.Paint.Join.MITER", o)
  ; ("android.graphics.Paint.Join.ROUND", o)
  ; ("android.graphics.Paint.Style.FILL", o)
  ; ("android.graphics.Paint.Style.FILL_AND_STROKE", o)
  ; ("android.graphics.Paint.Style.STROKE", o)
  ; ("android.graphics.Typeface.BOLD", o)
  ; ("android.graphics.Typeface.DEFAULT", o)
  ; ("android.graphics.Typeface.DEFAULT_BOLD", o)
  ; ("android.graphics.Typeface.MONOSPACE", o)
  ; ("android.graphics.Typeface.SANS_SERIF", o)
  ; ("android.graphics.Typeface.SERIF", o)
  ; ("android.hardware.SensorEvent.values", o)
  ; ("android.net.Uri.EMPTY", o)
  ; ("android.provider.MediaStore$Images$Media.EXTERNAL_CONTENT_URI", o)
  ; ("android.provider.MediaStore$Images$Media.INTERNAL_CONTENT_URI", o)
  ; ("android.provider.Settings$Global.CONTENT_URI", o)
  ; ("android.provider.Settings$Secure.CONTENT_URI", o)
  ; ("android.provider.Settings$System.CONTENT_URI", o)
  ; ("android.provider.Settings$System.DEFAULT_ALARM_ALERT_URI", o)
  ; ("android.provider.Settings$System.DEFAULT_NOTIFICATION_URI", o)
  ; ("android.provider.Settings$System.DEFAULT_RINGTONE_URI", o)
  ; ("android.opengl.EGL10.EGL_NO_CONTEXT", o)
  ; ("android.opengl.EGL10.EGL_NO_DISPLAY", o)
  ; ("android.opengl.EGL10.EGL_NO_SURFACE", o)
  ; ("android.opengl.EGL14.EGL_NO_CONTEXT", o)
  ; ("android.opengl.EGL14.EGL_NO_DISPLAY", o)
  ; ("android.opengl.EGL14.EGL_NO_SURFACE", o)
  ; ("android.os.Bundle.EMPTY", o)
  ; ("android.os.Build.BOARD", o)
  ; ("android.os.Build.BOOTLOADER", o)
  ; ("android.os.Build.BRAND", o)
  ; ("android.os.Build.DEVICE", o)
  ; ("android.os.Build.DISPLAY", o)
  ; ("android.os.Build.FINGERPRINT", o)
  ; ("android.os.Build.HARDWARE", o)
  ; ("android.os.Build.HOST", o)
  ; ("android.os.Build.ID", o)
  ; ("android.os.Build.MANUFACTURER", o)
  ; ("android.os.Build.MODEL", o)
  ; ("android.os.Build.PRODUCT", o)
  ; ("android.os.Build.TAGS", o)
  ; ("android.os.Build.TYPE", o)
  ; ("android.os.Build.USER", o)
  ; ("android.os.Build.SERIAL", o)
  ; ("android.os.Build.SUPPORTED_ABIS", o)
  ; ( "android.os.Message.obj"
    , n (* An extra object going with the message - null if was not attached *) )
  ; ( "android.util.Pair.first"
    , o
      (* Deliberate UNSOUNDNESS. Since we don't support annotations in generic params,
         for the sake of usability we consider Pair to be non-nullable,
         which is true in majority of usages.
         This is a concious decision: Pair is a useful utility class, and without this
         it would be barely usable in practice.
         This is also on par with our policy for arrays and other containers, which are considered
         non-nullable.
      *) )
  ; ("android.net.wifi.WifiConfiguration.SSID", o)
  ; ("android.util.Pair.second", o)
  ; ("android.util.Patterns.DOMAIN_NAME", o)
  ; ("android.util.Patterns.EMAIL_ADDRESS", o)
  ; ("android.util.Patterns.IP_ADDRESS", o)
  ; ("android.util.Patterns.PHONE", o)
  ; ("android.util.Patterns.WEB_URL", o)
  ; ("android.view.View.ALPHA", o)
  ; ("android.view.View.ROTATION", o)
  ; ("android.view.View.ROTATION_X", o)
  ; ("android.view.View.ROTATION_Y", o)
  ; ("android.view.View.SCALE_X", o)
  ; ("android.view.View.SCALE_Y", o)
  ; ("android.view.View.TRANSLATION_X", o)
  ; ("android.view.View.TRANSLATION_Y", o)
  ; ("android.view.View.TRANSLATION_Z", o)
  ; ("android.view.View.X", o)
  ; ("android.view.View.Y", o)
  ; ("android.view.View.Z", o)
  ; ("androidx.core.text.TextDirectionHeuristicsCompat.ANYRTL_LTR", o)
  ; ("androidx.core.text.TextDirectionHeuristicsCompat.FIRSTSTRONG_LTR", o)
  ; ("androidx.core.text.TextDirectionHeuristicsCompat.FIRSTSTRONG_RTL", o)
  ; ("androidx.core.text.TextDirectionHeuristicsCompat.LOCALE", o)
  ; ("androidx.core.text.TextDirectionHeuristicsCompat.LTR", o)
  ; ("androidx.core.text.TextDirectionHeuristicsCompat.RTL", o)
  ; ("java.lang.Boolean.FALSE", o)
  ; ("java.lang.Boolean.TRUE", o)
  ; ("java.lang.Boolean.TYPE", o)
  ; ("java.lang.Integer.TYPE", o)
  ; ("java.lang.Void.TYPE", o)
  ; ("java.lang.Float.TYPE", o)
  ; ("java.lang.Long.TYPE", o)
  ; ("java.lang.Short.TYPE", o)
  ; ("java.lang.Double.TYPE", o)
  ; ("java.lang.Byte.TYPE", o)
  ; ("java.lang.System.in", o)
  ; ("java.lang.System.out", o)
  ; ("java.lang.System.err", o)
  ; ("java.nio.ByteOrder.BIG_ENDIAN", o)
  ; ("java.nio.ByteOrder.LITTLE_ENDIAN", o)
  ; ("java.nio.charset.StandardCharsets.ISO_8859_1", o)
  ; ("java.nio.charset.StandardCharsets.US_ASCII", o)
  ; ("java.nio.charset.StandardCharsets.UTF_16", o)
  ; ("java.nio.charset.StandardCharsets.UTF_16BE", o)
  ; ("java.nio.charset.StandardCharsets.UTF_16LE", o)
  ; ("java.nio.charset.StandardCharsets.UTF_8", o)
  ; ( "com.google.android.exoplayer2.source.dash.manifest.AdaptationSet.representations"
    , o (* Set in the constructor, where this is non-nullable *) )
  ; ("com.google.android.exoplayer2.source.dash.manifest.Representation.format", o)
  ; ("com.google.android.exoplayer2.upstream.DataSpec.fbDataSpecExtension", o)
  ; ( "com.google.android.exoplayer2.upstream.DataSpec.uri"
    , o (* Set in the constructor, where this is non-nullable *) )
  ; ("com.fasterxml.jackson.databind.node.JsonNodeFactory.instance", o) ]


let field_nullability_table =
  let table = Hashtbl.create 1 in
  List.iter field_nullability ~f:(fun (name, nullability) -> Hashtbl.add table name nullability) ;
  table
