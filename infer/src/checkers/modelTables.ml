(*
* Copyright (c) 2013 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
*)

(*
 * This file is a big bunch of tables; they read better with really long lines.
 * @nolint
 *)

open Utils

(* in strict mode cannot insert null in containers *)
let strict_containers = false

(* in strict mode, give an error if a nullable is passed to checkNotNull *)
let check_not_null_strict = false

let o = false and n = true (* o is not annotated and n is annotated with @Nullable *)
let o1 = (o, [o]) (* not annotated with one argument *)
let o2 = (o, [o; o]) (* not annotated with two arguments *)
let n1 = (o, [n]) (* one argument nullable *)
let n2 = (o, [n; n]) (* two arguments nullable *)
let n3 = (o, [n; n; n]) (* three arguments nullable *)
let on = (o, [o; n]) (* the second argument is nullable *)
let ca = if strict_containers then (o, [o]) else (o, [n]) (* container add *)
let cg = if strict_containers then (n, [o]) else (n, [n]) (* container get *)
let cp = if strict_containers then (n, [o; o]) else (n, [n; n]) (* container put *)
let ng = (n, []) (* Nullable getter *)

let check_not_null_parameter_list, check_not_null_list =
  let x = if check_not_null_strict then o else n in
  let list =
    [
      1, (o, [x; n]), "com.facebook.common.internal.Preconditions.checkNotNull(java.lang.Object,java.lang.Object):java.lang.Object";
      1, (o, [x; n; n]), "com.facebook.common.internal.Preconditions.checkNotNull(java.lang.Object,java.lang.String,java.lang.Object[]):java.lang.Object";
      1, (o, [x]), "com.facebook.common.internal.Preconditions.checkNotNull(java.lang.Object):java.lang.Object";
      1, (o, [x; n]), "com.google.common.base.Preconditions.checkNotNull(java.lang.Object,java.lang.Object):java.lang.Object";
      1, (o, [x; n; n]), "com.google.common.base.Preconditions.checkNotNull(java.lang.Object,java.lang.String,java.lang.Object[]):java.lang.Object";
      1, (o, [x]), "com.google.common.base.Preconditions.checkNotNull(java.lang.Object):java.lang.Object";
      1, (o, [x]), "org.junit.Assert.assertNotNull(java.lang.Object):void";
      2, (o, [n; x]), "org.junit.Assert.assertNotNull(java.lang.String,java.lang.Object):void";
      1, (o, [n]), "com.facebook.infer.annotation.Assertions.assertNotNull(java.lang.Object):java.lang.Object";
      1, (o, [n; o]), "com.facebook.infer.annotation.Assertions.assertNotNull(java.lang.Object,java.lang.String):java.lang.Object";
      1, (o, [n]), "com.facebook.infer.annotation.Assertions.assumeNotNull(java.lang.Object):java.lang.Object";
      1, (o, [n; o]), "com.facebook.infer.annotation.Assertions.assumeNotNull(java.lang.Object,java.lang.String):java.lang.Object";
    ] in
  list_map (fun (x, y, z) -> (x, z)) list, list_map (fun (x, y, z) -> (y, z)) list

let check_state_list =
  [
    (o, [n]), "Preconditions.checkState(boolean):void";
    (o, [n]), "com.facebook.common.internal.Preconditions.checkState(boolean):void";
    (o, [n; n]), "com.facebook.common.internal.Preconditions.checkState(boolean,java.lang.Object):void";
    (o, [n; n; n]), "com.facebook.common.internal.Preconditions.checkState(boolean,java.lang.String,java.lang.Object[]):void";
    (o, [n]), "com.google.common.base.Preconditions.checkState(boolean):void";
    (o, [n; n]), "com.google.common.base.Preconditions.checkState(boolean,java.lang.Object):void";
    (o, [n; n; n]), "com.google.common.base.Preconditions.checkState(boolean,java.lang.String,java.lang.Object[]):void";
    (o, [n]), "com.facebook.infer.annotation.Assertions.assertCondition(boolean):void";
    (o, [n; o]), "com.facebook.infer.annotation.Assertions.assertCondition(boolean,java.lang.String):void";
    (o, [n]), "com.facebook.infer.annotation.Assertions.assumeCondition(boolean):void";
    (o, [n; o]), "com.facebook.infer.annotation.Assertions.assumeCondition(boolean,java.lang.String):void";
  ]

let check_argument_list =
  [
    (o, [n]), "com.facebook.common.internal.Preconditions.checkArgument(boolean):void";
    (o, [n; n]), "com.facebook.common.internal.Preconditions.checkArgument(boolean,java.lang.Object):void";
    (o, [n; n; n]), "com.facebook.common.internal.Preconditions.checkArgument(boolean,java.lang.String,java.lang.Object[]):void";
    (o, [n]), "com.google.common.base.Preconditions.checkArgument(boolean):void";
    (o, [n; n]), "com.google.common.base.Preconditions.checkArgument(boolean,java.lang.Object):void";
    (o, [n; n; n]), "com.google.common.base.Preconditions.checkArgument(boolean,java.lang.String,java.lang.Object[]):void";
  ]

let optional_get_list : ((_ * bool list) * _) list =
  [
    (o, []), "Optional.get():java.lang.Object";
    (o, []), "com.google.common.base.Optional.get():java.lang.Object";
  ]

let optional_isPresent_list : ((_ * bool list) * _) list =
  [
    (o, []), "Optional.isPresent():boolean";
    (o, []), "com.google.common.base.Optional.isPresent():boolean";
  ]

(** Models for Map.containsKey *)
let containsKey_list =
  [
    n1, "com.google.common.collect.ImmutableMap.containsKey(java.lang.Object):boolean";
    n1, "java.util.Map.containsKey(java.lang.Object):boolean";
  ]

(** Models for @Strict annotations *)
let annotated_list_strict =
  [
    (n, [o]), "android.content.Context.getSystemService(java.lang.String):java.lang.Object";
  ]

(** Models for @Nullable annotations *)
let annotated_list_nullable =
  check_not_null_list @ check_state_list @ check_argument_list @
  annotated_list_strict @
  [
    n1, "android.os.Parcel.writeList(java.util.List):void";
    n2, "android.os.Parcel.writeParcelable(android.os.Parcelable,int):void";
    n1, "android.os.Parcel.writeString(java.lang.String):void";
    (o, [o; o; n; n; n]), "com.android.sdklib.build.ApkBuilder.<init>(java.io.File,java.io.File,java.io.File,java.lang.String,java.io.PrintStream)";
    (o, [n]), "com.android.manifmerger.ManifestMerger.xmlFileAndLine(org.w3c.dom.Node):com.android.manifmerger.IMergerLog$FileAndLine";
    on, "com.android.util.CommandLineParser$Mode.process(com.android.util.CommandLineParser$Arg,java.lang.String):java.lang.Object";
    on, "com.google.common.base.Objects$ToStringHelper.add(java.lang.String,java.lang.Object):com.google.common.base.Objects$ToStringHelper";
    n2, "com.google.common.base.Objects.equal(java.lang.Object,java.lang.Object):boolean";
    n1, "com.google.common.base.Optional.fromNullable(java.lang.Object):com.google.common.base.Optional";
    (n, []), "com.google.common.base.Optional.orNull():java.lang.Object";
    n1, "com.google.common.base.Strings.nullToEmpty(java.lang.String):java.lang.String";
    cg, "com.google.common.collect.ImmutableMap.get(java.lang.Object):java.lang.Object"; (* container get *)
    o1, "com.google.common.collect.ImmutableList$Builder.add(java.lang.Object):com.google.common.collect.ImmutableList$Builder";
    o1, "com.google.common.collect.ImmutableList$Builder.addAll(java.lang.Iterable):com.google.common.collect.ImmutableList$Builder";
    o1, "com.google.common.collect.ImmutableSortedSet$Builder.add(java.lang.Object):com.google.common.collect.ImmutableSortedSet$Builder";
    on, "com.google.common.collect.Iterables.getFirst(java.lang.Iterable,java.lang.Object):java.lang.Object";
    o1, "com.google.common.util.concurrent.SettableFuture.setException(java.lang.Throwable):boolean";
    o1, "java.io.File.<init>(java.lang.String)";
    n1, "java.io.PrintStream.print(java.lang.String):void";
    o1, "java.lang.Class.isAssignableFrom(java.lang.Class):boolean";
    n1, "java.lang.Integer.equals(java.lang.Object):boolean";
    n2, "java.lang.RuntimeException.<init>(java.lang.String,java.lang.Throwable)";
    n1, "java.lang.String.equals(java.lang.Object):boolean";
    n1, "java.lang.StringBuilder.append(java.lang.String):java.lang.StringBuilder";
    on, "java.net.URLClassLoader.newInstance(java.net.URL[],java.lang.ClassLoader):java.net.URLClassLoader";
    n1, "java.util.AbstractList.equals(java.lang.Object):boolean";
    ca, "java.util.ArrayList.add(java.lang.Object):boolean"; (* container add *)
    ca, "java.util.List.add(java.lang.Object):boolean"; (* container add *)
    cg, "java.util.Map.get(java.lang.Object):java.lang.Object"; (* container get *)
    cp, "java.util.Map.put(java.lang.Object,java.lang.Object):java.lang.Object"; (* container put *)
    n3, "javax.tools.JavaCompiler.getStandardFileManager(javax.tools.DiagnosticListener,java.util.Locale,java.nio.charset.Charset):javax.tools.StandardJavaFileManager";
    (n, [o; n; n]), "org.w3c.dom.Document.setUserData(java.lang.String,java.lang.Object,org.w3c.dom.UserDataHandler):java.lang.Object";
    (n, [o; n; n]), "org.w3c.dom.Node.setUserData(java.lang.String,java.lang.Object,org.w3c.dom.UserDataHandler):java.lang.Object";

    (* References *)
    ng, "java.lang.ref.Reference.get():java.lang.Object";
    ng, "java.lang.ref.PhantomReference.get():java.lang.Object";
    ng, "java.lang.ref.SoftReference.get():java.lang.Object";
    ng, "java.lang.ref.WeakReference.get():java.lang.Object";
    ng, "java.util.concurrent.atomic.AtomicReference.get():java.lang.Object";
  ]

(** Models for @Present annotations *)
let annotated_list_present =
  [
    (n, [o]), "Optional.of(java.lang.Object):Optional";
    (n, [o]), "com.google.common.base.Optional.of(java.lang.Object):com.google.common.base.Optional";
  ]

type model_table_t = (string, bool * bool list) Hashtbl.t

let mk_table list =
  let map = Hashtbl.create 1 in
  list_iter (function (v, pn_id) -> Hashtbl.replace map pn_id v) list;
  map

let annotated_table_nullable = mk_table annotated_list_nullable
let annotated_table_present = mk_table annotated_list_present
let annotated_table_strict = mk_table annotated_list_strict
let check_not_null_table, check_not_null_parameter_table =
  mk_table check_not_null_list, mk_table check_not_null_parameter_list
let check_state_table = mk_table check_state_list
let check_argument_table = mk_table check_argument_list
let optional_get_table = mk_table optional_get_list
let optional_isPresent_table = mk_table optional_isPresent_list
let containsKey_table = mk_table containsKey_list
