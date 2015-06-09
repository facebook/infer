(*
* Copyright (c) 2013 - Facebook. All rights reserved.
*)

open Utils
module L = Logging

(** Module for standard library models. *)

(* in strict mode cannot insert null in containers *)
let strict_containers = false

(* in strict mode, give an error if a nullable is passed to checkNotNull *)
let check_not_null_strict = false

(* use library of inferred return annotations *)
let use_library = false

(* use model annotations for library functions *)
let use_models = true

(* libary functions: infer nullable annotation of return type *)
let infer_library_return = Config.from_env_variable "ERADICATE_LIBRARY"


(** Module for inference of parameter and return annotations. *)
module Inference = struct
  let enabled = false

  let get_dir () = Filename.concat !Config.results_dir "eradicate"

  let field_get_dir_fname fn =
    let fname = Ident.fieldname_to_string fn in
    (get_dir (), fname)

  let field_is_marked fn =
    let dir, fname = field_get_dir_fname fn in
    DB.read_file_with_lock dir fname <> None

  let proc_get_ret_dir_fname pname =
    let fname = Procname.to_filename pname ^ "_ret" in
    (get_dir (), fname)

  let proc_get_param_dir_fname pname =
    let fname = Procname.to_filename pname ^ "_params" in
    (get_dir (), fname)

  let update_count_str s_old =
    let n =
      if s_old = "" then 0
      else try int_of_string s_old with
        | Failure _ ->
            L.stderr "int_of_string %s@." s_old;
            assert false in
    string_of_int (n + 1)

  let update_boolvec_str _s size index bval =
    let s = if _s = "" then String.make size '0' else _s in
    String.set s index (if bval then '1' else '0');
    s

  let mark_file update_str dir fname =
    DB.update_file_with_lock dir fname update_str;
    match DB.read_file_with_lock dir fname with
    | Some buf -> L.stderr "Read %s: %s@." fname buf
    | None -> L.stderr "Read %s: None@." fname

  let mark_file_count = mark_file update_count_str

  (** Mark the field @Nullable indirectly by writing to a global file. *)
  let field_add_nullable_annotation fn =
    let dir, fname = field_get_dir_fname fn in
    mark_file_count dir fname

  (** Mark the return type @Nullable indirectly by writing to a global file. *)
  let proc_add_return_nullable pn =
    let dir, fname = proc_get_ret_dir_fname pn in
    mark_file_count dir fname

  (** Return true if the return type is marked @Nullable in the global file *)
  let proc_return_is_marked pname =
    let dir, fname = proc_get_ret_dir_fname pname in
    DB.read_file_with_lock dir fname <> None

  (** Mark the n-th parameter @Nullable indirectly by writing to a global file. *)
  let proc_add_parameter_nullable pn n tot =
    let dir, fname = proc_get_param_dir_fname pn in
    let update_str s = update_boolvec_str s tot n true in
    mark_file update_str dir fname

  (** Return None if the parameters are not marked, or a vector of marked parameters *)
  let proc_parameters_marked pn =
    let dir, fname = proc_get_param_dir_fname pn in
    match DB.read_file_with_lock dir fname with
    | None -> None
    | Some buf ->
        let boolvec = ref [] in
        String.iter (fun c -> boolvec := (c = '1') :: !boolvec) buf;
        Some (list_rev !boolvec)
end (* Inference *)


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

let mk_table list =
  let map = Hashtbl.create 1 in
  list_iter (function (v, pn_id) -> Hashtbl.replace map pn_id v) list;
  map

let table_has_procedure table proc_name =
  let proc_id = Procname.to_unique_id proc_name in
  try ignore (Hashtbl.find table proc_id); true
  with Not_found -> false

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

type table_t = (string, bool) Hashtbl.t

(* precomputed marshalled table of inferred return annotations. *)
let ret_library_table : table_t Lazy.t =
  lazy (Hashtbl.create 1)
(*
lazy (Marshal.from_string Eradicate_library.marshalled_library_table 0)
*)

(** Return the annotated signature of the procedure, taking into account models. *)
let get_annotated_signature callee_pdesc callee_pname =
  let annotated_signature =
    Annotations.get_annotated_signature
      Specs.proc_get_method_annotation callee_pdesc callee_pname in
  let proc_id = Procname.to_unique_id callee_pname in
  let infer_parameters ann_sig =
    let mark_par =
      if Inference.enabled then Inference.proc_parameters_marked callee_pname
      else None in
    match mark_par with
    | None -> ann_sig
    | Some bs ->
        let mark = (false, bs) in
        Annotations.annotated_signature_mark callee_pname Annotations.Nullable ann_sig mark in
  let infer_return ann_sig =
    let mark_r =
      let from_library =
        if use_library then
          try
            Hashtbl.find (Lazy.force ret_library_table) proc_id
          with Not_found -> false
        else false in
      let from_inference = Inference.enabled && Inference.proc_return_is_marked callee_pname in
      from_library || from_inference in
    if mark_r
    then Annotations.annotated_signature_mark_return callee_pname Annotations.Nullable ann_sig
    else ann_sig in
  let lookup_models_nullable ann_sig =
    if use_models then
      try
        let mark = Hashtbl.find annotated_table_nullable proc_id in
        Annotations.annotated_signature_mark callee_pname Annotations.Nullable ann_sig mark
      with Not_found ->
          ann_sig
    else ann_sig in
  let lookup_models_present ann_sig =
    if use_models then
      try
        let mark = Hashtbl.find annotated_table_present proc_id in
        Annotations.annotated_signature_mark callee_pname Annotations.Present ann_sig mark
      with Not_found ->
          ann_sig
    else ann_sig in
  let lookup_models_strict ann_sig =
    if use_models
    && Hashtbl.mem annotated_table_strict proc_id
    then
      Annotations.annotated_signature_mark_return_strict callee_pname ann_sig
    else
      ann_sig in

  annotated_signature
  |> lookup_models_nullable
  |> lookup_models_present
  |> lookup_models_strict
  |> infer_return
  |> infer_parameters

(** Return true when the procedure has been modelled for nullable. *)
let is_modelled_nullable proc_name =
  if use_models then
    let proc_id = Procname.to_unique_id proc_name in
    try ignore (Hashtbl.find annotated_table_nullable proc_id ); true
    with Not_found -> false
  else false

(** Return true when the procedure belongs to the library of inferred return annotations. *)
let is_ret_library proc_name =
  if use_library && not infer_library_return then
    let proc_id = Procname.to_unique_id proc_name in
    try ignore (Hashtbl.find (Lazy.force ret_library_table) proc_id); true
    with Not_found -> false
  else false

(** Check if the procedure is one of the known Preconditions.checkNotNull. *)
let is_check_not_null proc_name =
  table_has_procedure check_not_null_table proc_name

(** Parameter number for a procedure known to be a checkNotNull *)
let get_check_not_null_parameter proc_name =
  let proc_id = Procname.to_unique_id proc_name in
  try Hashtbl.find check_not_null_parameter_table proc_id
  with Not_found -> 0

(** Check if the procedure is one of the known Preconditions.checkState. *)
let is_check_state proc_name =
  table_has_procedure check_state_table proc_name

(** Check if the procedure is one of the known Preconditions.checkArgument. *)
let is_check_argument proc_name =
  table_has_procedure check_argument_table proc_name

(** Check if the procedure is Optional.get(). *)
let is_optional_get proc_name =
  table_has_procedure optional_get_table proc_name

(** Check if the procedure is Optional.isPresent(). *)
let is_optional_isPresent proc_name =
  table_has_procedure optional_isPresent_table proc_name

(** Check if the procedure is Map.containsKey(). *)
let is_containsKey proc_name =
  table_has_procedure containsKey_table proc_name
