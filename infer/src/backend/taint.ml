(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module L = Logging

open PatternMatch

(* list of sources that return a tainted value *)
let sources = [
  (* for testing only *)
  {
    classname = "com.facebook.infer.models.InferTaint";
    method_name = "inferSecretSource";
    ret_type = "java.lang.Object";
    params = [];
    is_static = true;
    language = Config.Java;
  };
  {
    classname = "com.facebook.infer.models.InferTaint";
    method_name = "inferSecretSourceUndefined";
    ret_type = "java.lang.Object";
    params = [];
    is_static = true;
    language = Config.Java
  };
  (* actual specs *)
  {
    classname = "android.content.SharedPreferences";
    method_name = "getString";
    ret_type = "java.lang.String";
    params = ["java.lang.String"; "java.lang.String"];
    is_static = false;
    language = Config.Java
  };
] @ FbTaint.sources

(* list of (sensitive sinks, zero-indexed numbers of parameters that should not be tainted). note:
   index 0 means "the first non-this/self argument"; we currently don't have a way to say "this/self
   should not be tainted" with this form of specification *)
let sinks = [
  (* for testing only *)
  ({
    classname = "com.facebook.infer.models.InferTaint";
    method_name = "inferSensitiveSink";
    ret_type = "void";
    params = ["java.lang.Object"];
    is_static = true;
    language = Config.Java
  }, [0]);
  ({
    classname = "com.facebook.infer.models.InferTaint";
    method_name = "inferSensitiveSinkUndefined";
    ret_type = "void";
    params = ["java.lang.Object"];
    is_static = true;
    language = Config.Java
  }, [0]);
  (* actual specs *)
  ({
    classname = "android.util.Log";
    method_name = "d";
    ret_type = "int";
    params = ["java.lang.String"; "java.lang.String"];
    is_static = true;
    language = Config.Java
  }, [0;1]);
  ({
    classname = "android.content.ContentResolver";
    method_name = "openInputStream";
    ret_type = "java.io.InputStream";
    params = ["android.net.Uri"];
    is_static = false;
    language = Config.Java;
  }, [1]);
  ({
    classname = "android.content.ContentResolver";
    method_name = "openOutputStream";
    ret_type = "java.io.OutputStream";
    params = ["android.net.Uri"];
    is_static = false;
    language = Config.Java;
  }, [0]);
  ({
    classname = "android.content.ContentResolver";
    method_name = "openOutputStream";
    ret_type = "java.io.OutputStream";
    params = ["android.net.Uri"; "java.lang.String"];
    is_static = false;
    language = Config.Java;
  }, [0]);
  ({
    classname = "android.content.ContentResolver";
    method_name = "openAssetFileDescriptor";
    ret_type = "android.content.res.AssetFileDescriptor";
    params = ["android.net.Uri"; "java.lang.String"];
    is_static = false;
    language = Config.Java;
  }, [0]);
  ({
    classname = "android.content.ContentResolver";
    method_name = "openAssetFileDescriptor";
    ret_type = "android.content.res.AssetFileDescriptor";
    params = ["android.net.Uri"; "java.lang.String"; "android.os.CancellationSignal"];
    is_static = false;
    language = Config.Java;
  }, [0]);
  ({
    classname = "android.content.ContentResolver";
    method_name = "openFileDescriptor";
    ret_type = "android.os.ParcelFileDescriptor";
    params = ["android.net.Uri"; "java.lang.String"; "android.os.CancellationSignal"];
    is_static = false;
    language = Config.Java;
  }, [0]);
  ({
    classname = "android.content.ContentResolver";
    method_name = "openFileDescriptor";
    ret_type = "android.os.ParcelFileDescriptor";
    params = ["android.net.Uri"; "java.lang.String"];
    is_static = false;
    language = Config.Java;
  }, [0]);
  ({
    classname = "android.content.ContentResolver";
    method_name = "openTypedAssetFileDescriptor";
    ret_type = "android.content.res.AssetFileDescriptor";
    params = ["android.net.Uri"; "java.lang.String"; "android.os.Bundle";
              "android.os.CancellationSignal"];
    is_static = false;
    language = Config.Java;
  }, [0]);
  ({
    classname = "android.content.ContentResolver";
    method_name = "openTypedAssetFileDescriptor";
    ret_type = "android.content.res.AssetFileDescriptor";
    params = ["android.net.Uri"; "java.lang.String"; "android.os.Bundle"];
    is_static = false;
    language = Config.Java;
  }, [0]);

  (*  ==== iOS for testing only ==== *)
  ({
    classname = "ExampleViewController";
    method_name = "loadURL:trackingCodes:";
    ret_type = "void";
    params = [];
    is_static = false;
    language = Config.C_CPP
  }, [1]); (* it's instance method *)
] @ FbTaint.sinks

let functions_with_tainted_params = [
  (*  ==== iOS for testing only ==== *)
  ({
    classname = "ExampleDelegate";
    method_name = "application:openURL:sourceApplication:annotation:";
    ret_type = "BOOL";
    params = [];
    is_static = false; (* it's instance method *)
    language = Config.C_CPP
  }, [2]);

  (* actual specs *)
  ({ (* This method is a source in iOS as it get as parameter
        a non trusted URL (openURL). The method the passes
        it around and this URL may arrive unsanitized to
        loadURL:trackingCodes: of FBWebViewController
        which uses the URL. *)
    classname = "AppDelegate";
    method_name = "application:openURL:sourceApplication:annotation:";
    ret_type = "BOOL";
    params = [];
    is_static = false; (* it's instance method *)
    language = Config.C_CPP
  }, [2]);
] @ FbTaint.functions_with_tainted_params

(* turn string specificiation of Java method into a procname *)
let java_method_to_procname java_method =
  Procname.Java
    (Procname.java
       (Procname.split_classname java_method.classname)
       (Some (Procname.split_classname java_method.ret_type))
       java_method.method_name
       (IList.map Procname.split_classname java_method.params)
       (if java_method.is_static then Procname.Static else Procname.Non_Static))

(* turn string specificiation of an objc method into a procname *)
let objc_method_to_procname objc_method =
  let method_kind = Procname.objc_method_kind_of_bool (not objc_method.is_static) in
  let mangled = Procname.mangled_of_objc_method_kind method_kind in
  Procname.ObjC_Cpp
    (Procname.objc_cpp objc_method.classname objc_method.method_name mangled)

let method_str_to_pname method_str =
  match method_str.language with
  | Config.C_CPP ->
      objc_method_to_procname method_str
  | Config.Java ->
      java_method_to_procname method_str

let sources =
  IList.map method_str_to_pname sources

let mk_pname_param_num methods =
  IList.map
    (fun (mname, param_num) -> method_str_to_pname mname, param_num)
    methods

let sinks =
  mk_pname_param_num sinks

let func_with_tainted_params =
  mk_pname_param_num functions_with_tainted_params

(** returns true if [callee_pname] returns a tainted value *)
let returns_tainted callee_pname =
  IList.exists (fun pname -> Procname.equal pname callee_pname) sources

let find_callee methods callee_pname =
  try
    snd (IList.find (fun (pname, _) -> Procname.equal pname callee_pname) methods)
  with Not_found -> []

(** returns list of zero-indexed argument numbers of [callee_pname] that may be tainted *)
let accepts_sensitive_params callee_pname =
  find_callee sinks callee_pname

(** returns list of zero-indexed parameter numbers of [callee_pname] that should be
    considered tainted during symbolic execution *)
let tainted_params callee_pname =
  find_callee func_with_tainted_params callee_pname
