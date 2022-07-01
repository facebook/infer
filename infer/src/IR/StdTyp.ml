(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = Typ.t

let boolean = Typ.mk (Tint IBool)

let char = Typ.mk (Tint IChar)

let double = Typ.mk (Tfloat FDouble)

let float = Typ.mk (Tfloat FFloat)

let int = Typ.mk (Tint IInt)

let long = Typ.mk (Tint ILong)

let uint = Typ.mk (Tint IUInt)

let void = Typ.mk Tvoid

let void_star = Typ.mk_ptr void

module Name = struct
  type t = Typ.Name.t

  module Java = struct
    open Typ.Name.Java

    let java_io_serializable = from_string "java.io.Serializable"

    let java_lang_class = from_string "java.lang.Class"

    let java_lang_cloneable = from_string "java.lang.Cloneable"

    let java_lang_object = from_string "java.lang.Object"

    let java_lang_string = from_string "java.lang.String"

    let kotlin_coroutines_jvm_internal_restrictedsuspendlambda =
      from_string "kotlin.coroutines.jvm.internal.RestrictedSuspendLambda"
  end

  module CSharp = struct
    open Typ.Name.CSharp

    let system_string = from_string "System.String"

    let system_object = from_string "System.Object"
  end

  module Objc = struct
    open Typ.Name.Objc

    let ns_enumerator = from_string "NSEnumerator"

    let ns_object = from_string "NSObject"
  end
end

module Java = struct
  let byte = Typ.mk (Tint ISChar)

  let char = Typ.mk (Tint IUShort)

  let short = Typ.mk (Tint IShort)

  let pointer_to_java_lang_class = Typ.mk_ptr (Typ.mk_struct Name.Java.java_lang_class)

  let pointer_to_java_lang_object = Typ.mk_ptr (Typ.mk_struct Name.Java.java_lang_object)

  let pointer_to_java_lang_string = Typ.mk_ptr (Typ.mk_struct Name.Java.java_lang_string)
end

module Objc = struct
  let pointer_to_nszone =
    Typ.(mk_ptr (mk_struct (CStruct (QualifiedCppName.of_qual_string "NSZone"))))
end
