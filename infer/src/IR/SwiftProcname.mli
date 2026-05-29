(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type builtin =
  | DerivedEnumEquals
  | DynamicCall
  | InitTuple
  | Memcpy
  | NonDet
  | ObjcMsgSend
  | ObjcMsgSendSuper2
  | ObjcAllocFromSwift  (** Swift-driven ObjC allocation, takes [(sizeof typ, dynamic class)]. *)
  | OptionalInitNone  (** Swift [Optional<T>] construction of [.none]. *)
  | OptionalInitSome  (** Swift [Optional<T>] construction of [.some(payload)]. *)
  | OptionalInitSg
      (** Swift [Optional<T>] construction from a symbolic discriminator, [Sg]-class shape (e.g.
          [Int?]). Path-splits on the tag value: [tag = 1] is [.none], [tag = 0] is [.some]. *)
  | OptionalInitTuple
      (** Swift [Optional<T>] construction from a symbolic discriminator, 2-component
          [__infer_tuple_class<int,int>] shape (e.g. [String?]). Path-splits on the tag value:
          [tag = 0] is [.none], non-zero is [.some]. *)
  | OptionalUnsafelyUnwrapped  (** Swift [Optional<T>.unsafelyUnwrapped]. *)
  | OptionalForceUnwrapTrap
      (** Swift Optional force-unwrap (postfix [!]) trap: emitted on the proven-[.none] branch of
          the [if eq(disc, 0) then assert_fail] idiom. *)
  | SwiftAlloc  (** Swift class allocation, takes a single [sizeof typ] arg. *)
  | SwiftGetDynamicType
  | MetadataEquals  (** Used to compare metadata of two types. *)
[@@deriving compare, equal, yojson_of, sexp, hash, normalize, enumerate]

type t =
  | ClassMethod of {class_name: Typ.Name.t; method_name: Mangled.t}
  | Function of {function_name: Mangled.t}
  | Builtin of builtin
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

val mk_function : Mangled.t -> t

val mk_class_method : Typ.Name.t -> Mangled.t -> t

val mk_builtin : builtin -> t

val get_function_name : t -> Mangled.t

val pp : PpDetailLevel.t -> F.formatter -> t -> unit

val builtin_from_string : string -> builtin option

val show_builtin : builtin -> string

val to_string : t -> string
