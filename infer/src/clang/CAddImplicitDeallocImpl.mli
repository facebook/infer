(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val process : Cfg.t -> Tenv.t -> unit
(** This models ARC implementation of dealloc, see
    https://clang.llvm.org/docs/AutomaticReferenceCounting.html#dealloc. Dealloc methods can be
    added to ObjC classes to free C memory for example, but the deallocation of the ObjC instance
    variables of the object is done automatically. So here we add this explicitely to Infer: we add
    calls to dealloc of the ObjC instance variables. Here we assume that every ObjC class has
    already a dealloc method, because if it doesn't exist we add an empty method in
    CFrontend_decl.create_and_process_dealloc_objc_impl TODO(T68411500): add calls to dealloc of the
    superclass. *)
