(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val is_recursive : Tenv.t -> caller:Procname.t -> Typ.Name.t -> AbstractAddress.t -> bool
(** is the lock, of the given type, a pthread mutex initialised as recursive, by
    [PTHREAD_RECURSIVE_MUTEX_INITIALIZER[_NP]] or by [pthread_mutex_init] with an attribute made
    recursive by [pthread_mutexattr_settype] in the same procedure? Initialisations are looked for
    in the translation unit of [caller], in the initializer of a global lock and in the constructors
    of the class of a field lock, and are matched by global or by field only. *)
