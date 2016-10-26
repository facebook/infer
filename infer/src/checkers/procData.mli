(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

type 'a t = { pdesc : Cfg.Procdesc.t; tenv : Tenv.t; extras : 'a; }

type no_extras

val empty_extras : no_extras

val make : Cfg.Procdesc.t -> Tenv.t -> 'a -> 'a t

val make_default : Cfg.Procdesc.t -> Tenv.t -> no_extras t
