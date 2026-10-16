(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type typ_model =
  | CArray of
      { element_typ: Typ.t
      ; deref_kind: Symb.SymbolPath.deref_kind
      ; length: IntLit.t
      ; stride: int option  (** byte size of an element, if known *) }
  | CppStdVector
  | JavaCollection
  | JavaInteger

val dispatch : (Tenv.t, typ_model, unit) ProcnameDispatcher.TypName.dispatcher

val dispatch_std_array : (Tenv.t, typ_model, unit) ProcnameDispatcher.TypName.dispatcher
(** [dispatch] restricted to [std::array], cheaper on other classes *)
