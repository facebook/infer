(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** {2 Global state for the backend analyses}

    While global state should be kept under control at possible (so try to introduce as little of it
    as possible!), it is sometimes too convenient to ignore. This module lets us do it safely. In
    particular, global state (really, domain-local state) is appropriately and safely stashed away
    and restored when the analysis of a procedure is suspended to go analyze another procedure with
    ondemand (see the {!Backend.Ondemand} module). *)

type t

val save : unit -> t

val restore : t -> unit

val initialize : Procdesc.t -> Tenv.t -> unit

val register : init:(unit -> unit) -> save:(unit -> 'a) -> restore:('a -> unit) -> unit
(** Register pieces of global state from other analysis modules:

    - [save] and [restore] are called when suspending/resuming the analysis of the procedure when we
      need to go analyze a dependency
    - [init] is called when the analysis of a procedure starts *)

val register_ref : init:(unit -> 'a) -> 'a ref -> unit
[@@deprecated "Do not use refs, use [*_dls] functions instead"]
(** special case of a value stored in a reference; [init] sets the ref to [init ()] *)

val make_dls : init:(unit -> 'a) -> 'a DLS.key
(** create a value stored in domain-local storage; [init] is used to set the value on initialization
    and when spawning a new domain *)

val register_dls_with_proc_desc_and_tenv : init:(Procdesc.t -> Tenv.t -> 'a) -> 'a DLS.key -> unit
(** special case of a value stored in domain local storage *)
