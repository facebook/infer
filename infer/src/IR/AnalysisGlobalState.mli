(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** {2 Global state for the backend analyses}

    While global state should be kept under control at possible (so try to introduce as little of it
    as possible!), it is sometimes too convenient to ignore. This module lets us do it safely, at
    least until infer becomes multicore. In particular, global state is appropriately and safely
    stashed away and restored when the analysis of a procedure is suspended to go analyze another
    procedure with ondemand (see the {!Backend.Ondemand} module). *)

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
(** special case of a value stored in a reference; [init] sets the ref to [init ()] *)

val register_ref_with_proc_desc_and_tenv : init:(Procdesc.t -> Tenv.t -> 'a) -> 'a ref -> unit
(** same as [register_ref] but [init] takes a proc desc and a tenv *)
