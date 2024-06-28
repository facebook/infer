(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module type Kind = sig
  include TaintTraceElem.Kind

  val get :
    caller_pname:Procname.t -> Procname.t -> HilExp.t list -> Tenv.t -> (t * int option) list

  val get_tainted_formals : Procdesc.t -> Tenv.t -> (Mangled.t * Typ.t * t option) list
end

module type S = sig
  include TaintTraceElem.S

  type spec = {source: t; index: int option}

  val get : caller_pname:Procname.t -> CallSite.t -> HilExp.t list -> Tenv.t -> spec list

  val get_tainted_formals : Procdesc.t -> Tenv.t -> (Mangled.t * Typ.t * t option) list
end

module Dummy = struct
  type t = unit [@@deriving compare, equal]

  type spec = {source: t; index: int option}

  let call_site _ = CallSite.dummy

  let kind t = t

  let make ?indexes:_ kind _ = kind

  let pp _ () = ()

  let get ~caller_pname:_ _ _ _ = []

  let get_tainted_formals pdesc _ =
    List.map ~f:(fun (name, typ, _) -> (name, typ, None)) (Procdesc.get_formals pdesc)


  module Kind = struct
    type nonrec t = t [@@deriving compare, equal]

    let matches ~caller ~callee = Int.equal 0 (compare caller callee)

    let pp = pp
  end

  module Set = PrettyPrintable.MakePPSet (struct
    type nonrec t = t [@@deriving compare]

    let pp = pp
  end)

  let with_callsite t _ = t
end
