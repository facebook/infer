(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Global variables *)

type t = {var: Var.t; init: Exp.t option; siz: int; typ: Typ.t; loc: Loc.t}
[@@deriving compare, hash, sexp]

let equal = [%compare.equal: t]

let demangle =
  let open Ctypes in
  let cxa_demangle =
    (* char *__cxa_demangle(const char *, char *, size_t *, int * ) *)
    Foreign.foreign "__cxa_demangle"
      ( string @-> ptr char @-> ptr size_t @-> ptr int
      @-> returning string_opt )
  in
  let null_ptr_char = from_voidp char null in
  let null_ptr_size_t = from_voidp size_t null in
  let status = allocate int 0 in
  fun mangled ->
    let demangled =
      cxa_demangle mangled null_ptr_char null_ptr_size_t status
    in
    if !@status = 0 then demangled else None

let pp fs {var} =
  let name = Var.name var in
  let pf pp =
    Format.pp_open_box fs 2 ;
    Format.kfprintf (fun fs -> Format.pp_close_box fs ()) fs pp
  in
  pf "@%s%t" name (fun fs ->
      match demangle name with
      | Some demangled when not (String.equal name demangled) ->
          Format.fprintf fs "“%s”" demangled
      | _ -> () )

let pp_defn fs {var; init; typ} =
  Format.fprintf fs "@[<2>%a %a%a@]" Typ.pp typ Var.pp var
    (Option.pp " =@ @[%a@]" Exp.pp)
    init

let invariant g =
  Invariant.invariant [%here] g [%sexp_of: t]
  @@ fun () ->
  let {typ} = g in
  assert (Typ.is_sized typ)

let mk ?init var siz typ loc = {var; init; siz; typ; loc} |> check invariant
