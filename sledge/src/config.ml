(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Configuration options *)

(** Extension of Cmdliner supporting lighter-weight option definition *)
module Cmdliner : sig
  include module type of Cmdliner

  val mk : default:'a -> 'a Term.t -> 'a ref
  (** [mk ~default term] is a ref which, after [parse] is called, contains
      the value of the command line option specified by [term]. *)

  val parse : Term.info -> (unit -> unit Term.ret) -> unit
  (** [parse info validate] parses the command line according to the options
      declared by calls to [mk], using manual and version [info], and
      calling [validate] to check usage constraints not expressible in the
      [Term] language. *)
end = struct
  include Cmdliner

  (** existential package of a Term and a setter for a ref to receive the
      parsed value *)
  type arg = Arg : 'a Term.t * ('a -> unit) -> arg

  (** convert a list of arg packages to a term for the tuple of all the arg
      terms, and apply it to a function that sets all the receiver refs *)
  let tuple args =
    let pair (Arg (trm_x, set_x)) (Arg (trm_y, set_y)) =
      let trm_xy = Term.(const (fun a b -> (a, b)) $ trm_x $ trm_y) in
      let set_xy (a, b) = set_x a ; set_y b in
      Arg (trm_xy, set_xy)
    in
    let init = Arg (Term.const (), fun () -> ()) in
    let (Arg (trm, set)) = List.fold_right ~f:pair args ~init in
    Term.app (Term.const set) trm

  let args : arg list ref = ref []

  let mk ~default arg =
    let var = ref default in
    let set x = var := x in
    args := Arg (arg, set) :: !args ;
    var

  let parse info validate =
    match Term.eval (Term.(ret (const validate $ tuple !args)), info) with
    | `Ok () -> ()
    | `Error _ -> Caml.exit 1
    | `Help | `Version -> Caml.exit 0
end

open Cmdliner

let compile_only =
  let default = false in
  mk ~default Arg.(value & flag & info ["c"; "compile-only"])

let input =
  mk ~default:""
    Arg.(required & pos ~rev:true 0 (some string) None & info [])

let output =
  let default = None in
  mk ~default Arg.(value & opt (some string) default & info ["o"; "output"])

let trace =
  let default = Map.empty (module String) in
  let parse s =
    try
      let index_from s i =
        Option.merge ~f:min
          (String.index_from s i '+')
          (String.index_from s i '-')
      in
      let rec split s rev_parts i =
        match index_from s (i + 1) with
        | Some j when j = i -> split s rev_parts j
        | Some j ->
            split s (String.sub s ~pos:i ~len:(j - i) :: rev_parts) j
        | _ -> List.rev (String.subo s ~pos:i :: rev_parts)
      in
      let parts = split s [] 0 in
      List.fold parts ~init:default ~f:(fun m part ->
          let parse_part part =
            let sign, rest =
              match part.[0] with
              | '-' -> (false, String.subo part ~pos:1)
              | '+' -> (true, String.subo part ~pos:1)
              | _ -> (true, part)
            in
            assert (not (String.is_empty rest)) ;
            assert (Char.is_uppercase rest.[0]) ;
            match String.lsplit2 rest ~on:'.' with
            | Some (mod_name, fun_name) ->
                assert (Char.is_lowercase fun_name.[0]) ;
                (mod_name, Some fun_name, sign)
            | None -> (rest, None, sign)
          in
          match parse_part part with
          | mod_name, Some fun_name, enabled ->
              let {trace_mod; trace_funs} =
                match Map.find m mod_name with
                | Some c -> c
                | None -> {trace_mod= None; trace_funs= default}
              in
              Map.set m ~key:mod_name
                ~data:
                  { trace_mod
                  ; trace_funs=
                      Map.set trace_funs ~key:fun_name ~data:enabled }
          | mod_name, None, enabled ->
              Map.set m ~key:mod_name
                ~data:{trace_mod= Some enabled; trace_funs= default} )
      |> fun x -> `Ok x
    with _ -> `Error ("Invalid trace spec: " ^ s)
  in
  let print fs c =
    let pf fmt = Format.fprintf fs fmt in
    Map.iteri c ~f:(fun ~key:mod_name ~data:{trace_mod; trace_funs} ->
        ( match trace_mod with
        | Some true -> pf "+%s" mod_name
        | Some false -> pf "-%s" mod_name
        | None -> () ) ;
        Map.iteri trace_funs ~f:(fun ~key:fun_name ~data:fun_enabled ->
            if fun_enabled then pf "+%s.%s" mod_name fun_name
            else pf "-%s.%s" mod_name fun_name ) )
  in
  mk ~default Arg.(value & opt (parse, print) default & info ["t"; "trace"])

let trace_all =
  let default = false in
  mk ~default Arg.(value & flag & info ["v"; "trace-all"])

let info = Term.info "sledge" ~version:Version.version
let validate () = `Ok ()

let run main =
  parse info validate ;
  Trace.init ~config:{trace_all= !trace_all; trace_mods_funs= !trace} () ;
  main ~input:!input ~output:!output ~compile_only:!compile_only
