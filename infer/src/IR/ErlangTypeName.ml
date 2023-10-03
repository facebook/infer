(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(* TODO: Add other types as they are needed by translation (otherwise it's dead code). *)
type t =
  | Any
  | Atom
  | Integer
  | Cons
  | Nil
  | Tuple of int
  | Map
  | GenServerPid of {module_name: string option}
  | ModuleInfo
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

let pp f = function
  | Any ->
      Format.fprintf f "ErlangAny"
  | Atom ->
      Format.fprintf f "ErlangAtom"
  | Integer ->
      Format.fprintf f "ErlangInteger"
  | Nil ->
      Format.fprintf f "ErlangNil"
  | Cons ->
      Format.fprintf f "ErlangCons"
  | Tuple arity ->
      Format.fprintf f "ErlangTuple%d" arity
  | Map ->
      Format.fprintf f "ErlangMap"
  | GenServerPid {module_name} ->
      Format.fprintf f "ErlangGenServerPid_%s" (Option.value module_name ~default:"")
  | ModuleInfo ->
      Format.fprintf f "ErlangModuleInfo"


let to_string name = Format.asprintf "%a" pp name

let from_string s =
  let constr_opt format constr =
    try Scanf.sscanf s format (fun d -> Some (constr d)) with Scanf.Scan_failure _ -> None
  in
  match s with
  | "ErlangAny" | "Any" ->
      Some Any
  | "ErlangAtom" | "Atom" ->
      Some Atom
  | "ErlangCons" | "Cons" ->
      Some Cons
  | "ErlangInteger" | "Integer" ->
      Some Integer
  | "ErlangMap" | "Map" ->
      Some Map
  | "ErlangNil" | "Nil" ->
      Some Nil
  | "ErlangModuleInfo" | "ModuleInfo" ->
      Some ModuleInfo
  | _ ->
      let mk_tuple i = Tuple i in
      let mk_genserverpid m =
        match m with
        | "" ->
            GenServerPid {module_name= None}
        | _ ->
            GenServerPid {module_name= Some m}
      in
      let find_some li = List.find_map ~f:Fn.id li in
      find_some
        [ constr_opt "ErlangTuple%d%!" mk_tuple
        ; constr_opt "Tuple%d%!" mk_tuple
        ; constr_opt "ErlangGenServerPid_%s%!" mk_genserverpid
        ; constr_opt "GenServerPid_%s%!" mk_genserverpid ]


let atom_name = "name"

let atom_hash = "hash"

let atom_true = "true"

let atom_false = "false"

let module_info_field_name = "module_info"

let module_info_attributes_class_name = "attributes"

let calculate_hash atom =
  (* DISCLAIMER: there is currently no guarantee that this remains stable in the future and no
     production-critical code should depend on this. The goal of this unofficial specification is to
     facilitate debugging and fast development iterations.

     We hash atom names into integers to ease the job of the analysers.

     To do that, we interpret the first 7 bytes (= first 56 bits) of the MD5 hash of the name as
     a big-endian integer.
     - Using less than 62 bits ensures that the hash fits on a positive 63-bits OCaml integer.
     - Using a round number of bytes makes implementations easier to write.
     - Interpreting the bytes as big endian is both easier and less confusing, cf. example below.

     Example. The MD5 of "foo" is the byte sequence acbd18db4cc2f85cedef654fccc4a4d8 (in the usual
     hexdump notation). We therefore use 0xacbd18db4cc2f8 as the integer hash. Note: that's
     big-endian because, in little-endian, the number would be Oxf8c2... instead. Little-endian would
     mostly make implementation more annoying to write (and less intuitive to the debugging reader
     who has access to a md5 hexdump).


     OTHER LANGUAGE IMPLEMENTATIONS.

     For easier reference and faster moving, we provide various ways of implementing the same hashing
     function in some common languages.

     * POSIX Shell:
     ```
     $ echo $((0x$(echo -n foo | md5sum | head -c 14)))
     48621610450338552
     ```

     * Python:
     ```
     >>> from hashlib import md5
     >>> 0xacbd18db4cc2f8 # FYI
     48621610450338552

     >>> # Preferred: directly interpret the hash bytes as an int
     >>> int.from_bytes(md5("foo".encode()).digest()[:7], "big")
     48621610450338552

     >>> # Easier-to-debug steps: go through the md5 hexdump
     >>> int(md5("foo".encode()).hexdigest()[:14], base=16)
     48621610450338552
     ```

     * Python invocation from shell:
     ```
     $ python -c 'from hashlib import *; print(int(md5("foo".encode()).hexdigest()[:14], 16))'
     ```
  *)
  Md5.digest_string atom (* Compute md5 *)
  |> Md5.to_binary |> Bytes.of_string (* Get the hash as bytes *)
  |> Fn.flip Caml.Bytes.get_int64_be 0 (* Get the first 8 bytes as a big-endian int64 *)
  |> Fn.flip Int64.shift_right_logical 8 (* Drop the last byte / keep the first 7 bytes only *)
  |> Int64.to_int_exn (* Put in an integer, raising if it unexpectedly does not fit *)


let integer_value = "value"

let cons_head = "head"

let cons_tail = "tail"

let tuple_elem i = Printf.sprintf "elem%d" i

(* Tuple element indexing is one based *)
let tuple_field_names size = List.init size ~f:(fun i -> tuple_elem (i + 1))

let erlang_namespace = "erlang"

let unsupported = "__unsupported"

let infer_erlang_namespace = "__infer__erlang"
