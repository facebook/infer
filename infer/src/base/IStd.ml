(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module Arg = Core.Std.Arg
module Array = Core.Std.Array
module Bool = Core.Std.Bool
module Bytes = Core.Std.Bytes
module Caml = Core.Std.Caml
module Char = Core.Std.Char
module Exn = Core.Std.Exn
module Filename = Core.Std.Filename
module Fn = Core.Std.Fn
module Gc = Core.Std.Gc
module In_channel = Core.Std.In_channel
module Int = Core.Std.Int
module Int32 = Core.Std.Int32
module Int63 = Core.Std.Int63
module Int64 = Core.Std.Int64
module Lazy = Core.Std.Lazy
module Nativeint = Core.Std.Nativeint
module Option = Core.Std.Option
module Pid = Core.Std.Pid
module Printexc = Core.Std.Printexc
module Printf = Core.Std.Printf
module Queue = Core.Std.Queue
module Random = Core.Std.Random
module Signal = Core.Std.Signal
module Stack = Core.Std.Stack
module String = Core.Std.String
module Sys = struct
  include Core.Std.Sys

  (* Core_sys does not catch Unix_error ENAMETOOLONG, see
     https://github.com/janestreet/core/issues/76 *)
  let file_exists ?follow_symlinks path =
    try file_exists ?follow_symlinks path
    with Unix.Unix_error _ -> `Unknown

  let is_directory ?follow_symlinks path =
    try is_directory ?follow_symlinks path
    with Unix.Unix_error _ -> `Unknown

  let is_file ?follow_symlinks path =
    try is_file ?follow_symlinks path
    with Unix.Unix_error _ -> `Unknown
end
module Unix = Core.Std.Unix

module IntSet = Caml.Set.Make(Int)


(** List police: don't use the list module to avoid non-tail recursive
    functions and builtin equality. Use IList instead. *)
module List = struct end

(** Compare police: generic compare disabled. *)
let compare = ()


(** {2 Generic Utility Functions} *)

type ('a, 'b) result =
  | Ok of 'a
  | Error of 'b

let fst3 (x,_,_) = x
let snd3 (_,x,_) = x
let trd3 (_,_,x) = x

let failwithf fmt =
  Format.kfprintf (fun _ -> failwith (Format.flush_str_formatter ()))
    Format.str_formatter fmt

let invalid_argf fmt =
  Format.kfprintf (fun _ -> invalid_arg (Format.flush_str_formatter ()))
    Format.str_formatter fmt

let ( // ) = Filename.concat
