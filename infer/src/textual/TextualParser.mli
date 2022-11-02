(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type error

module TextualFile : sig
  type t =
    | StandaloneFile of string  (** Path to a file with textual SIL as content. *)
    | TranslatedFile of {source_path: string; content: string}
        (** File with textual SIL [content] which is a result of translation of a file at
            [source_path]. *)
end

val parse_string : SourceFile.t -> string -> (Textual.Module.t, error list) result [@@warning "-32"]

val pp_error : SourceFile.t -> F.formatter -> error -> unit [@@warning "-32"]

val capture_one : TextualFile.t -> unit

val capture : TextualFile.t list -> unit
