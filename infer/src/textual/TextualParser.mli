(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type error

val pp_error : Textual.SourceFile.t -> F.formatter -> error -> unit
  [@@warning "-unused-value-declaration"]

val log_error : Textual.SourceFile.t -> error -> unit

val parse_string : Textual.SourceFile.t -> string -> (Textual.Module.t, error list) result
  [@@warning "-unused-value-declaration"]

module TextualFile : sig
  type t =
    | StandaloneFile of string  (** Path to a file with textual SIL as content. *)
    | TranslatedFile of {source_path: string; content: string; line_map: LineMap.t}
        (** File with textual SIL [content] which is a result of translation of a file at
            [source_path]. *)

  type sil

  val translate : t -> (sil, Textual.SourceFile.t * error list) result

  val capture : sil -> Tenv.t

  val line_map : t -> LineMap.t option [@@warning "-unused-value-declaration"]
end

val capture : TextualFile.t list -> unit
