(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type error =
  | SyntaxError of {loc: Textual.Location.t; msg: string}
  | BasicError of TextualBasicVerification.error
  | TypeError of TextualTypeVerification.error
  | TransformError of Textual.transform_error list
  | DeclaredTwiceError of TextualDecls.error  (** errors related to Textual *)

val pp_error : Textual.SourceFile.t -> F.formatter -> error -> unit

val error_to_string : Textual.SourceFile.t -> error -> string

val parse_string : Textual.SourceFile.t -> string -> (Textual.Module.t, error list) result
[@@warning "-unused-value-declaration"]

module TextualFile : sig
  type t =
    | StandaloneFile of string  (** Path to a file with textual SIL as content. *)
    | TranslatedFile of {source_path: string; content: string; line_map: LineMap.t}
        (** File with textual SIL [content] which is a result of translation of a file at
            [source_path]. *)

  type sil = {sourcefile: Textual.SourceFile.t; cfg: Cfg.t; tenv: Tenv.t}

  val translate_module :
    Textual.SourceFile.t -> Textual.Module.t -> (sil, Textual.SourceFile.t * error list) result

  val translate : t -> (sil, Textual.SourceFile.t * error list) result

  val capture : use_global_tenv:bool -> sil -> unit

  val line_map : t -> LineMap.t option [@@warning "-unused-value-declaration"]
end

val capture : TextualFile.t list -> unit
(** turn a list of textual files into a SIL-Java program and capture them. *)
