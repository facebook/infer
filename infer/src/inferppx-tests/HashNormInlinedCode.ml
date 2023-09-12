(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

(* file used only to apply the deriver through [inline] and commit the generated functions
   so as to track changes *)

type record = {s: string; i: int} [@@deriving equal, hash] [@@deriving_inline normalize]

let _ = fun (_ : record) -> ()

let normalize_record t =
  let {s; i} = t in
  let s' = HashNormalizer.StringNormalizer.normalize s in
  if phys_equal s s' then t else {s= s'; i}


let _ = normalize_record

[@@@end]

module RecordHashNormalizer = HashNormalizer.Make (struct
  type t = record [@@deriving equal, hash] [@@deriving_inline normalize]

  let _ = fun (_ : t) -> ()

  let normalize = normalize_record

  let _ = normalize

  [@@@end]
end)

type tuple = string * int * string [@@deriving equal, hash] [@@deriving_inline normalize]

let _ = fun (_ : tuple) -> ()

let normalize_tuple t =
  let x0, x1, x2 = t in
  let x2' = HashNormalizer.StringNormalizer.normalize x2 in
  let x0' = HashNormalizer.StringNormalizer.normalize x0 in
  if phys_equal x2 x2' && phys_equal x0 x0' then t else (x0', x1, x2')


let _ = normalize_tuple

[@@@end]

module TupleHashNormalizer = HashNormalizer.Make (struct
  type t = tuple [@@deriving equal, hash] [@@deriving_inline normalize]

  let _ = fun (_ : t) -> ()

  let normalize = normalize_tuple

  let _ = normalize

  [@@@end]
end)

type variant =
  | NoArgs
  | String of string
  | Int of int
  | Tuple of int * string
  | Record of {i: int; s: string}
  | NonInline of record
[@@deriving equal, hash] [@@deriving_inline normalize]

let _ = fun (_ : variant) -> ()

let normalize_variant t =
  match t with
  | NoArgs ->
      t
  | String x0 ->
      let x0' = HashNormalizer.StringNormalizer.normalize x0 in
      if phys_equal x0 x0' then t else String x0'
  | Int _ ->
      t
  | Tuple (x0, x1) ->
      let x1' = HashNormalizer.StringNormalizer.normalize x1 in
      if phys_equal x1 x1' then t else Tuple (x0, x1')
  | Record {i; s} ->
      let s' = HashNormalizer.StringNormalizer.normalize s in
      if phys_equal s s' then t else Record {i; s= s'}
  | NonInline x0 ->
      let x0' = normalize_record x0 in
      if phys_equal x0 x0' then t else NonInline x0'


let _ = normalize_variant

[@@@end]

module VariantHashNormalizer = HashNormalizer.Make (struct
  type t = variant [@@deriving equal, hash] [@@deriving_inline normalize]

  let _ = fun (_ : t) -> ()

  let normalize = normalize_variant

  let _ = normalize

  [@@@end]
end)

module SourceFile = struct
  type t =
    | HashedBuckOut of string
        (** source file only exists during build under some non-deterministic prefix; however, the
            value here has been post processed to remove non-determinism *)
    | Invalid of {ml_source_file: string}
    | Absolute of string
    | RelativeProjectRoot of string  (** path of the source file relative to the project root *)
    | RelativeProjectRootAndWorkspace of
        { workspace_rel_root: string
              (** path relative to the workspace of the project root with respect to which the
                  source file was captured *)
        ; rel_path: string  (** path of the source file relative to the project root *) }
  [@@deriving equal, hash] [@@deriving_inline normalize]

  let _ = fun (_ : t) -> ()

  let normalize t =
    match t with
    | HashedBuckOut x0 ->
        let x0' = HashNormalizer.StringNormalizer.normalize x0 in
        if phys_equal x0 x0' then t else HashedBuckOut x0'
    | Invalid {ml_source_file} ->
        let ml_source_file' = HashNormalizer.StringNormalizer.normalize ml_source_file in
        if phys_equal ml_source_file ml_source_file' then t
        else Invalid {ml_source_file= ml_source_file'}
    | Absolute x0 ->
        let x0' = HashNormalizer.StringNormalizer.normalize x0 in
        if phys_equal x0 x0' then t else Absolute x0'
    | RelativeProjectRoot x0 ->
        let x0' = HashNormalizer.StringNormalizer.normalize x0 in
        if phys_equal x0 x0' then t else RelativeProjectRoot x0'
    | RelativeProjectRootAndWorkspace {workspace_rel_root; rel_path} ->
        let rel_path' = HashNormalizer.StringNormalizer.normalize rel_path in
        let workspace_rel_root' = HashNormalizer.StringNormalizer.normalize workspace_rel_root in
        if phys_equal rel_path rel_path' && phys_equal workspace_rel_root workspace_rel_root' then t
        else
          RelativeProjectRootAndWorkspace
            {workspace_rel_root= workspace_rel_root'; rel_path= rel_path'}


  let _ = normalize

  [@@@end]

  module Normalizer = HashNormalizer.Make (struct
    type nonrec t = t [@@deriving equal, hash]

    let normalize = normalize
  end)
end

module Location = struct
  type t =
    { file: SourceFile.t  (** The name of the source file *)
    ; line: int  (** The line number. -1 means "do not know" *)
    ; col: int  (** The column number. -1 means "do not know" *)
    ; macro_file_opt: SourceFile.t option
          (** If the location is coming from macro expansion, the name of the file macro is defined
              in *)
    ; macro_line: int  (** If the location is coming from macro expansion, the line number *) }
  [@@deriving equal, hash] [@@deriving_inline normalize]

  let _ = fun (_ : t) -> ()

  let normalize t =
    let {file; line; col; macro_file_opt; macro_line} = t in
    let macro_file_opt' = SourceFile.Normalizer.normalize_opt macro_file_opt in
    let file' = SourceFile.Normalizer.normalize file in
    if phys_equal macro_file_opt macro_file_opt' && phys_equal file file' then t
    else {file= file'; line; col; macro_file_opt= macro_file_opt'; macro_line}


  let _ = normalize

  [@@@end]

  module Normalizer = HashNormalizer.Make (struct
    type nonrec t = t [@@deriving equal, hash]

    let normalize = normalize
  end)
end

module CSharpClassName = struct
  type t = {classname: string; namespace: string option}
  [@@deriving equal, hash] [@@deriving_inline normalize]

  let _ = fun (_ : t) -> ()

  let normalize t =
    let {classname; namespace} = t in
    let namespace' = HashNormalizer.StringNormalizer.normalize_opt namespace in
    let classname' = HashNormalizer.StringNormalizer.normalize classname in
    if phys_equal namespace namespace' && phys_equal classname classname' then t
    else {classname= classname'; namespace= namespace'}


  let _ = normalize

  [@@@end]

  module Normalizer = HashNormalizer.Make (struct
    type nonrec t = t [@@deriving equal, hash]

    let normalize = normalize
  end)
end

module JavaClassName = struct
  type t = {classname: string; namespace: string option}
  [@@deriving equal, hash] [@@deriving_inline normalize]

  let _ = fun (_ : t) -> ()

  let normalize t =
    let {classname; namespace} = t in
    let namespace' = HashNormalizer.StringNormalizer.normalize_opt namespace in
    let classname' = HashNormalizer.StringNormalizer.normalize classname in
    if phys_equal namespace namespace' && phys_equal classname classname' then t
    else {classname= classname'; namespace= namespace'}


  let _ = normalize

  [@@@end]

  module Normalizer = HashNormalizer.Make (struct
    type nonrec t = t [@@deriving equal, hash]

    let normalize = normalize
  end)
end
