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

include struct
  [@@@ocaml.warning "-60"]

  let _ = fun (_ : record) -> ()

  let hash_normalize_record =
    let module T = struct
      type nonrec t = record

      let equal = equal_record

      let _ = equal

      let hash = hash_record

      let _ = hash
    end in
    let module H = Caml.Hashtbl.Make (T) in
    let table : T.t H.t = H.create 11 in
    let () = HashNormalizer.register_reset (fun () -> H.reset table) in
    let normalize t =
      let {s; i} = t in
      let s' = HashNormalizer.String.hash_normalize s in
      if phys_equal s s' then t else {s= s'; i}
    in
    fun t ->
      match H.find_opt table t with
      | Some t' ->
          t'
      | None ->
          let normalized = normalize t in
          H.add table normalized normalized ;
          normalized


  let _ = hash_normalize_record

  let hash_normalize_record_opt = function
    | Some _ as some_t ->
        IOption.map_changed ~equal:phys_equal ~f:hash_normalize_record some_t
    | None ->
        None


  let _ = hash_normalize_record_opt

  let hash_normalize_record_list ts =
    IList.map_changed ~equal:phys_equal ~f:hash_normalize_record ts


  let _ = hash_normalize_record_list
end [@@ocaml.doc "@inline"]

[@@@end]

type tuple = string * int * string [@@deriving equal, hash] [@@deriving_inline normalize]

include struct
  [@@@ocaml.warning "-60"]

  let _ = fun (_ : tuple) -> ()

  let hash_normalize_tuple =
    let module T = struct
      type nonrec t = tuple

      let equal = equal_tuple

      let _ = equal

      let hash = hash_tuple

      let _ = hash
    end in
    let module H = Caml.Hashtbl.Make (T) in
    let table : T.t H.t = H.create 11 in
    let () = HashNormalizer.register_reset (fun () -> H.reset table) in
    let normalize t =
      let x0, x1, x2 = t in
      let x2' = HashNormalizer.String.hash_normalize x2 in
      let x0' = HashNormalizer.String.hash_normalize x0 in
      if phys_equal x2 x2' && phys_equal x0 x0' then t else (x0', x1, x2')
    in
    fun t ->
      match H.find_opt table t with
      | Some t' ->
          t'
      | None ->
          let normalized = normalize t in
          H.add table normalized normalized ;
          normalized


  let _ = hash_normalize_tuple

  let hash_normalize_tuple_opt = function
    | Some _ as some_t ->
        IOption.map_changed ~equal:phys_equal ~f:hash_normalize_tuple some_t
    | None ->
        None


  let _ = hash_normalize_tuple_opt

  let hash_normalize_tuple_list ts = IList.map_changed ~equal:phys_equal ~f:hash_normalize_tuple ts

  let _ = hash_normalize_tuple_list
end [@@ocaml.doc "@inline"]

[@@@end]

type variant =
  | NoArgs
  | String of string
  | Int of int
  | Tuple of int * string
  | Record of {i: int; s: string}
  | NonInline of record
[@@deriving equal, hash] [@@deriving_inline normalize]

include struct
  [@@@ocaml.warning "-60"]

  let _ = fun (_ : variant) -> ()

  let hash_normalize_variant =
    let module T = struct
      type nonrec t = variant

      let equal = equal_variant

      let _ = equal

      let hash = hash_variant

      let _ = hash
    end in
    let module H = Caml.Hashtbl.Make (T) in
    let table : T.t H.t = H.create 11 in
    let () = HashNormalizer.register_reset (fun () -> H.reset table) in
    let normalize t =
      match t with
      | NoArgs ->
          t
      | String x0 ->
          let x0' = HashNormalizer.String.hash_normalize x0 in
          if phys_equal x0 x0' then t else String x0'
      | Int _ ->
          t
      | Tuple (x0, x1) ->
          let x1' = HashNormalizer.String.hash_normalize x1 in
          if phys_equal x1 x1' then t else Tuple (x0, x1')
      | Record {i; s} ->
          let s' = HashNormalizer.String.hash_normalize s in
          if phys_equal s s' then t else Record {i; s= s'}
      | NonInline x0 ->
          let x0' = hash_normalize_record x0 in
          if phys_equal x0 x0' then t else NonInline x0'
    in
    fun t ->
      match H.find_opt table t with
      | Some t' ->
          t'
      | None ->
          let normalized = normalize t in
          H.add table normalized normalized ;
          normalized


  let _ = hash_normalize_variant

  let hash_normalize_variant_opt = function
    | Some _ as some_t ->
        IOption.map_changed ~equal:phys_equal ~f:hash_normalize_variant some_t
    | None ->
        None


  let _ = hash_normalize_variant_opt

  let hash_normalize_variant_list ts =
    IList.map_changed ~equal:phys_equal ~f:hash_normalize_variant ts


  let _ = hash_normalize_variant_list
end [@@ocaml.doc "@inline"]

[@@@end]

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

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()

    let hash_normalize =
      let module T = struct
        type nonrec t = t

        let equal = equal

        let _ = equal

        let hash = hash

        let _ = hash
      end in
      let module H = Caml.Hashtbl.Make (T) in
      let table : T.t H.t = H.create 11 in
      let () = HashNormalizer.register_reset (fun () -> H.reset table) in
      let normalize t =
        match t with
        | HashedBuckOut x0 ->
            let x0' = HashNormalizer.String.hash_normalize x0 in
            if phys_equal x0 x0' then t else HashedBuckOut x0'
        | Invalid {ml_source_file} ->
            let ml_source_file' = HashNormalizer.String.hash_normalize ml_source_file in
            if phys_equal ml_source_file ml_source_file' then t
            else Invalid {ml_source_file= ml_source_file'}
        | Absolute x0 ->
            let x0' = HashNormalizer.String.hash_normalize x0 in
            if phys_equal x0 x0' then t else Absolute x0'
        | RelativeProjectRoot x0 ->
            let x0' = HashNormalizer.String.hash_normalize x0 in
            if phys_equal x0 x0' then t else RelativeProjectRoot x0'
        | RelativeProjectRootAndWorkspace {workspace_rel_root; rel_path} ->
            let rel_path' = HashNormalizer.String.hash_normalize rel_path in
            let workspace_rel_root' = HashNormalizer.String.hash_normalize workspace_rel_root in
            if phys_equal rel_path rel_path' && phys_equal workspace_rel_root workspace_rel_root'
            then t
            else
              RelativeProjectRootAndWorkspace
                {workspace_rel_root= workspace_rel_root'; rel_path= rel_path'}
      in
      fun t ->
        match H.find_opt table t with
        | Some t' ->
            t'
        | None ->
            let normalized = normalize t in
            H.add table normalized normalized ;
            normalized


    let _ = hash_normalize

    let hash_normalize_opt = function
      | Some _ as some_t ->
          IOption.map_changed ~equal:phys_equal ~f:hash_normalize some_t
      | None ->
          None


    let _ = hash_normalize_opt

    let hash_normalize_list ts = IList.map_changed ~equal:phys_equal ~f:hash_normalize ts

    let _ = hash_normalize_list
  end [@@ocaml.doc "@inline"]

  [@@@end]

  module Normalizer : HashNormalizer.S with type t = t = struct
    type nonrec t = t

    let normalize = hash_normalize

    let normalize_opt = hash_normalize_opt

    let normalize_list = hash_normalize_list
  end
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

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()

    let hash_normalize =
      let module T = struct
        type nonrec t = t

        let equal = equal

        let _ = equal

        let hash = hash

        let _ = hash
      end in
      let module H = Caml.Hashtbl.Make (T) in
      let table : T.t H.t = H.create 11 in
      let () = HashNormalizer.register_reset (fun () -> H.reset table) in
      let normalize t =
        let {file; line; col; macro_file_opt; macro_line} = t in
        let macro_file_opt' = SourceFile.hash_normalize_opt macro_file_opt in
        let file' = SourceFile.hash_normalize file in
        if phys_equal macro_file_opt macro_file_opt' && phys_equal file file' then t
        else {file= file'; line; col; macro_file_opt= macro_file_opt'; macro_line}
      in
      fun t ->
        match H.find_opt table t with
        | Some t' ->
            t'
        | None ->
            let normalized = normalize t in
            H.add table normalized normalized ;
            normalized


    let _ = hash_normalize

    let hash_normalize_opt = function
      | Some _ as some_t ->
          IOption.map_changed ~equal:phys_equal ~f:hash_normalize some_t
      | None ->
          None


    let _ = hash_normalize_opt

    let hash_normalize_list ts = IList.map_changed ~equal:phys_equal ~f:hash_normalize ts

    let _ = hash_normalize_list
  end [@@ocaml.doc "@inline"]

  [@@@end]

  module Normalizer : HashNormalizer.S with type t = t = struct
    type nonrec t = t

    let normalize = hash_normalize

    let normalize_opt = hash_normalize_opt

    let normalize_list = hash_normalize_list
  end
end

module CSharpClassName = struct
  type t = {classname: string; namespace: string option}
  [@@deriving equal, hash] [@@deriving_inline normalize]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()

    let hash_normalize =
      let module T = struct
        type nonrec t = t

        let equal = equal

        let _ = equal

        let hash = hash

        let _ = hash
      end in
      let module H = Caml.Hashtbl.Make (T) in
      let table : T.t H.t = H.create 11 in
      let () = HashNormalizer.register_reset (fun () -> H.reset table) in
      let normalize t =
        let {classname; namespace} = t in
        let namespace' = HashNormalizer.String.hash_normalize_opt namespace in
        let classname' = HashNormalizer.String.hash_normalize classname in
        if phys_equal namespace namespace' && phys_equal classname classname' then t
        else {classname= classname'; namespace= namespace'}
      in
      fun t ->
        match H.find_opt table t with
        | Some t' ->
            t'
        | None ->
            let normalized = normalize t in
            H.add table normalized normalized ;
            normalized


    let _ = hash_normalize

    let hash_normalize_opt = function
      | Some _ as some_t ->
          IOption.map_changed ~equal:phys_equal ~f:hash_normalize some_t
      | None ->
          None


    let _ = hash_normalize_opt

    let hash_normalize_list ts = IList.map_changed ~equal:phys_equal ~f:hash_normalize ts

    let _ = hash_normalize_list
  end [@@ocaml.doc "@inline"]

  [@@@end]

  module Normalizer : HashNormalizer.S with type t = t = struct
    type nonrec t = t

    let normalize = hash_normalize

    let normalize_opt = hash_normalize_opt

    let normalize_list = hash_normalize_list
  end
end

module JavaClassName = struct
  type t = {classname: string; namespace: string option}
  [@@deriving equal, hash] [@@deriving_inline normalize]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()

    let hash_normalize =
      let module T = struct
        type nonrec t = t

        let equal = equal

        let _ = equal

        let hash = hash

        let _ = hash
      end in
      let module H = Caml.Hashtbl.Make (T) in
      let table : T.t H.t = H.create 11 in
      let () = HashNormalizer.register_reset (fun () -> H.reset table) in
      let normalize t =
        let {classname; namespace} = t in
        let namespace' = HashNormalizer.String.hash_normalize_opt namespace in
        let classname' = HashNormalizer.String.hash_normalize classname in
        if phys_equal namespace namespace' && phys_equal classname classname' then t
        else {classname= classname'; namespace= namespace'}
      in
      fun t ->
        match H.find_opt table t with
        | Some t' ->
            t'
        | None ->
            let normalized = normalize t in
            H.add table normalized normalized ;
            normalized


    let _ = hash_normalize

    let hash_normalize_opt = function
      | Some _ as some_t ->
          IOption.map_changed ~equal:phys_equal ~f:hash_normalize some_t
      | None ->
          None


    let _ = hash_normalize_opt

    let hash_normalize_list ts = IList.map_changed ~equal:phys_equal ~f:hash_normalize ts

    let _ = hash_normalize_list
  end [@@ocaml.doc "@inline"]

  [@@@end]

  module Normalizer : HashNormalizer.S with type t = t = struct
    type nonrec t = t

    let normalize = hash_normalize

    let normalize_opt = hash_normalize_opt

    let normalize_list = hash_normalize_list
  end
end
