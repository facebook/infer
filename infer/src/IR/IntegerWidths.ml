(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The Smallfoot Intermediate Language: Types *)

open! IStd

type t = {char_width: int; short_width: int; int_width: int; long_width: int; longlong_width: int}
[@@deriving compare, equal]

let java = {char_width= 16; short_width= 16; int_width= 32; long_width= 64; longlong_width= 64}

module SQLite = SqliteUtils.MarshalledNullableDataNOTForComparison (struct
  type nonrec t = t
end)

let load =
  let load_statement =
    Database.register_statement CaptureDatabase
      "SELECT integer_type_widths FROM source_files WHERE source_file = :k"
  in
  fun source ->
    let res_opt =
      Database.with_registered_statement load_statement ~f:(fun db load_stmt ->
          SourceFile.SQLite.serialize source
          |> Sqlite3.bind load_stmt 1
          |> SqliteUtils.check_result_code db ~log:"load bind source file" ;
          SqliteUtils.result_single_column_option ~finalize:false ~log:"Typ.IntegerWidths.load" db
            load_stmt
          |> Option.map ~f:SQLite.deserialize )
    in
    match res_opt with
    | None ->
        MissingDependencies.record_sourcefile source ;
        None
    | Some some_opt ->
        some_opt


let width_of_ikind {char_width; short_width; int_width; long_width; longlong_width} ikind =
  match (ikind : Typ.ikind) with
  | IBool ->
      8
  | ISChar | IChar | IUChar ->
      char_width
  | IShort | IUShort ->
      short_width
  | IInt | IUInt ->
      int_width
  | ILong | IULong ->
      long_width
  | ILongLong | IULongLong ->
      longlong_width
  | I128 | IU128 ->
      128


let range_of_ikind =
  let range bits ~unsigned =
    if unsigned then Z.(~$0, shift_left ~$1 bits - ~$1)
    else
      let bound = Z.(shift_left ~$1) (bits - 1) in
      Z.(~-bound, bound - ~$1)
  in
  fun integer_widths (x : Typ.ikind) ->
    let bits_for_range = match x with IBool -> 1 | _ -> width_of_ikind integer_widths x in
    range bits_for_range ~unsigned:(Typ.ikind_is_unsigned x)
