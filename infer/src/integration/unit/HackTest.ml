(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let%test_module "naming_table" =
  ( module struct
    (* Scaled down version of Hack's naming table.
       The full version is
       CREATE TABLE NAMING_FILE_INFO(
          FILE_INFO_ID INTEGER PRIMARY KEY AUTOINCREMENT,
          PATH_PREFIX_TYPE INTEGER NOT NULL,
          PATH_SUFFIX TEXT NOT NULL,
          TYPE_CHECKER_MODE INTEGER,
          DECL_HASH INTEGER,
          CLASSES TEXT,
          CONSTS TEXT,
          FUNS TEXT,
          TYPEDEFS TEXT,
          MODULES TEXT
        );
    *)
  end )
