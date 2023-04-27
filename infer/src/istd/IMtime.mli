(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val span_to_s_float : Mtime.Span.t -> float

val span_to_us_int : Mtime.Span.t -> int

val span_to_ms_int : Mtime.Span.t -> int [@@warning "-unused-value-declaration"]
