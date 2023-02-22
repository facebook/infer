(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let span_to_s_float span = Mtime.Span.to_float_ns span /. 1_000_000_000.0

let span_to_us_int span =
  let dur_ns = Mtime.Span.to_uint64_ns span in
  Int64.(dur_ns / 1000L) |> Int64.to_int_trunc


let span_to_ms_int span =
  let dur_ns = Mtime.Span.to_uint64_ns span in
  Int64.(dur_ns / 1_000_000L) |> Int64.to_int_trunc
