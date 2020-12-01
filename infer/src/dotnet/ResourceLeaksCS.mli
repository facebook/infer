(*
 * Copyright (c) 2017-present, Facebook, Inc.
 * Portions Copyright (c) Microsoft Corporation.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val checker :
  ResourceLeakCSDomain.summary InterproceduralAnalysis.t -> ResourceLeakCSDomain.summary option
 