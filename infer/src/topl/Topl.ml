(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let automaton = lazy (ToplAutomaton.make Config.topl_properties)

let automaton () = Lazy.force automaton

let is_active () = Config.is_checker_enabled Topl && not (List.is_empty Config.topl_properties)
