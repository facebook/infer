(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let%test_module "failed specializations" =
  ( module struct
    let alias_specialization i =
      let param name =
        Specialization.HeapPath.Pvar
          (Pvar.mk (Mangled.from_string name) (Procname.from_string_c_fun "f"))
      in
      { Specialization.Pulse.bottom with
        Specialization.Pulse.aliases= Some [[param (sprintf "a%d" i); param (sprintf "b%d" i)]] }


    let alias = alias_specialization 0

    let summary =
      { PulseSummary.main= PulseSummary.empty
      ; specialized= Specialization.Pulse.Map.empty
      ; failed= Specialization.Pulse.Map.empty }


    let failed = PulseSummary.add_failed alias summary

    let computed = {summary with specialized= Specialization.Pulse.Map.singleton alias summary.main}

    let is_already_specialized specialization summary =
      Pulse.is_already_specialized (Specialization.Pulse specialization) summary


    let%test "a failed specialization is not analyzed again" = is_already_specialized alias failed

    let%test "other specializations are still analyzed" =
      not (is_already_specialized Specialization.Pulse.bottom failed)


    let%test "failed specializations do not count toward the specialization limit" =
      let failed =
        List.init Config.pulse_specialization_limit ~f:alias_specialization
        |> List.fold ~init:summary ~f:(fun summary specialization ->
            PulseSummary.add_failed specialization summary )
      in
      not (Specialization.Pulse.is_pulse_specialization_limit_reached failed.specialized)


    let%test "storing a summary without the failed specialization keeps the failure" =
      PulseSummary.is_failed alias (PulseSummary.merge summary failed)


    let%test "a computed specialization wins over a failure on merge" =
      List.for_all
        [PulseSummary.merge failed computed; PulseSummary.merge computed failed]
        ~f:(fun (merged : PulseSummary.t) ->
          Specialization.Pulse.Map.mem alias merged.specialized
          && not (PulseSummary.is_failed alias merged) )


    let%test "failures recorded by an earlier run are ignored and dropped" =
      let stale = {summary with failed= Specialization.Pulse.Map.singleton alias "earlier run"} in
      (not (is_already_specialized alias stale))
      && Specialization.Pulse.Map.is_empty (PulseSummary.merge summary stale).failed
  end )
