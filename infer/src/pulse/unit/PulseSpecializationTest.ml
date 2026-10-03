(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let%test_module "failed specializations" =
  ( module struct
    let alias_specialization =
      let param name =
        Specialization.HeapPath.Pvar
          (Pvar.mk (Mangled.from_string name) (Procname.from_string_c_fun "f"))
      in
      {Specialization.Pulse.bottom with Specialization.Pulse.aliases= Some [[param "a"; param "b"]]}


    let summary =
      {PulseSummary.main= PulseSummary.empty; specialized= Specialization.Pulse.Map.empty}


    let failed =
      Pulse.mark_specialization_failed (Specialization.Pulse alias_specialization) summary


    let%test "a failed specialization is recorded with the main summary" =
      Pulse.is_already_specialized (Specialization.Pulse alias_specialization) failed
      && phys_equal
           (Specialization.Pulse.Map.find alias_specialization failed.PulseSummary.specialized)
           failed.PulseSummary.main


    let%test "other specializations are still requested" =
      not (Pulse.is_already_specialized (Specialization.Pulse Specialization.Pulse.bottom) failed)


    let%test "storing a summary without the failed specialization keeps the failure" =
      Pulse.is_already_specialized (Specialization.Pulse alias_specialization)
        (PulseSummary.merge summary failed)
  end )
