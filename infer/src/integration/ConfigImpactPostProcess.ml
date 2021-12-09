(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let payload_fold ~init ~f =
  let acc = ref init in
  Summary.OnDisk.iter_specs ~f:(fun summary ->
      Payloads.config_impact_analysis summary.payloads
      |> Option.iter ~f:(fun summary -> acc := f summary !acc) ) ;
  !acc


let all_config_fields =
  lazy
    (payload_fold ~init:ConfigImpactAnalysis.Fields.empty ~f:(fun summary acc ->
         ConfigImpactAnalysis.Fields.union acc
           (ConfigImpactAnalysis.Summary.get_config_fields summary) ) )


let all_gated_classes =
  lazy
    (let all_gated_classes =
       payload_fold ~init:ConfigImpactAnalysis.GatedClasses.bottom ~f:(fun summary acc ->
           ConfigImpactAnalysis.GatedClasses.join acc
             (ConfigImpactAnalysis.Summary.get_gated_classes summary) )
     in
     ConfigImpactAnalysis.GatedClasses.fold
       (fun typ conditions acc ->
         if
           ConfigImpactAnalysis.ClassGateConditions.is_gated (Lazy.force all_config_fields)
             conditions
         then Typ.Name.Set.add typ acc
         else acc )
       all_gated_classes Typ.Name.Set.empty )


let instantiate_unchecked_callees_cond summary =
  ConfigImpactAnalysis.Summary.instantiate_unchecked_callees_cond
    ~all_config_fields:(Lazy.force all_config_fields) summary


let is_in_gated_classes pname =
  Procname.get_class_type_name pname
  |> Option.exists ~f:(fun typ_name -> Typ.Name.Set.mem typ_name (Lazy.force all_gated_classes))
