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
      |> ILazy.force_option
      |> Option.iter ~f:(fun summary -> acc := f summary !acc) ) ;
  !acc


let all_configs =
  let open ConfigImpactAnalysis in
  lazy
    (let config_cond, field_alias =
       payload_fold ~init:(LatentConfigs.empty, LatentConfigAlias.empty)
         ~f:(fun summary (acc_cond, acc_alias) ->
           let acc_cond = LatentConfigs.union acc_cond (Summary.get_configs summary) in
           let acc_alias =
             LatentConfigAlias.union acc_alias (Summary.get_latent_config_alias summary)
           in
           (acc_cond, acc_alias) )
     in
     let rec apply_alias worklist acc =
       match worklist with
       | [] ->
           acc
       | hd :: tl when LatentConfigs.mem hd acc ->
           apply_alias tl acc
       | hd :: tl ->
           let worklist = LatentConfigAlias.get_all hd field_alias @ tl in
           let acc = LatentConfigs.add hd acc in
           apply_alias worklist acc
     in
     apply_alias (LatentConfigs.elements config_cond) LatentConfigs.empty )


let all_gated_classes =
  let open ConfigImpactAnalysis in
  lazy
    (let all_gated_classes =
       payload_fold ~init:GatedClasses.bottom ~f:(fun summary acc ->
           GatedClasses.join acc (Summary.get_gated_classes summary) )
     in
     GatedClasses.fold
       (fun typ conditions acc ->
         if ClassGateConditions.is_gated (Lazy.force all_configs) conditions then
           Typ.Name.Set.add typ acc
         else acc )
       all_gated_classes Typ.Name.Set.empty )


let instantiate_unchecked_callees_cond summary =
  ConfigImpactAnalysis.Summary.instantiate_unchecked_callees_cond
    ~all_configs:(Lazy.force all_configs) summary


let is_in_gated_classes pname =
  Procname.get_class_type_name pname
  |> Option.exists ~f:(fun typ_name -> Typ.Name.Set.mem typ_name (Lazy.force all_gated_classes))
