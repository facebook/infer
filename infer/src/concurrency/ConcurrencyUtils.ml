(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

let get_java_class_initializer_summary_of {InterproceduralAnalysis.proc_desc; analyze_dependency} =
  let procname = Procdesc.get_proc_name proc_desc in
  match procname with
  | Procname.Java _ ->
      Procname.get_class_type_name procname
      |> Option.map ~f:(fun tname -> Procname.Java (Procname.Java.get_class_initializer tname))
      |> Option.bind ~f:(fun proc_name -> analyze_dependency proc_name |> AnalysisResult.to_option)
  | _ ->
      None


let get_java_constructor_summaries_of {InterproceduralAnalysis.proc_desc; tenv; analyze_dependency}
    =
  let procname = Procdesc.get_proc_name proc_desc in
  Procname.get_class_type_name procname
  (* retrieve its definition *)
  |> Option.bind ~f:(Tenv.lookup tenv)
  (* get the list of methods in the class *)
  |> Option.value_map ~default:[] ~f:(fun (tstruct : Struct.t) -> tstruct.methods)
  (* keep only the constructors *)
  |> List.filter ~f:Procname.(function Java jname -> Java.is_constructor jname | _ -> false)
  (* get the summaries of the constructors *)
  |> List.filter_map ~f:(fun proc_name -> analyze_dependency proc_name |> AnalysisResult.to_option)
