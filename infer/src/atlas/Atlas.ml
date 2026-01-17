open! IStd

let checker (analysis_data : IntraproceduralAnalysis.t) : unit =
  let init_state = AtlasDomain.empty in
  let _final_state = AtlasTransfer.run analysis_data init_state in
  let proc_name = Procdesc.get_proc_name analysis_data.proc_desc in
  Format.printf
    "@[<v2>Atlas finished procedure %a:@,%a@]@."
    Procname.pp proc_name
    AtlasDomain.pp _final_state
