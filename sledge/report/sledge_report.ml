(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Command = Core.Command
module Tbl = String.Tbl

let read filename =
  let tbl = Tbl.create () in
  let sexps = try Sexp.load_sexps filename with Sys_error _ -> [] in
  List.iter sexps ~f:(fun sexp ->
      let {Report.name; entry} = Report.t_of_sexp sexp in
      match (Tbl.find_opt tbl name, entry) with
      | None, ProcessTimes t -> Tbl.replace tbl name ([t], [], [], [])
      | None, GcStats g -> Tbl.replace tbl name ([], [g], [], [])
      | None, Coverage c -> Tbl.replace tbl name ([], [], [c], [])
      | None, Status s -> Tbl.replace tbl name ([], [], [], [s])
      | Some (times, gcs, coverages, statuses), ProcessTimes ptimes ->
          Tbl.replace tbl name (ptimes :: times, gcs, coverages, statuses)
      | Some (times, gcs, coverages, statuses), GcStats gc ->
          Tbl.replace tbl name (times, gc :: gcs, coverages, statuses)
      | Some (times, gcs, coverages, statuses), Coverage c ->
          Tbl.replace tbl name (times, gcs, c :: coverages, statuses)
      | Some (times, gc, coverages, statuses), Status status ->
          Tbl.replace tbl name (times, gc, coverages, status :: statuses) ) ;
  tbl

type times = {etime: float; utime: float; stime: float}

type ('t, 'g) row =
  { name: string
  ; times: 't
  ; times_deltas: 't
  ; gcs: 'g
  ; gcs_deltas: 'g
  ; cov: Report.coverage list
  ; cov_deltas: Report.coverage list option
  ; status: Report.status list
  ; status_deltas: Report.status list option }

let times_of_raw {Report.etime; utime; stime; cutime; cstime} =
  let utime = utime +. cutime in
  let stime = stime +. cstime in
  let etime = etime in
  {etime; utime; stime}

let add_time base_times ptimes row =
  let tustimes = times_of_raw ptimes in
  let times = tustimes :: row.times in
  let times_deltas =
    Option.fold base_times row.times_deltas
      ~f:(fun {etime= btt; utime= but; stime= bst} times_deltas ->
        let {etime= tt; utime= ut; stime= st} = tustimes in
        {etime= tt -. btt; utime= ut -. but; stime= st -. bst}
        :: times_deltas )
  in
  {row with times; times_deltas}

let add_times base_times times row =
  if List.is_empty times then
    {row with times_deltas= Option.to_list base_times}
  else List.fold ~f:(add_time base_times) times row

let add_gc base_gcs gc row =
  let gcs = gc :: row.gcs in
  let gcs_deltas =
    Option.fold base_gcs row.gcs_deltas ~f:(fun bgc gcs_deltas ->
        Report.
          { allocated= gc.allocated -. bgc.allocated
          ; promoted= gc.promoted -. bgc.promoted
          ; peak_size= gc.peak_size -. bgc.peak_size }
        :: gcs_deltas )
  in
  {row with gcs; gcs_deltas}

let add_gcs base_gcs gcs row =
  if List.is_empty gcs then {row with gcs_deltas= Option.to_list base_gcs}
  else List.fold ~f:(add_gc base_gcs) gcs row

let add_cov base_cov cov row =
  if List.mem ~eq:Report.equal_coverage cov row.cov then row
  else
    let cov_deltas =
      match base_cov with
      | Some (base_cov :: _) ->
          let covd =
            Report.
              { steps= cov.steps - base_cov.steps
              ; hit= cov.hit - base_cov.hit
              ; fraction= cov.fraction -. base_cov.fraction
              ; solver_steps= cov.solver_steps - base_cov.solver_steps }
          in
          Some (covd :: Option.value row.cov_deltas ~default:[])
      | _ -> None
    in
    {row with cov= cov :: row.cov; cov_deltas}

let add_covs base_cov covs row =
  let row = List.fold ~f:(add_cov base_cov) covs row in
  {row with cov= List.sort ~cmp:(Ord.opp Report.compare_coverage) row.cov}

let add_status base_status status row =
  if List.mem ~eq:Report.equal_status status row.status then row
  else
    match base_status with
    | Some base_status
      when not (List.mem ~eq:Report.equal_status status base_status) ->
        { row with
          status= status :: row.status
        ; status_deltas=
            Some (base_status @ Option.value row.status_deltas ~default:[])
        }
    | _ -> {row with status= status :: row.status}

let add_statuses base_status statuses row =
  let row = List.fold ~f:(add_status base_status) statuses row in
  {row with status= List.sort ~cmp:Report.compare_status row.status}

let ave_floats flts =
  assert (not (Iter.is_empty flts)) ;
  let min, max, sum, num =
    Iter.fold flts (Float.infinity, Float.neg_infinity, 0., 0)
      ~f:(fun flt (min, max, sum, num) ->
        (Float.min min flt, Float.max max flt, sum +. flt, num + 1) )
  in
  if num >= 5 then (sum -. min -. max) /. Float.of_int (num - 2)
  else sum /. Float.of_int num

let combine name b_result c_result =
  let base_times, base_gcs, base_cov, base_status =
    match b_result with
    | Some (times, gcs, covs, statuses) ->
        let times =
          if List.is_empty times then None
          else
            let etimes, utimes, stimes, cutimes, cstimes =
              let init =
                (Iter.empty, Iter.empty, Iter.empty, Iter.empty, Iter.empty)
              in
              List.fold times init
                ~f:(fun {Report.etime; utime; stime; cutime; cstime}
                   (etimes, utimes, stimes, cutimes, cstimes)
                   ->
                  ( Iter.cons etime etimes
                  , Iter.cons utime utimes
                  , Iter.cons stime stimes
                  , Iter.cons cutime cutimes
                  , Iter.cons cstime cstimes ) )
            in
            Some
              (times_of_raw
                 { etime= ave_floats etimes
                 ; utime= ave_floats utimes
                 ; stime= ave_floats stimes
                 ; cutime= ave_floats cutimes
                 ; cstime= ave_floats cstimes })
        in
        let gcs =
          if List.is_empty gcs then None
          else
            let allocs, promos, peaks =
              List.fold gcs (Iter.empty, Iter.empty, Iter.empty)
                ~f:(fun {Report.allocated; promoted; peak_size}
                   (allocs, promos, peaks)
                   ->
                  ( Iter.cons allocated allocs
                  , Iter.cons promoted promos
                  , Iter.cons peak_size peaks ) )
            in
            Some
              Report.
                { allocated= ave_floats allocs
                ; promoted= ave_floats promos
                ; peak_size= ave_floats peaks }
        in
        let covs = if List.is_empty covs then None else Some covs in
        let status =
          Some (List.sort_uniq ~cmp:Report.compare_status statuses)
        in
        (times, gcs, covs, status)
    | None -> (None, None, None, None)
  in
  let row =
    { name
    ; times= []
    ; times_deltas= []
    ; gcs= []
    ; gcs_deltas= []
    ; cov= []
    ; cov_deltas= None
    ; status= []
    ; status_deltas= None }
  in
  match c_result with
  | None ->
      let times_deltas = Option.to_list base_times in
      let gcs_deltas = Option.to_list base_gcs in
      let cov_deltas = base_cov in
      let status_deltas = base_status in
      {row with times_deltas; gcs_deltas; cov_deltas; status_deltas}
  | Some (c_times, c_gcs, c_cov, c_statuses) ->
      row
      |> add_times base_times c_times
      |> add_gcs base_gcs c_gcs
      |> add_covs base_cov c_cov
      |> add_statuses base_status c_statuses

type ranges =
  { max_time: float
  ; pct_time: float
  ; max_alloc: float
  ; pct_alloc: float
  ; max_promo: float
  ; pct_promo: float
  ; max_peak: float
  ; pct_peak: float }

let ranges rows =
  let init =
    { max_time= 0.
    ; pct_time= 0.
    ; max_alloc= 0.
    ; pct_alloc= 0.
    ; max_promo= 0.
    ; pct_promo= 0.
    ; max_peak= 0.
    ; pct_peak= 0. }
  in
  Iter.fold rows init ~f:(fun {times; times_deltas; gcs; gcs_deltas} acc ->
      Option.fold times_deltas acc ~f:(fun deltas acc ->
          let max_time = Float.max acc.max_time (Float.abs deltas.etime) in
          let pct_time =
            Option.fold times acc.pct_time ~f:(fun times pct_time ->
                let pct = 100. *. deltas.etime /. times.etime in
                Float.max pct_time (Float.abs pct) )
          in
          {acc with max_time; pct_time} )
      |> fun acc ->
      Option.fold gcs_deltas acc ~f:(fun deltas acc ->
          let max_alloc = Float.max acc.max_alloc deltas.Report.allocated in
          let pct_alloc =
            Option.fold gcs acc.pct_alloc ~f:(fun gcs pct_alloc ->
                let pct =
                  100. *. deltas.Report.allocated /. gcs.Report.allocated
                in
                Float.max pct_alloc (Float.abs pct) )
          in
          let max_promo = Float.max acc.max_promo deltas.Report.promoted in
          let pct_promo =
            Option.fold gcs acc.pct_promo ~f:(fun gcs pct_promo ->
                let pct =
                  100. *. deltas.Report.promoted /. gcs.Report.promoted
                in
                Float.max pct_promo (Float.abs pct) )
          in
          let max_peak = Float.max acc.max_peak deltas.Report.peak_size in
          let pct_peak =
            Option.fold gcs acc.pct_peak ~f:(fun gcs pct_peak ->
                let pct =
                  100. *. deltas.Report.peak_size /. gcs.Report.peak_size
                in
                Float.max pct_peak (Float.abs pct) )
          in
          { acc with
            max_alloc
          ; pct_alloc
          ; max_promo
          ; pct_promo
          ; max_peak
          ; pct_peak } ) )

let color max dat =
  (* linear interpolation mapping -1 to green, 0 to lace, and 1 to red *)
  let green = (133., 153., 0.) in
  let lace = (253., 246., 227.) in
  let red = (220., 50., 47.) in
  let gradient x =
    let scale x (r0, g0, b0) (r1, g1, b1) =
      let scale1 x c0 c1 = (x *. (c1 -. c0)) +. c0 in
      (scale1 x r0 r1, scale1 x g0 g1, scale1 x b0 b1)
    in
    let x = Float.max (-1.) (Float.min x 1.) in
    if Float.(x < 0.) then scale (-.x) lace green else scale x lace red
  in
  let rgb_to_hex (r, g, b) =
    let to_int x = Int.min 255 (Int.max 0 (Float.to_int x)) in
    Printf.sprintf "#%2x%2x%2x" (to_int r) (to_int g) (to_int b)
  in
  let rat = dat /. max in
  if Float.(abs dat < 0.000001 || is_nan rat) then rgb_to_hex lace
  else rgb_to_hex (gradient rat)

let write_html ranges rows chan =
  let pf fmt = Printf.fprintf chan fmt in
  pf "<html><head><title>Test results</title><style>\n" ;
  pf ".base2 { background-color: #eee8d5; }\n" ;
  pf ".regress { background-color: #dc322f; }\n" ;
  pf ".neutral { background-color: #b58900; }\n" ;
  pf ".improve { background-color: #859900; }\n" ;
  pf "th { background-color: #eee8d5; position: sticky; top: 0; }" ;
  pf "th, td { padding: 5px; }\n" ;
  pf "</style></head>\n" ;
  pf "<body style=\"background-color:#fdf6e3\">" ;
  pf
    {|<table style="border-collapse: collapse">
        <tr>
          <th>Test</th>
          <th>elapsed<br>(sec)</th>
          <th>&Delta;<br></th>
          <th><math><mrow><mfrac><mi>prev</mi><mi>curr</mi></mfrac></mrow></math></th>
          <th>user<br>(sec)</th>
          <th>&Delta;<br></th>
          <th><math><mrow><mfrac><mi>prev</mi><mi>curr</mi></mfrac></mrow></math></th>
          <th>system<br>(sec)</th>
          <th>&Delta;<br></th>
          <th><math><mrow><mfrac><mi>prev</mi><mi>curr</mi></mfrac></mrow></math></th>
          <th>alloc<br>(bytes)</th>
          <th>&Delta;<br></th>
          <th><math><mrow><mfrac><mi>prev</mi><mi>curr</mi></mfrac></mrow></math></th>
          <th>promo<br>(bytes)</th>
          <th>&Delta;<br></th>
          <th><math><mrow><mfrac><mi>prev</mi><mi>curr</mi></mfrac></mrow></math></th>
          <th>peak<br>(bytes)</th>
          <th>&Delta;<br></th>
          <th><math><mrow><mfrac><mi>prev</mi><mi>curr</mi></mfrac></mrow></math></th>
          <th>Status</th>
          <th>&Delta;<br></th>
          <th>Steps</th>
          <th>&Delta;<br></th>
          <th>Cover</th>
          <th>%%</th>
          <th>&Delta;<br></th>
          <th><math><mrow><mfrac><mi>prev</mi><mi>curr</mi></mfrac></mrow></math></th>
          <th>Solver<br>Steps</th>
          <th>&Delta;<br></th>
        </tr>|} ;
  pf "\n" ;
  Iter.iter rows ~f:(fun row ->
      let { name
          ; times
          ; times_deltas
          ; gcs
          ; gcs_deltas
          ; cov
          ; cov_deltas
          ; status
          ; status_deltas } =
        row
      in
      let max_name_length = 50 in
      let name =
        if String.length name <= max_name_length then name
        else
          let len = max_name_length / 2 in
          String.take len name ^ "…" ^ String.rtake len name
      in
      let time ppf t =
        Printf.fprintf ppf
          "<td style=\"border-left: 2px solid #eee8d5\"; \
           align=\"right\">%12.3f</td>\n"
          t
      in
      let mem ppf w =
        Printf.fprintf ppf
          "<td style=\"border-left: 2px solid #eee8d5\"; \
           align=\"right\">%s</td>\n"
          Core_kernel.Byte_units.(to_string_short (of_megabytes w))
      in
      let nondelta ppf t =
        Printf.fprintf ppf "<td align=\"right\">%12.3f</td>\n" t
      in
      let nondelta_mem ppf w =
        Printf.fprintf ppf "<td align=\"right\">%s</td>\n"
          Core_kernel.Byte_units.(to_string_short (of_megabytes w))
      in
      let delta max pct t ppf d =
        let r = 100. *. d /. t in
        let x = (t -. d) /. t in
        Printf.fprintf ppf
          "<td align=\"right\" bgcolor=\"%s\">%12.3f</td>\n\
           <td align=\"right\" bgcolor=\"%s\">%12.2fx</td>\n"
          (color max d) d (color pct r)
          (Base.Float.round_decimal ~decimal_digits:2 x)
      in
      let delta_mem max pct w ppf d =
        let r = if Float.(abs d < 0.000001) then 0. else 100. *. d /. w in
        let x = (w -. d) /. w in
        Printf.fprintf ppf
          "<td align=\"right\" bgcolor=\"%s\">%s</td>\n\
           <td align=\"right\" bgcolor=\"%s\">%12.2fx</td>\n"
          (color max d)
          Core_kernel.Byte_units.(to_string_short (of_megabytes d))
          (color pct r)
          (Base.Float.round_decimal ~decimal_digits:2 x)
      in
      let timed = delta ranges.max_time ranges.pct_time in
      let allocd = delta_mem ranges.max_alloc ranges.pct_alloc in
      let promod = delta_mem ranges.max_promo ranges.pct_promo in
      let peakd = delta_mem ranges.max_peak ranges.pct_peak in
      let pf_status ppf s =
        let status_to_string = Format.asprintf "%a" Report.pp_status in
        Printf.fprintf ppf "%s" (String.take 50 (status_to_string s))
      in
      let steps attr ppf = function
        | [] -> Printf.fprintf ppf "<td %s></td>\n" attr
        | cs ->
            List.iter cs ~f:(fun {Report.steps} ->
                if steps = 0 then Printf.fprintf ppf "<td></td>\n"
                else
                  Printf.fprintf ppf "<td %s align=\"right\">%i</td>\n" attr
                    steps )
      in
      let solver_steps attr ppf = function
        | [] -> Printf.fprintf ppf "<td %s></td>\n" attr
        | cs ->
            List.iter cs ~f:(fun {Report.solver_steps} ->
                if solver_steps = 0 then Printf.fprintf ppf "<td></td>\n"
                else
                  Printf.fprintf ppf "<td %s align=\"right\">%i</td>\n" attr
                    solver_steps )
      in
      let hit attr ppf = function
        | [] -> Printf.fprintf ppf "<td %s></td>\n" attr
        | cs ->
            List.iter cs ~f:(fun {Report.hit} ->
                if hit = 0 then Printf.fprintf ppf "<td></td>\n"
                else
                  Printf.fprintf ppf "<td %s align=\"right\">%i</td>\n" attr
                    hit )
      in
      let coverage attr ppf = function
        | [] -> Printf.fprintf ppf "<td align=\"right\"></td>\n"
        | cs ->
            List.iter cs ~f:(fun {Report.fraction} ->
                if Float.(abs fraction < 0.000001) then
                  Printf.fprintf ppf "<td></td>\n"
                else
                  Printf.fprintf ppf
                    "<td %s align=\"right\">%12.0f%%</td>\n" attr
                    (Base.Float.round_decimal ~decimal_digits:2
                       (100. *. fraction)) )
      in
      let coveraged coverage ppf cs =
        let cs = Option.value cs ~default:[] in
        let attr = if List.is_empty cs then "" else " class=\"neutral\"" in
        Printf.fprintf ppf "%a" (coverage attr) cs
      in
      let stat ppf ss =
        Printf.fprintf ppf "<td style=\"border-left: 2px solid #eee8d5\";" ;
        ( match ss with
        | [] -> Printf.fprintf ppf ">"
        | ss ->
            List.iter ss ~f:(fun s ->
                match (s : Report.status) with
                | Safe _ | Unsafe _ | Ok ->
                    Printf.fprintf ppf ">%a" pf_status s
                | _ -> Printf.fprintf ppf "class=\"regress\">%a" pf_status s )
        ) ;
        Printf.fprintf ppf "</td>\n"
      in
      let statd ppf = function
        | None | Some [] -> Printf.fprintf ppf "<td></td>\n"
        | Some ss ->
            Printf.fprintf ppf "<td class=\"neutral\">" ;
            List.iter ss ~f:(fun s -> Printf.fprintf ppf "%a" pf_status s) ;
            Printf.fprintf ppf "</td>\n"
      in
      pf "<tr>\n" ;
      pf "<td>%s</td>" name ;
      ( match (times, times_deltas) with
      | ( Some {etime; utime; stime}
        , Some {etime= etime_delta; utime= utime_delta; stime= stime_delta}
        ) ->
          pf "%a%a%a%a%a%a" time etime (timed etime) etime_delta time utime
            (timed utime) utime_delta time stime (timed stime) stime_delta
      | Some {etime; utime; stime}, None ->
          pf
            "%a<td></td><td></td>\n\
             %a<td></td><td></td>\n\
             %a<td></td><td></td>\n"
            time etime time utime time stime
      | None, Some {etime; utime; stime} ->
          pf
            "<td style=\"border-left: 2px solid #eee8d5\";></td>%a<td></td>\n\
             <td style=\"border-left: 2px solid #eee8d5\";></td>%a<td></td>\n\
             <td style=\"border-left: 2px solid #eee8d5\";></td>%a<td></td>\n"
            nondelta etime nondelta utime nondelta stime
      | None, None ->
          pf
            "<td style=\"border-left: 2px solid \
             #eee8d5\";></td><td></td><td></td>\n\
             <td style=\"border-left: 2px solid \
             #eee8d5\";></td><td></td><td></td>\n\
             <td style=\"border-left: 2px solid \
             #eee8d5\";></td><td></td><td></td>\n" ) ;
      ( match (gcs, gcs_deltas) with
      | ( Some {Report.allocated; promoted; peak_size}
        , Some
            { allocated= allocated_delta
            ; promoted= promoted_delta
            ; peak_size= peak_size_delta } ) ->
          pf "%a%a%a%a%a%a" mem allocated (allocd allocated) allocated_delta
            mem promoted (promod promoted) promoted_delta mem peak_size
            (peakd peak_size) peak_size_delta
      | Some {allocated; promoted; peak_size}, None ->
          pf
            "%a<td></td><td></td>\n\
             %a<td></td><td></td>\n\
             %a<td></td><td></td>\n"
            mem allocated mem promoted mem peak_size
      | None, Some {allocated; promoted; peak_size} ->
          pf
            "<td style=\"border-left: 2px solid #eee8d5\";></td>%a<td></td>\n\
             <td style=\"border-left: 2px solid #eee8d5\";></td>%a<td></td>\n\
             <td style=\"border-left: 2px solid #eee8d5\";></td>%a<td></td>\n"
            nondelta_mem allocated nondelta_mem promoted nondelta_mem
            peak_size
      | None, None ->
          pf
            "<td style=\"border-left: 2px solid \
             #eee8d5\";></td><td></td><td></td>\n\
             <td style=\"border-left: 2px solid \
             #eee8d5\";></td><td></td><td></td>\n\
             <td style=\"border-left: 2px solid \
             #eee8d5\";></td><td></td><td></td>\n" ) ;
      pf "%a%a" stat status statd status_deltas ;
      pf "%a%a"
        (steps " style=\"border-left: 2px solid #eee8d5\";")
        cov (coveraged steps) cov_deltas ;
      pf "%a%a"
        (hit " style=\"border-left: 2px solid #eee8d5\";")
        cov (coverage "") cov ;
      pf "%a%a" (coveraged hit) cov_deltas (coveraged coverage) cov_deltas ;
      pf "%a%a"
        (solver_steps " style=\"border-left: 2px solid #eee8d5\";")
        cov (coveraged solver_steps) cov_deltas ;
      pf "</tr>\n" ) ;
  pf "<table>\n" ;
  pf "</body></html>\n"

let average row =
  let ave_times times =
    if List.is_empty times then None
    else
      let etimes, utimes, stimes =
        List.fold times (Iter.empty, Iter.empty, Iter.empty)
          ~f:(fun {etime; utime; stime} (etimes, utimes, stimes) ->
            ( Iter.cons etime etimes
            , Iter.cons utime utimes
            , Iter.cons stime stimes ) )
      in
      Some
        { etime= ave_floats etimes
        ; utime= ave_floats utimes
        ; stime= ave_floats stimes }
  in
  let times = ave_times row.times in
  let times_deltas = ave_times row.times_deltas in
  let ave_gcs gcs =
    if List.is_empty gcs then None
    else
      let alloc, promo, peak =
        List.fold gcs (Iter.empty, Iter.empty, Iter.empty)
          ~f:(fun {Report.allocated; promoted; peak_size}
             (alloc, promo, peak)
             ->
            ( Iter.cons allocated alloc
            , Iter.cons promoted promo
            , Iter.cons peak_size peak ) )
      in
      Some
        Report.
          { allocated= ave_floats alloc
          ; promoted= ave_floats promo
          ; peak_size= ave_floats peak }
  in
  let gcs = ave_gcs row.gcs in
  let gcs_deltas = ave_gcs row.gcs_deltas in
  {row with times; times_deltas; gcs; gcs_deltas}

let add_total rows =
  let init =
    { name= "TOTAL"
    ; times= Some {etime= 0.; utime= 0.; stime= 0.}
    ; times_deltas= Some {etime= 0.; utime= 0.; stime= 0.}
    ; gcs= Some {Report.allocated= 0.; promoted= 0.; peak_size= 0.}
    ; gcs_deltas= Some {Report.allocated= 0.; promoted= 0.; peak_size= 0.}
    ; cov= []
    ; cov_deltas= None
    ; status= []
    ; status_deltas= None }
  in
  let total =
    Iter.fold rows init ~f:(fun row total ->
        let times =
          match (total.times, row.times) with
          | Some total_times, Some row_times ->
              Some
                { etime= total_times.etime +. row_times.etime
                ; utime= total_times.utime +. row_times.utime
                ; stime= total_times.stime +. row_times.stime }
          | _ -> total.times
        in
        let times_deltas =
          match (total.times_deltas, row.times_deltas) with
          | Some total_deltas, Some row_deltas ->
              Some
                { etime= total_deltas.etime +. row_deltas.etime
                ; utime= total_deltas.utime +. row_deltas.utime
                ; stime= total_deltas.stime +. row_deltas.stime }
          | _ -> total.times_deltas
        in
        let gcs =
          match (total.gcs, row.gcs) with
          | Some total_gcs, Some row_gcs ->
              Some
                Report.
                  { allocated= total_gcs.allocated +. row_gcs.allocated
                  ; promoted= total_gcs.promoted +. row_gcs.promoted
                  ; peak_size= total_gcs.peak_size +. row_gcs.peak_size }
          | _ -> total.gcs
        in
        let gcs_deltas =
          match (total.gcs_deltas, row.gcs_deltas) with
          | Some total_deltas, Some row_deltas ->
              Some
                Report.
                  { allocated= total_deltas.allocated +. row_deltas.allocated
                  ; promoted= total_deltas.promoted +. row_deltas.promoted
                  ; peak_size=
                      total_deltas.peak_size +. row_deltas.peak_size }
          | _ -> total.gcs_deltas
        in
        let status_deltas =
          if
            Option.is_some total.status_deltas
            || Option.is_some row.status_deltas
          then Some []
          else None
        in
        {total with times; times_deltas; gcs; gcs_deltas; status_deltas} )
  in
  Iter.cons total rows

let cmp perf x y =
  match (x.status_deltas, y.status_deltas) with
  | Some xs, Some ys ->
      List.compare Report.compare_status xs ys
      |> fun o -> if o <> 0 then o else String.compare x.name y.name
  | Some _, None -> -1
  | None, Some _ -> 1
  | None, None -> (
      let max =
        Option.map_or ~default:0 ~f:(fun cds ->
            List.fold cds 0 ~f:(fun {Report.steps} m ->
                Int.(max (abs steps) m) ) )
      in
      -Int.compare (max x.cov_deltas) (max y.cov_deltas)
      |> fun o ->
      if o <> 0 then o
      else
        match (List.hd x.status, List.hd y.status) with
        | ( Some (Safe _ | Unsafe _ | Ok | Unsound | Incomplete)
          , Some (Safe _ | Unsafe _ | Ok | Unsound | Incomplete) )
          when perf -> (
          match (x.times_deltas, y.times_deltas) with
          | Some xtd, Some ytd ->
              -Float.(compare (abs xtd.utime) (abs ytd.utime))
              |> fun o -> if o <> 0 then o else String.compare x.name y.name
          | Some _, None -> 1
          | None, Some _ -> -1
          | None, None -> String.compare x.name y.name )
        | ( Some (Safe _ | Unsafe _ | Ok | Unsound | Incomplete)
          , Some (Safe _ | Unsafe _ | Ok | Unsound | Incomplete) ) -> (
          match (x.gcs_deltas, y.gcs_deltas) with
          | Some xgc, Some ygc ->
              -Float.(
                 compare
                   (abs xgc.Report.allocated)
                   (abs ygc.Report.allocated))
              |> fun o -> if o <> 0 then o else String.compare x.name y.name
          | Some _, None -> 1
          | None, Some _ -> -1
          | None, None -> String.compare x.name y.name )
        | _, Some (Safe _ | Unsafe _ | Ok | Unsound | Incomplete) -> -1
        | Some (Safe _ | Unsafe _ | Ok | Unsound | Incomplete), _ -> 1
        | s, t ->
            Option.compare (Ord.opp Report.compare_status) s t
            |> fun o -> if o <> 0 then o else String.compare x.name y.name )

let filter rows =
  Iter.filter rows ~f:(fun {status} ->
      List.exists status ~f:(function
        | InvalidInput _ | Unimplemented _ -> false
        | _ -> true ) )

let input_rows ?baseline current =
  let b_tbl = Option.map ~f:read baseline in
  let c_tbl = read current in
  let names =
    let keys = Tbl.keys c_tbl in
    let keys =
      Option.fold ~f:(fun t -> Iter.append (Tbl.keys t)) b_tbl keys
    in
    Iter.sort_uniq ~cmp:String.compare keys
  in
  Iter.map names ~f:(fun name ->
      let opt_find_opt t n = Option.bind ~f:(fun t -> Tbl.find_opt t n) t in
      let b_result = opt_find_opt b_tbl name in
      let c_result = Tbl.find_opt c_tbl name in
      combine name b_result c_result )

let generate_html perf ?baseline current output =
  let rows = input_rows ?baseline current in
  let rows = Iter.map ~f:average rows in
  let rows = filter rows in
  let rows = Iter.persistent rows in
  let ranges = ranges rows in
  let rows = Iter.sort ~cmp:(cmp perf) rows in
  let rows = add_total rows in
  Out_channel.with_file output ~f:(write_html ranges rows)

let html_cmd =
  let open Command.Let_syntax in
  let%map_open baseline =
    flag "baseline" (optional string)
      ~doc:"<file> read baseline results from report <file>"
  and current = anon ("<file>" %: string)
  and output =
    flag "output" (required string)
      ~doc:"<file> write html report to <file>"
  and perf =
    flag "perf" no_arg ~doc:"sort results for a performance comparison"
  in
  fun () -> generate_html perf ?baseline current output

let write_status ?baseline rows chan =
  let rows =
    if Option.is_none baseline then rows
    else Iter.filter rows ~f:(fun row -> Option.is_some row.status_deltas)
  in
  let rows =
    Iter.sort ~cmp:(fun x y -> String.compare x.name y.name) rows
  in
  let ppf = Format.str_formatter in
  Iter.iter rows ~f:(fun {name; status; status_deltas} ->
      Format.fprintf ppf "%s:\t%a%a@\n" name
        (List.pp ", " Report.pp_status)
        status
        (Option.pp "\t%a" (List.pp ", " Report.pp_status))
        status_deltas ) ;
  Out_channel.output_string chan (Format.flush_str_formatter ())

let generate_status ?baseline current output =
  let rows = input_rows ?baseline current in
  match output with
  | None -> write_status ?baseline rows Out_channel.stdout
  | Some output ->
      Out_channel.with_file output ~f:(write_status ?baseline rows)

let status_cmd =
  let open Command.Let_syntax in
  let%map_open baseline =
    flag "baseline" (optional string)
      ~doc:"<file> read baseline results from report <file>"
  and current = anon ("<file>" %: string)
  and output =
    flag "output" (optional string)
      ~doc:
        "<file> write status report to <file>, or to standard output if \
         omitted"
  in
  fun () -> generate_status ?baseline current output

;;
Command.run
  (Command.group ~summary:"SLEdge report manipulation"
     [ ("html", Command.basic ~summary:"generate html report" html_cmd)
     ; ("status", Command.basic ~summary:"generate status report" status_cmd)
     ])
