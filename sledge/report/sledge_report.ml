(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Tbl = CCHashtbl.Make (String)

let read filename =
  let tbl = Tbl.create 64 in
  List.iter (Sexp.load_sexps filename) ~f:(fun sexp ->
      let {Report.name; entry} = Report.t_of_sexp sexp in
      match (Tbl.find_opt tbl name, entry) with
      | None, ProcessTimes (etime, ptimes) ->
          Tbl.replace tbl name ([(etime, ptimes)], [], [])
      | None, GcStats gc -> Tbl.replace tbl name ([], [gc], [])
      | None, Status status -> Tbl.replace tbl name ([], [], [status])
      | Some (times, gcs, statuses), ProcessTimes (etime, ptimes) ->
          Tbl.replace tbl name ((etime, ptimes) :: times, gcs, statuses)
      | Some (times, gcs, statuses), GcStats gc ->
          Tbl.replace tbl name (times, gc :: gcs, statuses)
      | Some (times, gc, statuses), Status status ->
          Tbl.replace tbl name (times, gc, status :: statuses) ) ;
  tbl

type times = {etime: float; utime: float; stime: float}

type ('t, 'g) row =
  { name: string
  ; times: 't
  ; times_deltas: 't
  ; gcs: 'g
  ; gcs_deltas: 'g
  ; status: Report.status list
  ; status_deltas: Report.status list option }

let times_of_raw (etime, ptimes) =
  let {Unix.tms_utime; tms_cutime; tms_stime; tms_cstime} = ptimes in
  let utime = tms_utime +. tms_cutime in
  let stime = tms_stime +. tms_cstime in
  let etime = etime in
  {etime; utime; stime}

let add_time base_times row ptimes =
  let tustimes = times_of_raw ptimes in
  let times = tustimes :: row.times in
  let times_deltas =
    Option.fold base_times ~init:row.times_deltas
      ~f:(fun times_deltas {etime= btt; utime= but; stime= bst} ->
        let {etime= tt; utime= ut; stime= st} = tustimes in
        {etime= tt -. btt; utime= ut -. but; stime= st -. bst}
        :: times_deltas )
  in
  {row with times; times_deltas}

let add_times base_times times row =
  if List.is_empty times then
    {row with times_deltas= Option.to_list base_times}
  else List.fold ~f:(add_time base_times) ~init:row times

let add_gc base_gcs row gc =
  let gcs = gc :: row.gcs in
  let gcs_deltas =
    Option.fold base_gcs ~init:row.gcs_deltas ~f:(fun gcs_deltas bgc ->
        Report.
          { allocated= gc.allocated -. bgc.allocated
          ; promoted= gc.promoted -. bgc.promoted
          ; peak_size= gc.peak_size -. bgc.peak_size }
        :: gcs_deltas )
  in
  {row with gcs; gcs_deltas}

let add_gcs base_gcs gcs row =
  if List.is_empty gcs then {row with gcs_deltas= Option.to_list base_gcs}
  else List.fold ~f:(add_gc base_gcs) ~init:row gcs

let add_status base_status row status =
  if List.mem ~equal:Report.equal_status row.status status then row
  else
    match base_status with
    | Some base_status
      when not (List.mem ~equal:Report.equal_status base_status status) ->
        { row with
          status= status :: row.status
        ; status_deltas=
            Some (base_status @ Option.value row.status_deltas ~default:[])
        }
    | _ -> {row with status= status :: row.status}

let add_statuses base_status statuses row =
  List.fold ~f:(add_status base_status) ~init:row statuses

let ave_floats flts =
  assert (not (Iter.is_empty flts)) ;
  let min, max, sum, num =
    Iter.fold flts ~init:(Float.infinity, Float.neg_infinity, 0., 0)
      ~f:(fun (min, max, sum, num) flt ->
        (Float.min min flt, Float.max max flt, sum +. flt, num + 1) )
  in
  if num >= 5 then (sum -. min -. max) /. Float.of_int (num - 2)
  else sum /. Float.of_int num

let combine name b_result c_result =
  let base_times, base_gcs, base_status =
    match b_result with
    | Some (times, gcs, statuses) ->
        let times =
          if List.is_empty times then None
          else
            let etimes, utimes, stimes, cutimes, cstimes =
              List.fold times
                ~init:
                  ( Iter.empty
                  , Iter.empty
                  , Iter.empty
                  , Iter.empty
                  , Iter.empty )
                ~f:(fun (etimes, utimes, stimes, cutimes, cstimes)
                   ( etime
                   , {Unix.tms_utime; tms_cutime; tms_stime; tms_cstime} )
                   ->
                  ( Iter.cons etime etimes
                  , Iter.cons tms_utime utimes
                  , Iter.cons tms_stime stimes
                  , Iter.cons tms_cutime cutimes
                  , Iter.cons tms_cstime cstimes ) )
            in
            Some
              (times_of_raw
                 ( ave_floats etimes
                 , { tms_utime= ave_floats utimes
                   ; tms_stime= ave_floats stimes
                   ; tms_cutime= ave_floats cutimes
                   ; tms_cstime= ave_floats cstimes } ))
        in
        let gcs =
          if List.is_empty gcs then None
          else
            let allocs, promos, peaks =
              List.fold gcs ~init:(Iter.empty, Iter.empty, Iter.empty)
                ~f:(fun (allocs, promos, peaks)
                   {Report.allocated; promoted; peak_size}
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
        let status =
          Some (List.dedup_and_sort ~compare:Report.compare_status statuses)
        in
        (times, gcs, status)
    | None -> (None, None, None)
  in
  let row =
    { name
    ; times= []
    ; times_deltas= []
    ; gcs= []
    ; gcs_deltas= []
    ; status= []
    ; status_deltas= None }
  in
  match c_result with
  | None ->
      let times_deltas = Option.to_list base_times in
      let gcs_deltas = Option.to_list base_gcs in
      let status_deltas = base_status in
      {row with times_deltas; gcs_deltas; status_deltas}
  | Some (c_times, c_gcs, c_statuses) ->
      row
      |> add_times base_times c_times
      |> add_gcs base_gcs c_gcs
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
  Iter.fold rows ~init ~f:(fun acc {times; times_deltas; gcs; gcs_deltas} ->
      Option.fold times_deltas ~init:acc ~f:(fun acc deltas ->
          let max_time = Float.max acc.max_time (Float.abs deltas.etime) in
          let pct_time =
            Option.fold times ~init:acc.pct_time ~f:(fun pct_time times ->
                let pct = 100. *. deltas.etime /. times.etime in
                Float.max pct_time (Float.abs pct) )
          in
          {acc with max_time; pct_time} )
      |> fun init ->
      Option.fold gcs_deltas ~init ~f:(fun acc deltas ->
          let max_alloc = Float.max acc.max_alloc deltas.Report.allocated in
          let pct_alloc =
            Option.fold gcs ~init:acc.pct_alloc ~f:(fun pct_alloc gcs ->
                let pct =
                  100. *. deltas.Report.allocated /. gcs.Report.allocated
                in
                Float.max pct_alloc (Float.abs pct) )
          in
          let max_promo = Float.max acc.max_promo deltas.Report.promoted in
          let pct_promo =
            Option.fold gcs ~init:acc.pct_promo ~f:(fun pct_promo gcs ->
                let pct =
                  100. *. deltas.Report.promoted /. gcs.Report.promoted
                in
                Float.max pct_promo (Float.abs pct) )
          in
          let max_peak = Float.max acc.max_peak deltas.Report.peak_size in
          let pct_peak =
            Option.fold gcs ~init:acc.pct_peak ~f:(fun pct_peak gcs ->
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
    if Float.is_negative x then scale (-.x) lace green else scale x lace red
  in
  let rgb_to_hex (r, g, b) =
    let to_int x = Int.min 255 (Int.max 0 (Float.to_int x)) in
    Printf.sprintf "#%2x%2x%2x" (to_int r) (to_int g) (to_int b)
  in
  let rat = dat /. max in
  if Float.is_nan rat then rgb_to_hex lace else rgb_to_hex (gradient rat)

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
    {|<table style="border-collapse: collapse"><tr>
          <th>Test</th>
          <th>elapsed<br>(sec)</th>
          <th>&Delta;<br></th>
          <th>&Delta;%%<br></th>
          <th>user<br>(sec)</th>
          <th>&Delta;<br></th>
          <th>&Delta;%%<br></th>
          <th>system<br>(sec)</th>
          <th>&Delta;<br></th>
          <th>&Delta;%%<br></th>
          <th>alloc<br>(MB)</th>
          <th>&Delta;<br></th>
          <th>&Delta;%%<br></th>
          <th>promo<br>(MB)</th>
          <th>&Delta;<br></th>
          <th>&Delta;%%<br></th>
          <th>peak<br>(MB)</th>
          <th>&Delta;<br></th>
          <th>&Delta;%%<br></th>
          <th>Status</th>
          <th>&Delta;<br></th>
        </tr>
    |} ;
  Iter.iter rows ~f:(fun row ->
      let {name; times; times_deltas; gcs; gcs_deltas; status; status_deltas}
          =
        row
      in
      let max_name_length = 50 in
      let name =
        if String.length name <= max_name_length then name
        else
          String.prefix name (max_name_length / 2)
          ^ "…"
          ^ String.suffix name (max_name_length / 2)
      in
      let time ppf t =
        Printf.fprintf ppf
          "<td style=\"border-left: 2px solid #eee8d5\"; \
           align=\"right\">%12.3f</td>\n"
          t
      in
      let nondelta ppf t =
        Printf.fprintf ppf "<td align=\"right\">%12.3f</td>\n" t
      in
      let delta max pct t ppf d =
        let r = 100. *. d /. t in
        Printf.fprintf ppf
          "<td align=\"right\" bgcolor=\"%s\">%12.3f</td>\n\
           <td align=\"right\" bgcolor=\"%s\">%12.2f%%</td>\n"
          (color max d) d (color pct r) r
      in
      let timed = delta ranges.max_time ranges.pct_time in
      let allocd = delta ranges.max_alloc ranges.pct_alloc in
      let promod = delta ranges.max_promo ranges.pct_promo in
      let peakd = delta ranges.max_peak ranges.pct_peak in
      let pf_status ppf s =
        let status_to_string = Format.asprintf "%a" Report.pp_status in
        Printf.fprintf ppf "%s" (String.prefix (status_to_string s) 50)
      in
      let stat ppf = function
        | [] ->
            Printf.fprintf ppf
              "<td style=\"border-left: 2px solid #eee8d5\";></td>\n"
        | ss ->
            List.iter ss ~f:(fun s ->
                match (s : Report.status) with
                | Safe | Unsafe _ | Ok ->
                    Printf.fprintf ppf
                      "<td style=\"border-left: 2px solid #eee8d5\";>%a</td>\n"
                      pf_status s
                | _ ->
                    Printf.fprintf ppf
                      "<td style=\"border-left: 2px solid #eee8d5\"; \
                       class=\"regress\">%a</td>\n"
                      pf_status s )
      in
      let statd ppf = function
        | None | Some [] -> Printf.fprintf ppf "<td></td>\n"
        | Some ss ->
            Printf.fprintf ppf "<td class=\"neutral\">" ;
            List.iter ss ~f:(fun s -> Printf.fprintf ppf "%a" pf_status s) ;
            Printf.fprintf ppf "</td>\n"
      in
      pf "<tr>" ;
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
          pf "%a%a%a%a%a%a" time allocated (allocd allocated)
            allocated_delta time promoted (promod promoted) promoted_delta
            time peak_size (peakd peak_size) peak_size_delta
      | Some {allocated; promoted; peak_size}, None ->
          pf
            "%a<td></td><td></td>\n\
             %a<td></td><td></td>\n\
             %a<td></td><td></td>\n"
            time allocated time promoted time peak_size
      | None, Some {allocated; promoted; peak_size} ->
          pf
            "<td style=\"border-left: 2px solid #eee8d5\";></td>%a<td></td>\n\
             <td style=\"border-left: 2px solid #eee8d5\";></td>%a<td></td>\n\
             <td style=\"border-left: 2px solid #eee8d5\";></td>%a<td></td>\n"
            nondelta allocated nondelta promoted nondelta peak_size
      | None, None ->
          pf
            "<td style=\"border-left: 2px solid \
             #eee8d5\";></td><td></td><td></td>\n\
             <td style=\"border-left: 2px solid \
             #eee8d5\";></td><td></td><td></td>\n\
             <td style=\"border-left: 2px solid \
             #eee8d5\";></td><td></td><td></td>\n" ) ;
      pf "%a%a" stat status statd status_deltas ;
      pf "</tr>\n" ) ;
  pf "<table>\n" ;
  pf "</body></html>\n"

let average row =
  let ave_times times =
    if List.is_empty times then None
    else
      let etimes, utimes, stimes =
        List.fold times ~init:(Iter.empty, Iter.empty, Iter.empty)
          ~f:(fun (etimes, utimes, stimes) {etime; utime; stime} ->
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
        List.fold gcs ~init:(Iter.empty, Iter.empty, Iter.empty)
          ~f:(fun (alloc, promo, peak)
             {Report.allocated; promoted; peak_size}
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
    ; status= []
    ; status_deltas= None }
  in
  let total =
    Iter.fold rows ~init ~f:(fun total row ->
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

let cmp x y =
  match (x.status_deltas, y.status_deltas) with
  | Some xs, Some ys ->
      List.compare Report.compare_status xs ys
      |> fun o -> if o <> 0 then o else String.compare x.name y.name
  | Some _, None -> -1
  | None, Some _ -> 1
  | None, None -> (
    match (x.times_deltas, y.times_deltas) with
    | Some xtd, Some ytd -> Float.(compare (abs ytd.utime) (abs xtd.utime))
    | Some _, None -> 1
    | None, Some _ -> -1
    | None, None -> String.compare x.name y.name )

let filter rows =
  Iter.filter rows ~f:(fun {status} ->
      List.exists status ~f:(function Unimplemented _ -> false | _ -> true) )

let input_rows ?baseline current =
  let b_tbl = Option.map ~f:read baseline in
  let c_tbl = read current in
  let names =
    let keys = Tbl.keys c_tbl in
    let keys =
      Option.fold
        ~f:(fun i t -> Iter.append (Tbl.keys t) i)
        ~init:keys b_tbl
    in
    Iter.sort_uniq ~cmp:String.compare keys
  in
  Iter.map names ~f:(fun name ->
      let opt_find_opt t n = Option.bind ~f:(fun t -> Tbl.find_opt t n) t in
      let b_result = opt_find_opt b_tbl name in
      let c_result = Tbl.find_opt c_tbl name in
      combine name b_result c_result )

let generate_html ?baseline current output =
  let rows = input_rows ?baseline current in
  let rows = Iter.map ~f:average rows in
  let ranges = ranges rows in
  let rows = filter rows in
  let rows = add_total rows in
  let rows = Iter.sort ~cmp rows in
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
  in
  fun () -> generate_html ?baseline current output

let write_status rows chan =
  let rows =
    Iter.filter rows ~f:(fun row -> Option.is_some row.status_deltas)
  in
  let rows =
    Iter.sort ~cmp:(fun x y -> String.compare x.name y.name) rows
  in
  let ppf = Format.formatter_of_out_channel chan in
  Iter.iter rows ~f:(fun {name; status; status_deltas} ->
      Format.fprintf ppf "%s:\t%a%a@\n" name
        (List.pp ", " Report.pp_status)
        status
        (Option.pp "\t%a" (List.pp ", " Report.pp_status))
        status_deltas )

let generate_status ?baseline current output =
  let rows = input_rows ?baseline current in
  match output with
  | None -> write_status rows Out_channel.stdout
  | Some output -> Out_channel.with_file output ~f:(write_status rows)

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
