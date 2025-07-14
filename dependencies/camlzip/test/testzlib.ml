let compress infile outfile =
  let ic = open_in_bin infile
  and oc = open_out_bin outfile in
  Zlib.compress (fun buf -> input ic buf 0 (Bytes.length buf))
                (fun buf len -> output oc buf 0 len);
  close_in ic;
  close_out oc

let uncompress infile outfile =
  let ic = open_in_bin infile
  and oc = open_out_bin outfile in
  Zlib.uncompress (fun buf -> input ic buf 0 (Bytes.length buf))
                  (fun buf len -> output oc buf 0 len);
  close_in ic;
  close_out oc

let _ =
  if Array.length Sys.argv >= 4 && Sys.argv.(1) = "-d" then
    uncompress Sys.argv.(2) Sys.argv.(3)
  else if Array.length Sys.argv >= 3 then
    compress Sys.argv.(1) Sys.argv.(2)
