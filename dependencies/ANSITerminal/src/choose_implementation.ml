(* Script to choose the unix or windows implementation depending on
   the platform *)

open Printf

let copy_file ?(line_directive=false) ?(dir=".") source target =
  let fh0 = open_in (Filename.concat dir source) in
  let fh1 = open_out_bin (Filename.concat dir target) in
  if line_directive then
    fprintf fh1 "#1 \"%s\"\n" (Filename.concat dir source);
  let b = Bytes.create 4096 in
  let n = ref 0 in
  while n := input fh0 b 0 4096;  !n > 0 do
    output fh1 b 0 !n
  done;
  close_in fh0;
  close_out fh1


let choose_unix () =
  copy_file "ANSITerminal_unix.ml" "ANSITerminal.ml" ~line_directive:true;
  copy_file "ANSITerminal_unix_stubs.c" "ANSITerminal_stubs.c"

let choose_win () =
  copy_file "ANSITerminal_win.ml" "ANSITerminal.ml" ~line_directive:true;
  copy_file "ANSITerminal_win_stubs.c" "ANSITerminal_stubs.c"

let () =
  match Sys.os_type with
  | "Unix" | "Cygwin" -> choose_unix ()
  | "Win32" -> choose_win()
  | e -> eprintf "Unknown OS type %S.\n" e
