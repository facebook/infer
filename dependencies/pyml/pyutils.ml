let option_find f x =
  try Some (f x)
  with Not_found -> None

let substring_between string before after =
  String.sub string before (after - before)

let int_of_octal octal =
  int_of_string ("0o" ^ octal)

let int_of_hex hex =
  int_of_string ("0x" ^ hex)

let split_left_on_char ?(from=0) char s =
  try substring_between s from (String.index_from s from char)
  with Not_found ->
    if from = 0 then s
    else substring_between s from (String.length s)

let split_right_on_char ?(from=0) char s =
  try
    substring_between s (String.index_from s from char + 1)
      (String.length s)
  with Not_found ->
    if from = 0 then s
    else substring_between s from (String.length s)

let trim_carriage_return line =
  let length = String.length line in
  if String.sub line (length - 1) 1 = "\r" then
    String.sub line 0 (length - 1)
  else
    line

let input_lines channel =
  let accu = ref [] in
  try
    while true do
      accu := trim_carriage_return (input_line channel) :: !accu;
    done;
    assert false
  with End_of_file ->
    List.rev !accu

let write_and_close channel f arg =
  try
    let result = f arg in
    close_out channel;
    result
  with e ->
    close_out_noerr channel;
    raise e

let with_temp_file contents f =
  let (file, channel) = Filename.open_temp_file "pyml_tests" ".py" in
  Fun.protect begin fun () ->
    write_and_close channel (output_string channel) contents;
    In_channel.with_open_bin file (f file)
  end
  ~finally:(fun () -> Sys.remove file)

let with_pipe f =
  let (read, write) = Unix.pipe () in
  let in_channel = Unix.in_channel_of_descr read
  and out_channel = Unix.out_channel_of_descr write in
  Fun.protect begin fun () ->
    f in_channel out_channel
  end
  ~finally:begin fun () ->
    close_in_noerr in_channel;
    close_out_noerr out_channel
  end

let with_stdin_from channel f arg =
  let stdin_backup = Unix.dup Unix.stdin in
  Unix.dup2 (Unix.descr_of_in_channel channel) Unix.stdin;
  Fun.protect begin fun () ->
    f arg
  end
  ~finally:begin fun () ->
    Unix.dup2 stdin_backup Unix.stdin
  end

let with_channel_from_string s f =
  with_pipe begin fun in_channel out_channel ->
    output_string out_channel s;
    close_out out_channel;
    f in_channel
  end

let with_stdin_from_string s f arg =
  with_channel_from_string s (fun channel -> with_stdin_from channel f arg)
