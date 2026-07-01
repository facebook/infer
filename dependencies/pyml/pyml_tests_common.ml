let use_version = ref (None, None)

type status =
  | Passed
  | Failed of string
  | Disabled of string

let tests = Queue.create ()

let add_test ~title f =
  Queue.add (title, f) tests

let failed = ref false

let launch_test (title, f) =
  Printf.printf "Test '%s' ... %!" title;
  try
    match f () with
      Passed -> Printf.printf "passed\n%!"
    | Failed reason ->
       Printf.printf "failed: %s\n%!" reason;
       failed := true
    | Disabled reason -> Printf.printf "disabled: %s\n%!" reason
  with
    Py.E (_, value) ->
    Printf.printf
      "raised a Python exception: %s\n%!"
      (Py.Object.to_string value);
    failed := true
  | e ->
     Printf.printf "raised an exception: %s\n%!" (Printexc.to_string e);
     failed := true

let rec launch_tests () =
  match
    try Some (Queue.pop tests)
    with Queue.Empty -> None
  with
    None -> ()
  | Some test ->
      launch_test test;
      launch_tests ()

let enable_only_on_unix f arg =
  if Sys.os_type = "Unix" then
    f arg
  else
    Disabled "only on Unix"

let show_environment_variable envvar =
  try
    Printf.eprintf "%s=%s\n" envvar (Sys.getenv envvar)
  with Not_found ->
    Printf.eprintf "%s not set\n" envvar

let main () =
  let library_name, version, minor =
    match Sys.argv with
      [| _ |] -> None, None, None
    | [| _; version |] ->
       begin
         match  String.length version with
           1 -> None, Some (int_of_string version), None
         | (3 | 4) when version.[1] = '.' ->
             None,
             Some (int_of_string (String.sub version 0 1)),
             Some
               (int_of_string (String.sub version 2 (String.length version - 2)))
         | _ -> Some version, None, None
       end
    | _ -> failwith "Argument should be a version number" in
  use_version := (version, minor);
  prerr_endline "Environment variables:";
  show_environment_variable "PATH";
  show_environment_variable "PYTHONHOME";
  show_environment_variable "DYLD_LIBRARY_PATH";
  show_environment_variable "DYLD_FALLBACK_LIBRARY_PATH";
  prerr_endline "Initializing library...";
  Py.initialize ?library_name ~verbose:true ?version ?minor ~debug_build:true ();
  begin
    match Py.get_library_filename () with
      None -> prerr_endline "No library has been loaded.\n"
    | Some filename -> Printf.eprintf "Library \"%s\" has been loaded.\n" filename
  end;
  Format.eprintf "platform: %s@." (Pywrappers.py_getplatform ());
  Format.eprintf "build info: %s@." (Pywrappers.py_getbuildinfo ());
  if Py.is_debug_build () then
    prerr_endline "Debug build."
  else
    prerr_endline "Not a debug build.";
  prerr_endline "Starting tests...";
  launch_tests ();
  if !failed then
    exit 1
