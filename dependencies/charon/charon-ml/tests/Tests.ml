open Charon
open Logging
module EL = Easy_logging.Logging

(* Set the log level - we use the environment variable "CHARON_LOG" *)
let log = main_log

let () =
  try
    let _ = Unix.getenv "CHARON_LOG" in
    log#set_level EL.Debug
  with Not_found -> log#set_level EL.Info

(* Call the tests *)
(* llbc files are copied into the `_build` dir by the `(deps)` rule in `./dune`. *)
let () = Test_Deserialize.run_tests "test-outputs"
let () = Test_NameMatcher.run_tests "test-outputs/ml-name-matcher-tests.llbc"
