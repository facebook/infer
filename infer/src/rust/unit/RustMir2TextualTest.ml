(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let run_charon json_file rust_file =
  let redirected_cmd =
    F.sprintf "charon rustc --ullbc --dest-file %s -- %s --crate-name \"dummy\"" json_file rust_file
  in
  let {IUnix.Process_info.stdin; stderr} =
    IUnix.create_process ~prog:"sh" ~args:["-c"; redirected_cmd]
  in
  Unix.close stdin ;
  In_channel.input_lines (Unix.in_channel_of_descr stderr) |> String.concat ~sep:"\n"


let test source =
  let json_filename = IFilename.temp_file "charon" ".ullbc" in
  let rust_file = IFilename.temp_file "dummy" ".rs" in
  Out_channel.write_all rust_file ~data:source ;
  let cmd_out = run_charon json_filename rust_file in
  try
    let json = Yojson.Basic.from_file json_filename in
    match Charon.UllbcOfJson.crate_of_json json with
    | Error err ->
        F.printf "Test failed: %s" err
    | Ok crate -> (
        let textual = RustFrontend.RustMir2Textual.mk_module crate ~json_filename in
        match TextualVerification.verify_strict textual with
        | Error err ->
            F.printf "Test failed: %a" (F.pp_print_list TextualVerification.pp_error) err
        | Ok verified ->
            let transformed_textual, _ = TextualTransform.run Rust verified in
            Textual.Module.pp F.std_formatter transformed_textual )
  with e -> F.printf "Exn %s\n Command output: %s" (Exn.to_string e) cmd_out


(* An example test *)
let%expect_test "example" =
  let source = {|
#[allow(unused)]
fn main() {
    let x = 42;
}
|} in
  test source ;
  [%expect
    {|
  .source_language = "Rust"

  define dummy::main() : void {
    local var_0: void, x_1: int
    #node_0:
        store &x_1 <- 42:int
        store &var_0 <- null:void
        store &var_0 <- null:void
        n0:void = load &var_0
        ret n0

  }
|}]
