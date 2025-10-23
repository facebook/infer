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
  let json_file = IFilename.temp_file "charon" ".ullbc" in
  let rust_file = IFilename.temp_file "dummy" ".rs" in
  Out_channel.write_all rust_file ~data:source ;
  let cmd_out = run_charon json_file rust_file in
  try
    let json = Yojson.Basic.from_file json_file in
    match Charon.UllbcOfJson.crate_of_json json with
    | Ok _crate ->
        F.printf "Not yet implemented"
    | Error err ->
        F.printf "Test failed: %s" err
  with e -> F.printf "Exn %s\n Command output: %s" (Exn.to_string e) cmd_out

(* An example test *)
(* let%expect_test "example" =
  let source = 
    {|
#[allow(unused)]
fn foo() {
    let x = 42;
}

fn main(){
    foo();
}
|} 
  in
  test source ;
  [%expect
    {|
  .source_language = "Rust"

  define example::foo() : void {
    local var_0: void, x_1: int
    #node_0:
        store &x_1 <- 42:int
        store &var_0 <- null:void
        store &var_0 <- null:void
        n0:void = load &var_0
        ret n0

  }

  define example::main() : void {
    local var_0: void, var_1: void
    #node_0:
        n0 = example::foo()
        store &var_1 <- n0:void
        jmp node_1

    #node_1:
        store &var_0 <- null:void
        store &var_0 <- null:void
        n1:void = load &var_0
        ret n1

    #node_2:
        unreachable

  }
|}] *)
