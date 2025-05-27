#![feature(rustc_private)]
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate rustc_middle;

#[macro_use]
extern crate rustc_smir;
extern crate stable_mir;
use std::{env, ops::ControlFlow};

mod textual;
use textual::mir_to_textual;

fn main() {
    let args: Vec<String> = env::args().collect();
    let analyze_code = || -> ControlFlow<&str, &str> {
        mir_to_textual(stable_mir::all_local_items(), &mut std::io::stdout());
        ControlFlow::Break("")
    };
    let result = run!(&args, analyze_code);
    match result {
        Ok(ok) => println!("// Ok:{}", ok),
        Err(err) => println!("// Error : {}", err)
    }
}
