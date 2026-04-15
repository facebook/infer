//@ charon-args=--precise-drops
//@ charon-args=--monomorphize
//@ charon-args=--include=core::marker::Destruct
fn drop_array(_: [String; 4]) {}

fn drop_slice(_: Box<[String]>) {}

fn drop_tuple(_: (String, String)) {}
