//@ charon-args=--include=std::io::Write::write_all
use std::io::Write;
fn main() {
    let mut buf: Vec<u8> = Vec::new();
    buf.write_all(b"hello").unwrap();
}
