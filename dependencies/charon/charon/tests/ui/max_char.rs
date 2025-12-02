//@ charon-args=--include std::char::MAX
//@ charon-args=--include core::char::MAX
//@ charon-args=--include core::char::methods::_::MAX

fn main() {
    let _max_char = std::char::MAX;
}
