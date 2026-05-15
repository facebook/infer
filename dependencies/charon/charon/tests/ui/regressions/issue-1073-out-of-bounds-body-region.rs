//@ charon-args=--include std::io::_::advance_slices --include core::slice::_::default
use std::io::IoSlice;

pub fn foo(bufs: &mut &mut [IoSlice<'_>]) {
    IoSlice::advance_slices(bufs, 0);
}
