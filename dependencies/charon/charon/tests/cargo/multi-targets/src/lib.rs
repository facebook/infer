//! This package should be compiled on both host target and
//! no-std target via cross-compilation to test
//! `charon cargo` supports `-- --target` correctly.
#![no_std]

#[cfg(target_os = "none")]
pub fn no_os(left: u64, right: u64) -> u64 {
    left + right
}

#[cfg(target_os = "windows")]
pub fn on_windows(left: u64, right: u64) -> u64 {
    left + right
}

#[cfg(target_family = "unix")]
pub fn on_unix(left: u64, right: u64) -> u64 {
    left + right
}
