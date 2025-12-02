//! Exercise the translation of arrays, features not yet supported by Eurydice

pub fn index_array_generic<const N: usize>(s: [u32; N], i: usize) -> u32 {
    s[i]
}

pub fn index_array_generic_call<const N: usize>(s: [u32; N], i: usize) -> u32 {
    index_array_generic(s, i)
}

// Using const generics as values
pub fn const_gen_ret<const N: usize>() -> usize {
    N
}

// Comes from https://github.com/AeneasVerif/charon/issues/45
// We initialize an array with a variable length (this uses the `repeat` instruction).
pub fn init_array_variable_len<const LEN: usize>() -> [u8; LEN] {
    [0u8; LEN]
}
